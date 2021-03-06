MODULE StdStamps;
(**
    project    = "BlackBox"
    organization    = "www.oberon.ch"
    contributors    = "Oberon microsystems"
    version    = "System/Rsrc/About"
    copyright    = "System/Rsrc/About"
    license    = "Docu/BB-License"
    changes    = ""
    issues    = ""

**)

(*
    StdStamps are used to keep track of document changes, in particular program texts.
    StdStamps carry a sequence number and a fingerprint of the document with them.
    Each time the document (and therefore its fingerprint) is changed and stored,
    the sequence number is incremented. (When determining the fingerprint of the
    document, whitespace is ignored, except in string literals.)
    
    Each StdStamp also keeps track of the history of most recent changes.
    For the last maxHistoryEntries sequence numbers, the date and time,
    and an optional one-line comment is stored. To avoid too many entries in the history
    while working on a module, the most recent history entry is overwritten upon the
    generation of a new sequence number if the current date is the same as the date in
    the history entry.

*)

    IMPORT
        SYSTEM, (* SYSTEM.ROT only, for fingerprint calculation *)
        Strings, Dates, StdCmds,
        Ports, Models, Stores, Containers, Properties, Views, Controllers, Fonts,
        TextModels, TextSetters, TextMappers, TextViews, TextRulers;

    CONST
        setCommentKey = "#Std:Set Comment";
        maxHistoryEntries = 25;
        minVersion = 0; origStampVersion = 0; thisVersion = 2;
        
    TYPE
        History = ARRAY maxHistoryEntries OF RECORD
            fprint, snr: INTEGER;    (* fingerprint, sequence number *)
            date: INTEGER;            (* days since 1/1/1 *)
            time: INTEGER;            (* min + 64 * hour *)
            comment: POINTER TO ARRAY OF CHAR;    (* nil if no comment  *)
        END;
            
        StdView = POINTER TO RECORD (Views.View)
            (*--snr: LONGINT;*)            
            nentries: INTEGER;    (* number of entries in history *)
            history: History;            (* newest entry in history[0] *)
            cache: ARRAY 64 OF CHAR;
        END;

        SetCmtOp = POINTER TO RECORD (Stores.Operation)
            stamp: StdView;
            oldcomment: POINTER TO ARRAY OF CHAR;
        END;

    VAR
        comment*: RECORD
            s*: ARRAY 64 OF CHAR;
        END;


    PROCEDURE (op: SetCmtOp) Do;
        VAR temp: POINTER TO ARRAY OF CHAR;
    BEGIN
        temp := op.stamp.history[0].comment;
        op.stamp.history[0].comment := op.oldcomment;
        op.oldcomment := temp;
    END Do;

    PROCEDURE Format (v: StdView);
        VAR s: ARRAY 64 OF CHAR; d: Dates.Date; t: INTEGER;
    BEGIN
        t := v.history[0].time;
        Dates.DayToDate(v.history[0].date, d);
        Dates.DateToString(d, Dates.plainAbbreviated, s); v.cache := s$;
        Strings.IntToStringForm(v.history[0].snr, Strings.decimal, 4, "0", FALSE, s);
        v.cache := v.cache + " (" + s + ")"
    END Format;


    PROCEDURE FontContext (v: StdView): Fonts.Font;
        VAR c: Models.Context;
    BEGIN
        c := v.context;
        IF (c # NIL) & (c IS TextModels.Context) THEN
            RETURN c(TextModels.Context).Attr().font;
        ELSE
            RETURN Fonts.dir.Default()
        END;
    END FontContext;

    PROCEDURE CalcFP (t: TextModels.Model): INTEGER;
        CONST sglQuote = "'"; dblQuote = '"';
        VAR fp: INTEGER;  rd: TextModels.Reader; ch, quoteChar: CHAR; 
    BEGIN
        quoteChar := 0X; fp := 0;
        rd := t.NewReader(NIL); rd.ReadChar(ch);
        WHILE ~rd.eot DO
            IF ch = quoteChar THEN quoteChar := 0X;
            ELSIF (quoteChar = 0X) & ((ch = dblQuote) OR (ch = sglQuote)) THEN quoteChar := ch;
            END;
            IF (quoteChar = 0X) & (21X <= ch) & (ch # 8BX) & (ch # 8FX) & (ch # 0A0X) (* not in string literal *)
                OR (quoteChar # 0X) & (20X <= ch) (* within string literal *)
            THEN
                fp := SYSTEM.ROT(fp, 1) + 13 * ORD(ch);
            END;
            rd.ReadChar(ch);
        END;
        RETURN fp;
    END CalcFP;

    PROCEDURE Update (v: StdView; forcenew: BOOLEAN);
        VAR fp: INTEGER; i: INTEGER; ndays: INTEGER; d: Dates.Date; t: Dates.Time;
    BEGIN
        IF (v.context # NIL) & (v.context IS TextModels.Context) THEN
            fp := CalcFP(v.context(TextModels.Context).ThisModel());
            IF (fp # v.history[0].fprint) OR forcenew THEN
                Dates.GetDate(d); Dates.GetTime(t);
                ndays := Dates.Day(d);
                IF (ndays # v.history[0].date) OR forcenew THEN
                    (* move down entries in history list *)
                    i := maxHistoryEntries-1;
                    WHILE i > 0 DO
                        v.history[i] := v.history[i-1];
                        DEC(i);
                    END;
                    v.history[0].comment := NIL;
                END;
                IF v.nentries < maxHistoryEntries THEN INC(v.nentries) END;
                INC(v.history[0].snr);
                v.history[0].fprint := fp;
                v.history[0].date := ndays;
                v.history[0].time := t.minute + t.hour*64;
                Format(v);
                Views.Update(v, Views.keepFrames);
            END;
        END;
    END Update;

    PROCEDURE (v: StdView) Externalize (VAR wr: Stores.Writer);
        VAR i, len: INTEGER;
    BEGIN
        Update(v, FALSE);
        v.Externalize^(wr);
        wr.WriteVersion(thisVersion);
        (*--wr.WriteLInt(v.snr);*)
        wr.WriteXInt(v.nentries);
        FOR i := 0 TO v.nentries-1 DO
            wr.WriteInt(v.history[i].fprint);
            wr.WriteInt(v.history[i].snr);
            wr.WriteInt(v.history[i].date);
            wr.WriteXInt(v.history[i].time);
            IF v.history[i].comment # NIL THEN
                len := LEN(v.history[i].comment$);
                wr.WriteXInt(len);
                wr.WriteXString(v.history[i].comment^);
            ELSE wr.WriteXInt(0);
            END
        END;
    END Externalize;

    PROCEDURE (v: StdView) Internalize (VAR rd: Stores.Reader);
        VAR version: INTEGER; format: BYTE; i, len: INTEGER;
            d: Dates.Date; t: Dates.Time;
    BEGIN
        v.Internalize^(rd);
        IF ~rd.cancelled THEN
            rd.ReadVersion(minVersion, thisVersion, version);
            IF ~rd.cancelled THEN
                IF version = origStampVersion THEN (* deal with old StdStamp format *)
                    (* would like to calculate fingerprint, but hosting model not available at this time *)
                    v.history[0].fprint := 0;
                    v.history[0].snr := 1; v.nentries := 1;
                    rd.ReadXInt(d.year); rd.ReadXInt(d.month); rd.ReadXInt(d.day);
                    rd.ReadXInt(t.hour); rd.ReadXInt(t.minute); rd.ReadXInt(t.second);
                    rd.ReadByte(format); (* format not used anymore *)
                    v.history[0].date := Dates.Day(d);
                    v.history[0].time := t.minute + t.hour*64;
                ELSE
                    IF version = 1 THEN rd.ReadInt(v.history[0].snr) END; (* red text: to be removed soon *)
                    rd.ReadXInt(v.nentries);
                    FOR i := 0 TO v.nentries-1 DO
                        rd.ReadInt(v.history[i].fprint);
                        IF version > 1 THEN rd.ReadInt(v.history[i].snr)
                        ELSIF (* (version = 1) & *) i > 0 THEN v.history[i].snr := v.history[i-1].snr - 1;
                        END; (* red text: to be removed soon *)
                        rd.ReadInt(v.history[i].date);
                        rd.ReadXInt(v.history[i].time);
                        rd.ReadXInt(len);
                        IF len > 0 THEN
                            NEW(v.history[i].comment, len + 1);
                            rd.ReadXString(v.history[i].comment^);
                        ELSE v.history[i].comment := NIL;
                        END
                    END;
                END;
                Format(v);
            END
        END
    END Internalize;

    PROCEDURE (v: StdView) CopyFromSimpleView (source: Views.View);
        VAR i: INTEGER;
    BEGIN
        (* v.CopyFrom^(source); *)
        WITH source: StdView DO
            (*--v.snr := source.snr;*)
            v.nentries := source.nentries;
            v.history := source.history;
            v.cache := source.cache;
            FOR i := 0 TO v.nentries - 1 DO
                IF source.history[i].comment # NIL THEN
                    NEW(v.history[i].comment, LEN(source.history[i].comment$) + 1);
                    v.history[i].comment^ := source.history[i].comment^$;
                END
            END
        END
    END CopyFromSimpleView;

    PROCEDURE (v: StdView) Restore (f: Views.Frame; l, t, r, b: INTEGER);
        VAR a: TextModels.Attributes; color: Ports.Color; c: Models.Context; font: Fonts.Font;
            asc, dsc, fw: INTEGER;
    BEGIN
        c := v.context;
        IF (c # NIL) & (c IS TextModels.Context) THEN
            a := v.context(TextModels.Context).Attr();
            font := a.font;
            color := a.color;
        ELSE font := Fonts.dir.Default(); color := Ports.black;
        END;
        font.GetBounds(asc, dsc, fw);
        f.DrawLine(f.l, asc + f.dot, f.r, asc + f.dot, 1, Ports.grey25 );
        f.DrawString(0, asc, color, v.cache, font);
    END Restore;

    PROCEDURE SizePref (v: StdView; VAR p: Properties.SizePref);
        VAR font: Fonts.Font; asc, dsc, w: INTEGER; d: Dates.Date; s: ARRAY 64 OF CHAR;
    BEGIN
        font := FontContext(v);
        font.GetBounds(asc, dsc, w);
        d.day := 28; d.month := 1; d.year := 2222; p.w := 0;
        WHILE d.month <= 12 DO
            Dates.DateToString(d, Dates.plainAbbreviated, s);
            s := s + " (0000)";
            w := font.StringWidth(s);
            IF w > p.w THEN p.w := w END;
            INC(d.month)
        END;
        p.h := asc + dsc;
    END SizePref;

    PROCEDURE (v: StdView) HandlePropMsg (VAR msg: Properties.Message);
        VAR font: Fonts.Font; asc, w: INTEGER;
    BEGIN
        WITH msg: Properties.Preference DO
            WITH msg: Properties.SizePref DO
                SizePref(v, msg)
            | msg: Properties.ResizePref DO
                msg.fixed := TRUE
            | msg: Properties.FocusPref DO
                msg.hotFocus := TRUE
            | msg: TextSetters.Pref DO
                font := FontContext(v);
                font.GetBounds(asc, msg.dsc, w);
            ELSE
            END
        ELSE
        END
    END HandlePropMsg;

    PROCEDURE NewRuler (): TextRulers.Ruler;
        CONST mm = Ports.mm;
        VAR r: TextRulers.Ruler;
    BEGIN
        r := TextRulers.dir.New(NIL);
        TextRulers.SetRight(r, 140 * mm);
        TextRulers.AddTab(r, 15 * mm); TextRulers.AddTab(r, 35 * mm); TextRulers.AddTab(r, 75 * mm);
        RETURN r
    END NewRuler;

    PROCEDURE ShowHistory (v: StdView);
        VAR text: TextModels.Model; f: TextMappers.Formatter;
            i: INTEGER; d: Dates.Date; s: ARRAY 64 OF CHAR;
            tv: TextViews.View; attr: TextModels.Attributes;
    BEGIN
        text := TextModels.dir.New();
        f.ConnectTo(text);
        attr := f.rider.attr;
        f.rider.SetAttr(TextModels.NewStyle(attr, {Fonts.italic}));
        f.WriteString("seq nr."); f.WriteTab;
        f.WriteString("fingerprint"); f.WriteTab;
        f.WriteString("date and time"); f.WriteTab;
        f.WriteString("comment"); f.WriteLn;
        f.rider.SetAttr(attr); f.WriteLn;
        (*--n := v.snr;*)
        FOR i := 0 TO v.nentries-1 DO
            f.WriteIntForm(v.history[i].snr, 10, 4, "0", FALSE);
            (*--DEC(n);*)
            f.WriteTab;
            f.WriteIntForm(v.history[i].fprint, TextMappers.hexadecimal, 8, "0", FALSE);
            f.WriteTab;
            Dates.DayToDate(v.history[i].date, d);
            Dates.DateToString(d, Dates.plainAbbreviated, s);
            f.WriteString(s);
            f.WriteString("  ");
            f.WriteIntForm(v.history[i].time DIV 64, 10, 2, "0", FALSE);
            f.WriteString(":");
            f.WriteIntForm(v.history[i].time MOD 64, 10, 2, "0", FALSE);
            IF v.history[i].comment # NIL THEN
                f.WriteTab;
                f.WriteString( v.history[i].comment^);
            END;
            f.WriteLn;
        END;
        tv := TextViews.dir.New(text);
        tv.SetDefaults(NewRuler(), TextViews.dir.defAttr);
        tv.ThisController().SetOpts({Containers.noFocus, Containers.noCaret});
        Views.OpenAux(tv, "History");
    END ShowHistory;

    PROCEDURE Track (v: StdView; f: Views.Frame; x, y: INTEGER; buttons: SET);
        VAR c: Models.Context; w, h: INTEGER; isDown, in, in0: BOOLEAN; m: SET;
    BEGIN
        c := v.context; c.GetSize(w, h); in0 := FALSE; in := TRUE;
        REPEAT
            IF in # in0 THEN
                f.MarkRect(0, 0, w, h, Ports.fill, Ports.invert, Ports.show); in0 := in
            END;
            f.Input(x, y, m, isDown);
            in := (0 <= x) & (x < w) & (0 <= y) & (y < h)
        UNTIL ~isDown;
        IF in0 THEN
            f.MarkRect(0, 0, w, h, Ports.fill, Ports.invert, Ports.hide);
            IF Controllers.modify IN m THEN
                IF v.history[0].comment # NIL THEN comment.s := v.history[0].comment^$;
                ELSE comment.s := "";
                END;
                StdCmds.OpenToolDialog("Std/Rsrc/Stamps", "Comment");
            ELSE ShowHistory(v);
            END
        END
    END Track;

    PROCEDURE (v: StdView) HandleCtrlMsg (
                f: Views.Frame; VAR msg: Controllers.Message; VAR focus: Views.View);
    BEGIN
        WITH msg: Controllers.TrackMsg DO
            Track(v, f, msg.x, msg.y, msg.modifiers)
        | msg: Controllers.PollCursorMsg DO
            msg.cursor := Ports.refCursor
        ELSE
        END
    END HandleCtrlMsg;


    (* ------------ programming interface: ---------------------- *)

    PROCEDURE GetFirstInText* (t: TextModels.Model): Views.View;
        VAR r: TextModels.Reader; v: Views.View;
    BEGIN
        IF t # NIL THEN
            r := t.NewReader(NIL);
            REPEAT r.ReadView(v) UNTIL (v = NIL) OR (v IS StdView);
            RETURN v;
        ELSE RETURN NIL;
        END;
    END GetFirstInText;

    PROCEDURE IsStamp* (v: Views.View): BOOLEAN;
    BEGIN
        RETURN v IS StdView;
    END IsStamp;

    PROCEDURE GetInfo* (v: Views.View; VAR snr, historylen: INTEGER);
    BEGIN
        ASSERT(v IS StdView, 20);
        WITH v: StdView DO
            snr := v.history[0].snr; historylen := v.nentries;
        END
    END GetInfo;

    PROCEDURE GetData* (v: Views.View; entryno: INTEGER;
                VAR fprint: INTEGER; VAR date: Dates.Date; VAR time: Dates.Time);
    BEGIN
        ASSERT(v IS StdView, 20);
        WITH v: StdView DO
            IF entryno <= v.nentries THEN
                fprint := v.history[entryno].fprint;
                Dates.DayToDate(v.history[entryno].date, date);
                time.minute := v.history[entryno].time MOD 64;
                time.minute := v.history[entryno].time DIV 64;
                time.second := 0;
            END
        END
    END GetData;

    (** Insert new history entry with comment in v. *)
    PROCEDURE Stamp* (v: Views.View; comment: ARRAY OF CHAR);
    BEGIN
        ASSERT(v IS StdView, 20);
        WITH v: StdView DO
            Update(v, TRUE);
            NEW(v.history[0].comment, LEN(comment$) + 1);
            v.history[0].comment^ := comment$;
        END
    END Stamp;

    PROCEDURE New* (): Views.View;
        VAR v: StdView; d: Dates.Date; t: Dates.Time;
    BEGIN
        NEW(v); v.history[0].snr := 0; v.nentries := 0;
        v.history[0].fprint := 0;
        Dates.GetDate(d); Dates.GetTime(t);
        v.history[0].date := Dates.Day(d);
        v.history[0].time := t.minute + t.hour*64;
        Format(v);
        RETURN v;
    END New;

    PROCEDURE SetComment*;
        VAR v: Views.View; op: SetCmtOp;
    BEGIN
        v := GetFirstInText(TextViews.FocusText());
        IF v # NIL THEN
            WITH v: StdView DO
                NEW(op); op.stamp := v;
                NEW(op.oldcomment, LEN(comment.s$) + 1);
                op.oldcomment^ := comment.s$;
                Views.Do(v, setCommentKey, op);
            END
        END
    END SetComment;

    PROCEDURE Deposit*;
    BEGIN
        Views.Deposit(New())
    END Deposit;

END StdStamps.
