MODULE StdLinks;
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

    IMPORT Kernel, Services,
        Stores, Ports, Fonts,  Models, Views, Controllers, Properties, Dialog, Containers,
        TextModels, TextMappers, TextViews, TextControllers, TextSetters, TextRulers,
        Strings, StdCmds;

    CONST
        kind* = 0; cmd* = 1; close* = 2;    (* constants for Prop.valid *)
        always* = 0; ifShiftDown* = 1; never* = 2;    (* constants for close attrubute *)
        minLinkVersion = 0; maxLinkVersion = 1;
        minTargVersion = 0; maxTargVersion = 0;

    TYPE
        Directory* = POINTER TO ABSTRACT RECORD END;

        Link* = POINTER TO RECORD (Views.View)
            leftSide-: BOOLEAN;
            cmd: POINTER TO ARRAY OF CHAR;
            close: INTEGER
        END;

        Target* = POINTER TO RECORD (Views.View)
            leftSide-: BOOLEAN;
            ident: POINTER TO ARRAY OF CHAR
        END;

        Prop* = POINTER TO RECORD (Properties.Property)
            cmd*: POINTER TO ARRAY OF CHAR;
            link-: BOOLEAN;
            close*: INTEGER
        END;
        
        ChangeAttrOp = POINTER TO RECORD (Stores.Operation)
            v: Views.View;
            cmd: POINTER TO ARRAY OF CHAR;
            close: INTEGER;
            valid: SET
        END;
        
        StdDirectory = POINTER TO RECORD (Directory) END;

        TrapCleaner = POINTER TO RECORD (Kernel.TrapCleaner) END;

    VAR
        dir-, stdDir-: Directory;
        par-: Link;
        iconFont: Fonts.Typeface;
        linkLeft, linkRight, targetLeft, targetRight: ARRAY 8 OF SHORTCHAR;
        coloredBackg: BOOLEAN;
        
        cleaner: TrapCleaner;

        dialog*: RECORD
            cmd*: ARRAY 512 OF CHAR;
            type-: ARRAY 32 OF CHAR;
            close*: Dialog.List;
            known, valid: SET;
        END;
        fingerprint: INTEGER;

    (** Cleaner **)

    PROCEDURE (c: TrapCleaner) Cleanup;
    BEGIN
        par := NIL
    END Cleanup;

    (** Properties **)

    PROCEDURE (p: Prop) IntersectWith* (q: Properties.Property; OUT equal: BOOLEAN);
        VAR valid: SET;
    BEGIN
        WITH q: Prop DO
            valid := p.valid * q.valid; equal := TRUE;
            IF (cmd IN valid) & (p.cmd^ # q.cmd^) THEN EXCL(valid, cmd) END;
            IF (kind IN valid) & (p.link # q.link) THEN EXCL(valid, kind) END;
            IF (close IN valid) & (p.close # q.close) THEN EXCL (valid, close) END;
            IF p.valid # valid THEN p.valid := valid; equal := FALSE END
        END
    END IntersectWith;
        
    PROCEDURE (op: ChangeAttrOp) Do;
        VAR v: Views.View; s: POINTER TO ARRAY OF CHAR; c: INTEGER;
    BEGIN
        v := op.v; 
        WITH 
        | v: Link DO 
            IF cmd IN op.valid THEN s := op.cmd; op.cmd := v.cmd; v.cmd := s END;
            IF close IN op.valid THEN c := op.close; op.close := v.close; v.close := c END
        | v: Target DO 
            IF cmd IN op.valid THEN s := op.cmd; op.cmd := v.ident; v.ident := s END
        END
    END Do;

    PROCEDURE DoChangeAttrOp (v: Views.View; s: POINTER TO ARRAY OF CHAR; c: INTEGER; valid: SET);
        VAR op: ChangeAttrOp;
    BEGIN
        NEW(op); op.v := v; op.valid := valid;
        IF close IN valid THEN 
        op.close := c END;
        IF cmd IN valid THEN NEW(op.cmd, LEN(s)+1); op.cmd^ := s$ END;
        Views.Do(v,   "#Std:LinkChange", op)
    END DoChangeAttrOp;
    
    PROCEDURE SetProp(v: Views.View; msg: Properties.SetMsg);
        VAR p: Properties.Property;
     BEGIN
        p := msg.prop;
        WHILE p # NIL DO
            WITH p: Prop DO
                IF (cmd IN p.valid) OR (close IN p.valid) THEN DoChangeAttrOp(v, p.cmd, p.close, p.valid) END
            ELSE
            END;
            p := p.next
        END
    END SetProp;
    
    PROCEDURE PollProp(v: Views.View; VAR msg: Properties.PollMsg);
        VAR p: Prop;
    BEGIN
        NEW(p);    
        WITH v: Link DO
            p.known := {kind, cmd, close}; 
            p.link := TRUE;
            p.cmd := v.cmd;
            p.close := v.close
        | v: Target DO
            p.known := {kind, cmd}; 
            p.link := FALSE;
            p.cmd := v.ident
        ELSE
        END;
        p.valid := p.known;
        Properties.Insert(msg.prop, p)
    END PollProp;
    
    PROCEDURE InitDialog*;
        VAR  p: Properties.Property;
    BEGIN
        dialog.cmd := ""; dialog.type := ""; dialog.close.index := -1;
        dialog.known := {}; dialog.valid := {};
        Properties.CollectProp(p);
        WHILE p # NIL DO
            WITH p: Prop DO
                dialog.valid := p.valid; dialog.known := p.known;
                IF cmd IN p.valid THEN
                    dialog.cmd := p.cmd$
                END;
                IF kind IN p.valid THEN 
                    IF p.link THEN Dialog.MapString("#Std:Link", dialog.type)
                    ELSE Dialog.MapString("#Std:Target", dialog.type)
                    END
                END;
                IF close IN p.valid THEN
                    dialog.close.index := p.close
                END
            ELSE
            END;
            p := p.next
        END;
        Dialog.Update(dialog)
    END InitDialog;
    
    PROCEDURE Set*;
        VAR p: Prop;
    BEGIN
        NEW(p);
        p.valid := dialog.valid;
        IF cmd IN p.valid THEN
            NEW(p.cmd, LEN(dialog.cmd) + 1);
            p.cmd^ := dialog.cmd$
        END;
        p.close := dialog.close.index;
        Properties.EmitProp(NIL, p);
        fingerprint := 0    (* force actualization of fields *)
    END Set;
    
    PROCEDURE CmdGuard* (VAR par: Dialog.Par);
        VAR c: Containers.Controller; v: Views.View; fp: INTEGER;
    BEGIN
        IF ~(cmd IN dialog.known) THEN par.disabled := TRUE
        ELSIF ~(cmd IN dialog.valid) THEN par.undef := TRUE
        END;
        Controllers.SetCurrentPath(Controllers.targetPath);
        fp := 0;
        c := Containers.Focus();
        IF c # NIL THEN
            c.GetFirstView(Containers.selection, v);
            WHILE v # NIL DO fp := fp + Services.AdrOf(v); c.GetNextView(TRUE, v) END
        END;
        IF fp # fingerprint THEN fingerprint := fp; InitDialog END;
        Controllers.ResetCurrentPath()
    END CmdGuard;
    
    PROCEDURE CloseGuard* (VAR par: Dialog.Par);
    BEGIN
        IF ~(close IN dialog.known) THEN par.disabled := TRUE
        ELSIF ~(close IN dialog.valid) THEN par.undef := TRUE
        END;
    END CloseGuard;
    
    PROCEDURE Notifier* (idx, op, from, to: INTEGER);
    BEGIN
        IF op = Dialog.changed THEN INCL(dialog.valid, idx) END
    END Notifier;

    PROCEDURE (d: Directory) NewLink* (IN cmd: ARRAY OF CHAR): Link, NEW, ABSTRACT;
    PROCEDURE (d: Directory) NewTarget* (IN ident: ARRAY OF CHAR): Target, NEW, ABSTRACT;


    PROCEDURE InFrame (f: Views.Frame; x, y: INTEGER): BOOLEAN;
    BEGIN
        RETURN (f.l <= x) & (x < f.r) & (f.t <= y) & (y < f.b)
    END InFrame;

    PROCEDURE Mark (f: Views.Frame; show: BOOLEAN);
    BEGIN
        f.MarkRect(f.l, f.t, f.r, f.b, Ports.fill, Ports.hilite, show)
    END Mark;

    PROCEDURE ThisPos (v: TextViews.View; f: Views.Frame; x, y: INTEGER): INTEGER;
        (* "corrected" v.ThisPos: does not adjust position when crossing 50% boundary of characters *)
        VAR loc: TextViews.Location; pos: INTEGER;
    BEGIN
        pos := v.ThisPos(f, x, y); v.GetThisLocation(f, pos, loc);
        IF (loc.y <= y) & (y < loc.y + loc.asc + loc.dsc) & (x < loc.x) THEN DEC(pos) END;
        RETURN pos
    END ThisPos;

    PROCEDURE GetLinkPair (this: Link; VAR l, r: Link);
        (* POST: BalancedPair(l, r) & (l # r) & (l = this OR r = this) OR  (l = r = NIL) *)
        VAR t: TextModels.Model; rd: TextModels.Reader; v: Views.View; level: INTEGER;
    BEGIN
        l := NIL; r := NIL; level := 1;
        IF (this.context # NIL) & (this.context IS TextModels.Context) THEN
            t := this.context(TextModels.Context).ThisModel();
            rd := t.NewReader(NIL);
            IF this.leftSide THEN
                rd.SetPos(this.context(TextModels.Context).Pos() + 1);
                REPEAT
                    rd.ReadView(v);
                    IF (v # NIL) & (v IS Link) THEN
                        IF v(Link).leftSide THEN INC(level) ELSE DEC(level) END
                    END
                UNTIL (v = NIL) OR (level = 0);
                IF v # NIL THEN l := this; r := v(Link) END
            ELSE
                rd.SetPos(this.context(TextModels.Context).Pos());
                REPEAT
                    rd.ReadPrevView(v);
                    IF (v # NIL) & (v IS Link) THEN
                        IF v(Link).leftSide THEN DEC(level) ELSE INC(level) END
                    END
                UNTIL (v = NIL) OR (level = 0);
                IF v # NIL THEN l := v(Link); r := this END
            END
        END
    END GetLinkPair;

    PROCEDURE GetTargetPair (this: Target; VAR l, r: Target);
        (* POST: BalancedPair(l, r) & (l # r) & (l = this OR r = this) OR  (l = r = NIL) *)
        VAR t: TextModels.Model; rd: TextModels.Reader; v: Views.View; level: INTEGER;
    BEGIN
        l := NIL; r := NIL; level := 1;
        IF (this.context # NIL) & (this.context IS TextModels.Context) THEN
            t := this.context(TextModels.Context).ThisModel();
            rd := t.NewReader(NIL);
            IF this.leftSide THEN
                rd.SetPos(this.context(TextModels.Context).Pos() + 1);
                REPEAT
                    rd.ReadView(v);
                    IF (v # NIL) & (v IS Target) THEN
                        IF v(Target).leftSide THEN INC(level) ELSE DEC(level) END
                    END
                UNTIL (v = NIL) OR (level = 0);
                IF v # NIL THEN l := this; r := v(Target) END
            ELSE
                rd.SetPos(this.context(TextModels.Context).Pos());
                REPEAT
                    rd.ReadPrevView(v);
                    IF (v # NIL) & (v IS Target) THEN
                        IF v(Target).leftSide THEN DEC(level) ELSE INC(level) END
                    END
                UNTIL (v = NIL) OR (level = 0);
                IF v # NIL THEN l := v(Target); r := this END
            END
        END
    END GetTargetPair;

    PROCEDURE GetRange (l, r: Link; VAR beg, end: INTEGER);
    BEGIN
        beg := l.context(TextModels.Context).Pos();
        end := r.context(TextModels.Context).Pos() + 1
    END GetRange;

    PROCEDURE MarkRange (v: TextViews.View; f: Views.Frame; beg, end: INTEGER; show: BOOLEAN);
        VAR b, e: TextViews.Location; r, t: INTEGER;
    BEGIN
        ASSERT(beg < end, 20);
        v.GetThisLocation(f, beg, b); v.GetThisLocation(f, end, e);
        IF (b.pos < e.pos) OR (b.pos = e.pos) & (b.x < e.x) THEN
            IF b.start # e.start THEN
                r := f.r; t := b.y + b.asc + b.dsc;
                f.MarkRect(b.x, b.y, r, t, Ports.fill, Ports.hilite, show);
                IF t < e.y THEN f.MarkRect(0, t, r, e.y, Ports.fill, Ports.hilite, show) END;
                b.x := f.l; b.y := e.y
            END;
            f.MarkRect(b.x, b.y, e.x, e.y + e.asc + e.dsc, Ports.fill, Ports.hilite, show)
        END
    END MarkRange;

    PROCEDURE Reveal (left, right: Views.View; str: ARRAY OF CHAR; opname: Stores.OpName);
        VAR con: TextModels.Context; t: TextModels.Model; pos: INTEGER;
            w: TextMappers.Formatter; op: Stores.Operation;
    BEGIN
        con := left.context(TextModels.Context);
        t := con.ThisModel(); pos := con.Pos();
        w.ConnectTo(t); w.SetPos(pos);
        IF con.Attr() # NIL THEN w.rider.SetAttr(con.Attr()) END;
        Models.BeginScript(t, opname, op);
        t.Delete(pos, pos + 1);
        w.WriteChar("<");
        IF str # "" THEN w.WriteString(str) END;
        w.WriteChar(">");
        con := right.context(TextModels.Context);
        pos := con.Pos();
        w.SetPos(pos);
        IF con.Attr() # NIL THEN w.rider.SetAttr(con.Attr()) END;
        t.Delete(pos, pos + 1);
        w.WriteString("<>");
        Models.EndScript(t, op)
    END Reveal;
    
    PROCEDURE RevealCmd (v: Link);
        VAR left, right: Link;
    BEGIN GetLinkPair(v, left, right);
        IF left # NIL THEN
            IF v.cmd # NIL THEN Reveal(left, right, v.cmd^, "#StdLinks:Reveal Link Command") 
            ELSE Reveal(left, right, "", "#StdLinks:Reveal Link Command") 
            END
        END
    END RevealCmd;

    PROCEDURE RevealTarget (targ: Target);
        VAR  left, right: Target;
    BEGIN GetTargetPair(targ, left, right);
        IF left # NIL THEN
            IF left.ident # NIL THEN Reveal(left, right, left.ident^, "#SdtLinks:Reveal Target Ident")
            ELSE Reveal(left, right, "", "#SdtLinks:Reveal Target Ident")
            END
        END
    END RevealTarget;
    
    PROCEDURE CallCmd (v: Link; close: BOOLEAN);
        VAR res: INTEGER;
    BEGIN
        Kernel.PushTrapCleaner(cleaner); 
        par := v;
        IF v.cmd^ # "" THEN 
            IF close & (v.close = ifShiftDown) OR (v.close = always) THEN
                StdCmds.CloseDialog
            END;
            Dialog.Call(v.cmd^, "#StdLinks:Link Call Failed", res) 
        END;
        par := NIL;
        Kernel.PopTrapCleaner(cleaner)
    END CallCmd;

    PROCEDURE TrackSingle (f: Views.Frame; VAR in: BOOLEAN);
        VAR x, y: INTEGER; modifiers: SET; in0, isDown: BOOLEAN;
    BEGIN
        in := FALSE;
        REPEAT
            f.Input(x, y, modifiers, isDown);
            in0 := in; in := InFrame(f, x, y);
            IF in # in0 THEN Mark(f, in) END
        UNTIL ~isDown;
        IF in THEN Mark(f, FALSE) END
    END TrackSingle;

    PROCEDURE TrackRange (v: TextViews.View; f: Views.Frame; l, r: Link; x, y: INTEGER;
                                                VAR in: BOOLEAN);
        VAR pos, beg, end: INTEGER; modifiers: SET; in0, isDown: BOOLEAN;
    BEGIN
        in := FALSE;
        GetRange(l, r, beg, end); pos := ThisPos(v, f, x, y);
        IF (beg <= pos) & (pos < end) THEN
            REPEAT
                f.Input(x, y, modifiers, isDown); pos := ThisPos(v, f, x, y);
                in0 := in; in := (beg <= pos) & (pos < end);
                IF in # in0 THEN MarkRange(v, f, beg, end, in) END
            UNTIL ~isDown;
            IF in THEN
                MarkRange(v, f, beg, end, FALSE)
            END
        END
    END TrackRange;

    PROCEDURE Track (v: Link; f: Views.Frame; c: TextControllers.Controller;
                                    x, y: INTEGER; modifiers: SET);
    (* PRE: (c # NIL) & (f.view.ThisModel() = v.context.ThisModel())  OR  (c = NIL) & (f.view = v) *)
        VAR l, r: Link; in: BOOLEAN;
    BEGIN
        GetLinkPair(v, l, r);
        IF l # NIL THEN
            IF c # NIL THEN TrackRange(c.view, f, l, r, x, y, in)
            ELSE TrackSingle(f, in)
            END;
            IF in THEN
                IF (Controllers.modify IN modifiers) & ((c = NIL) OR ~(Containers.noCaret IN c.opts)) THEN
                    RevealCmd(l)
                ELSE
                    CallCmd(l, Controllers.extend IN modifiers)
                END
            END
        END
    END Track;

    PROCEDURE TrackTarget (targ: Target; f: Views.Frame; modifiers: SET);
        VAR in: BOOLEAN;
    BEGIN
        TrackSingle(f, in);
        IF in & (Controllers.modify IN modifiers) THEN RevealTarget(targ) END
    END TrackTarget;

    PROCEDURE (v: Link) CopyFromSimpleView- (source: Views.View);
    BEGIN
        WITH source: Link DO
            ASSERT(source.leftSide = (source.cmd # NIL), 100);
            v.leftSide := source.leftSide;
            v.close := source.close;
            IF source.cmd # NIL THEN
                NEW(v.cmd, LEN(source.cmd^));
                v.cmd^ := source.cmd^$
            ELSE v.cmd := NIL
            END
        END
    END CopyFromSimpleView;

    PROCEDURE (t: Target) CopyFromSimpleView- (source: Views.View);
    BEGIN
        WITH source: Target DO
            ASSERT(source.leftSide = (source.ident # NIL), 100);
            t.leftSide := source.leftSide;
            IF source.ident # NIL THEN
                NEW(t.ident, LEN(source.ident^));
                t.ident^ := source.ident^$
            ELSE t.ident := NIL
            END
        END
    END CopyFromSimpleView;

    PROCEDURE (v: Link) Internalize- (VAR rd: Stores.Reader);
        VAR len: INTEGER; version: INTEGER; pos: INTEGER;
    BEGIN
        v.Internalize^(rd);
        IF rd.cancelled THEN RETURN END;
        rd.ReadVersion(minLinkVersion, maxLinkVersion, version);
        IF rd.cancelled THEN RETURN END;
        rd.ReadBool(v.leftSide);
        rd.ReadInt(len);
        IF len = 0 THEN v.cmd := NIL
        ELSE NEW(v.cmd, len); rd.ReadXString(v.cmd^)
        END;
        v.leftSide := v.cmd # NIL;
        IF v.leftSide THEN
            IF version = 1 THEN
                rd.ReadInt(v.close)
            ELSE
                Strings.Find(v.cmd, "StdLinks.ShowTarget", 0, pos);
                IF (pos # 0) THEN v.close := ifShiftDown
                ELSE v.close := never
                END
            END
        END
    END Internalize;

    PROCEDURE (v: Link) Externalize- (VAR wr: Stores.Writer);
        VAR pos, version: INTEGER;
    BEGIN
        v.Externalize^(wr);
        IF v.leftSide THEN
            Strings.Find(v.cmd, "StdLinks.ShowTarget", 0, pos);
            IF (pos = 0) & (v.close = never) OR (v.close = ifShiftDown) THEN version := 0
            ELSE version := 1
            END
        ELSE
            version := 0
        END;
        wr.WriteVersion(version);
        wr.WriteBool(v.cmd # NIL);
        IF v.cmd = NIL THEN wr.WriteInt(0)
        ELSE wr.WriteInt(LEN(v.cmd^)); wr.WriteXString(v.cmd^)
        END;
        IF version = 1 THEN wr.WriteInt(v.close) END
    END Externalize;

    PROCEDURE (t: Target) Internalize- (VAR rd: Stores.Reader);
        VAR len: INTEGER; version: INTEGER;
    BEGIN
        t.Internalize^(rd);
        IF rd.cancelled THEN RETURN END;
        rd.ReadVersion(minTargVersion, maxTargVersion, version);
        IF rd.cancelled THEN RETURN END;
        rd.ReadBool(t.leftSide);
        rd.ReadInt(len);
        IF len = 0 THEN t.ident := NIL
        ELSE NEW(t.ident, len); rd.ReadXString(t.ident^)
        END;
        t.leftSide := t.ident # NIL
    END Internalize;

    PROCEDURE (t: Target) Externalize- (VAR wr: Stores.Writer);
    BEGIN
        t.Externalize^(wr);
        wr.WriteVersion(maxTargVersion);
        wr.WriteBool(t.ident # NIL);
        IF t.ident = NIL THEN wr.WriteInt(0)
        ELSE wr.WriteInt(LEN(t.ident^)); wr.WriteXString(t.ident^)
        END
    END Externalize;

    PROCEDURE RestoreView (v: Views.View; f: Views.Frame; icon: ARRAY OF SHORTCHAR);
        VAR c: Models.Context; a: TextModels.Attributes; font: Fonts.Font; color: Ports.Color;
            asc, dsc, w: INTEGER;
    BEGIN
        c := v.context;
        IF (c # NIL) & (c IS TextModels.Context) THEN
            a := c(TextModels.Context).Attr();
            font := Fonts.dir.This(iconFont, a.font.size, {}, Fonts.normal);
            color := a.color
        ELSE font := Fonts.dir.Default(); color := Ports.black
        END;
        IF coloredBackg THEN
        f.DrawRect(f.l, f.t, f.r, f.b, Ports.fill, Ports.grey25) END;
        font.GetBounds(asc, dsc, w);
        f.DrawSString(1*Ports.mm DIV 2, asc, color, icon, font)
    END RestoreView;

    PROCEDURE (v: Link) Restore* (f: Views.Frame; l, t, r, b: INTEGER);
    BEGIN
        IF v.leftSide THEN RestoreView(v, f, linkLeft)
        ELSE RestoreView(v, f, linkRight)
        END
    END Restore;

    PROCEDURE (targ: Target) Restore* (f: Views.Frame; l, t, r, b: INTEGER);
    BEGIN
        IF targ.leftSide THEN RestoreView(targ, f, targetLeft)
        ELSE RestoreView(targ, f, targetRight)
        END
    END Restore;

    PROCEDURE SizePref (v: Views.View; icon: ARRAY OF SHORTCHAR; VAR msg: Properties.SizePref);
        VAR c: Models.Context; a: TextModels.Attributes; font: Fonts.Font;
            asc, dsc, w: INTEGER;
    BEGIN
        c := v.context;
        IF (c # NIL) & (c IS TextModels.Context) THEN
            a := c(TextModels.Context).Attr();
            font := Fonts.dir.This(iconFont, a.font.size, {}, Fonts.normal)
        ELSE
            font := Fonts.dir.Default()
        END;
        msg.w := font.SStringWidth(icon) + 1*Ports.mm;
        font.GetBounds(asc, dsc, w);
        msg.h := asc + dsc
    END SizePref;
    
    PROCEDURE (v: Link) HandlePropMsg- (VAR msg: Properties.Message);
        VAR a: TextModels.Attributes; c: Models.Context; asc, dsc, w: INTEGER; l, r: Link;
    BEGIN
        WITH msg: Properties.SizePref DO
            IF v.leftSide THEN SizePref(v, linkLeft, msg)
            ELSE SizePref(v, linkRight, msg)
            END
        | msg: Properties.FocusPref DO
            msg.hotFocus := TRUE
        | msg: Properties.ResizePref DO
            msg.fixed := TRUE
        | msg: TextModels.Pref DO
            msg.opts := {TextModels.hideable}
        | msg: TextControllers.FilterPref DO
            msg.filter := TRUE
        | msg: TextSetters.Pref DO c := v.context;
            IF (c # NIL) & (c IS TextModels.Context) THEN
                a := c(TextModels.Context).Attr();
                a.font.GetBounds(asc, dsc, w);
                msg.dsc := dsc
            END
        | msg: Properties.PollMsg DO
            IF v.leftSide THEN PollProp(v, msg) 
            ELSE
                GetLinkPair(v, l, r);
                IF l # NIL THEN PollProp(l, msg) END
            END
        | msg: Properties.SetMsg DO
            IF v.leftSide THEN SetProp(v, msg)
            ELSE GetLinkPair(v, l, r); SetProp(l, msg)
            END
        ELSE
        END
    END HandlePropMsg;
    
    PROCEDURE (targ: Target) HandlePropMsg- (VAR msg: Properties.Message);
        VAR a: TextModels.Attributes; c: Models.Context; asc, dsc, w: INTEGER;  l, r: Target;
    BEGIN
        WITH msg: Properties.SizePref DO
            IF targ.leftSide THEN SizePref(targ, targetLeft, msg)
            ELSE SizePref(targ, targetRight, msg)
            END
        | msg: Properties.FocusPref DO
            msg.hotFocus := TRUE
        | msg: Properties.ResizePref DO
            msg.fixed := TRUE
        | msg: TextModels.Pref DO
            msg.opts := {TextModels.hideable}
        | msg: TextSetters.Pref DO c := targ.context;
            IF (c # NIL) & (c IS TextModels.Context) THEN
                a := c(TextModels.Context).Attr();
                a.font.GetBounds(asc, dsc, w);
                msg.dsc := dsc
            END
        | msg: Properties.PollMsg DO
            IF targ.leftSide THEN PollProp(targ, msg)
            ELSE
                GetTargetPair(targ, l, r);
                IF l # NIL THEN PollProp(l, msg) END
            END
        | msg: Properties.SetMsg DO
            IF targ.leftSide THEN SetProp(targ, msg)
            ELSE GetTargetPair(targ, l, r); SetProp(l, msg)
            END
        ELSE
        END
    END HandlePropMsg;

    PROCEDURE (v: Link) HandleCtrlMsg* (f: Views.Frame;
        VAR msg: Controllers.Message; VAR focus: Views.View);

        PROCEDURE isHot(c: TextControllers.Controller; x, y: INTEGER; mod: SET): BOOLEAN;
            VAR pos, beg, end: INTEGER;
        BEGIN
            (* ignore alt, cmd, and middle clicks in edit mode *)
            IF ~(Containers.noCaret IN c.opts) & (mod * {17, 27, 28} # {}) THEN RETURN FALSE END;
            pos := ThisPos(c.view, f, x, y);
            (* ignore clicks in selection *)
            c.GetSelection(beg, end);
            IF (end > beg) & (pos >= beg) & (pos <= end) THEN RETURN FALSE END;
            IF v.leftSide THEN RETURN pos >= v.context(TextModels.Context).Pos()
            ELSE RETURN pos < v.context(TextModels.Context).Pos()
            END
        END isHot;
        
    BEGIN
        WITH msg: Controllers.PollCursorMsg DO
            msg.cursor := Ports.refCursor
        | msg: TextControllers.FilterPollCursorMsg DO
            IF isHot(msg.controller, msg.x, msg.y, {}) THEN
                msg.cursor := Ports.refCursor; msg.done := TRUE
            END
        | msg: Controllers.TrackMsg DO
            Track(v, f, NIL, msg.x, msg.y, msg.modifiers)
        | msg: TextControllers.FilterTrackMsg DO
            IF isHot(msg.controller, msg.x, msg.y, msg.modifiers) THEN
                Track(v, f,  msg.controller, msg.x, msg.y, msg.modifiers);
                msg.done := TRUE
            END
        ELSE
        END
    END HandleCtrlMsg;

    PROCEDURE (targ: Target) HandleCtrlMsg* (f: Views.Frame; VAR msg: Controllers.Message;
                                                                        VAR focus: Views.View);
    BEGIN
        WITH msg: Controllers.TrackMsg DO TrackTarget(targ, f, msg.modifiers)
        ELSE
        END
    END HandleCtrlMsg;

    PROCEDURE (v: Link) GetCmd* (OUT cmd: ARRAY OF CHAR), NEW;
    BEGIN
        ASSERT(v.leftSide, 20);
        ASSERT(v.cmd # NIL, 100);
        cmd := v.cmd$
    END GetCmd;

    PROCEDURE (t: Target) GetIdent* (OUT ident: ARRAY OF CHAR), NEW;
    BEGIN
        ASSERT(t.leftSide, 20);
        ASSERT(t.ident # NIL, 100);
        ident := t.ident$
    END GetIdent;

    (* --------------- create commands and menu guards ------------------------ *)

    PROCEDURE GetParam (c: TextControllers.Controller; VAR param: ARRAY OF CHAR;
                                    VAR lbrBeg, lbrEnd, rbrBeg, rbrEnd: INTEGER);
        VAR rd: TextModels.Reader; i, beg, end: INTEGER;
            ch0, ch1, ch2: CHAR;
    BEGIN
        param[0] := 0X;
        IF (c # NIL) & c.HasSelection() THEN
            c.GetSelection(beg, end);
            IF end - beg > 4 THEN
                rd := c.text.NewReader(NIL);
                rd.SetPos(beg); rd.ReadChar(ch0);
                rd.SetPos(end-2); rd.ReadChar(ch1); rd.ReadChar(ch2);
                IF (ch0 = "<") & (ch1 = "<") & (ch2 = ">") THEN
                    rd.SetPos(beg+1); rd.ReadChar(ch0); i := 0;
                    WHILE ~rd.eot & (ch0 # ">") DO
                        IF i < LEN(param) - 1 THEN param[i] := ch0; INC(i) END;
                        rd.ReadChar(ch0)
                    END;
                    param[i] := 0X;
                    lbrBeg := beg; lbrEnd := rd.Pos();
                    rbrBeg := end -2; rbrEnd := end
                END
            END
        END
    END GetParam;
    
    PROCEDURE CreateGuard* (VAR par: Dialog.Par);
        VAR param: ARRAY 512 OF CHAR; lbrBeg, lbrEnd, rbrBeg, rbrEnd: INTEGER;
    BEGIN
        GetParam(TextControllers.Focus(), param, lbrBeg, lbrEnd, rbrBeg, rbrEnd);
        par.disabled := param = ""
    END CreateGuard;

    PROCEDURE InsertionAttr (c: TextControllers.Controller; pos: INTEGER): TextModels.Attributes;
        VAR rd: TextModels.Reader; r: TextRulers.Ruler; a: TextModels.Attributes; ch: CHAR;
    BEGIN
        rd := c.text.NewReader(NIL);  a := NIL;
        rd.SetPos(pos); rd.ReadChar(ch); a := rd.attr;
        IF a = NIL THEN c.view.PollDefaults(r, a) END;
        RETURN a
    END InsertionAttr;

    PROCEDURE CreateLink*;
        VAR lbrBeg, lbrEnd, rbrBeg, rbrEnd: INTEGER;
            left, right: Link; c: TextControllers.Controller;
            cmd: ARRAY 512 OF CHAR;
            op: Stores.Operation;
            w: TextModels.Writer; a: TextModels.Attributes;
    BEGIN
        c := TextControllers.Focus();
        GetParam(TextControllers.Focus(), cmd, lbrBeg, lbrEnd, rbrBeg, rbrEnd);
        IF cmd # "" THEN
            w := c.text.NewWriter(NIL);
            Models.BeginScript(c.text, "#StdLinks:Create Link", op);
            a := InsertionAttr(c, rbrBeg);
            c.text.Delete(rbrBeg, rbrEnd);
            right := dir.NewLink("");
            w.SetPos(rbrBeg);
            IF a # NIL THEN w.SetAttr(a) END;
            w.WriteView(right, 0, 0);
            a := InsertionAttr(c, lbrBeg);
            c.text.Delete(lbrBeg, lbrEnd);
            left := dir.NewLink(cmd);
            w.SetPos(lbrBeg);
            IF a # NIL THEN w.SetAttr(a) END;
            w.WriteView(left, 0, 0);
            Models.EndScript(c.text, op)
        END
    END CreateLink;

    PROCEDURE CreateTarget*;
        VAR lbrBeg, lbrEnd, rbrBeg, rbrEnd: INTEGER;
            left, right: Target; c: TextControllers.Controller;
            ident: ARRAY 512 OF CHAR;
            op: Stores.Operation;
            w: TextModels.Writer; a: TextModels.Attributes;
    BEGIN
        c := TextControllers.Focus();
        GetParam(TextControllers.Focus(), ident, lbrBeg, lbrEnd, rbrBeg, rbrEnd);
        IF ident # "" THEN
            w := c.text.NewWriter(NIL);
            Models.BeginScript(c.text, "#StdLinks:Create Target", op);
            a := InsertionAttr(c, rbrBeg);
            c.text.Delete(rbrBeg, rbrEnd);
            right := dir.NewTarget("");
            w.SetPos(rbrBeg);
            IF a # NIL THEN w.SetAttr(a) END;
            w.WriteView(right, 0, 0);
            a := InsertionAttr(c, lbrBeg);
            c.text.Delete(lbrBeg, lbrEnd);
            left := dir.NewTarget(ident);
            w.SetPos(lbrBeg);
            IF a # NIL THEN w.SetAttr(a) END;
            w.WriteView(left, 0, 0);
            Models.EndScript(c.text, op)
        END
    END CreateTarget;

    PROCEDURE ShowTarget* (IN ident: ARRAY OF CHAR);
        VAR c: TextControllers.Controller; rd: TextModels.Reader;
            v: Views.View; left, right: Target; beg, end: INTEGER;
    BEGIN
        c := TextControllers.Focus();
        IF c # NIL THEN
            rd := c.text.NewReader(NIL);
            REPEAT rd.ReadView(v)
            UNTIL rd.eot OR (v # NIL) & (v IS Target) & v(Target).leftSide & (v(Target).ident^ = ident);
            IF ~rd.eot THEN
                GetTargetPair(v(Target), left, right);
                IF (left # NIL) & (right # NIL) THEN
                    beg := left.context(TextModels.Context).Pos();
                    end := right.context(TextModels.Context).Pos() + 1;
                    c.SetSelection(beg, end);
                    c.view.SetOrigin(beg, 0)
                ELSE
                    Dialog.ShowParamMsg("target '^0' not found", ident, "", "")
                END
            ELSE
                Dialog.ShowParamMsg("target '^0' not found", ident, "", "")
            END
        END
    END ShowTarget;


    (* programming interface *)

    PROCEDURE (d: StdDirectory) NewLink (IN cmd: ARRAY OF CHAR): Link;
        VAR link: Link; i: INTEGER;
    BEGIN
        NEW(link); link.leftSide := cmd # "";
        IF link.leftSide THEN
            i := 0; WHILE cmd[i] # 0X DO INC(i) END;
            NEW(link.cmd, i + 1); link.cmd^ := cmd$
        ELSE
            link.cmd := NIL
        END;
        link.close := ifShiftDown;
        RETURN link
    END NewLink;

    PROCEDURE (d: StdDirectory) NewTarget (IN ident: ARRAY OF CHAR): Target;
        VAR t: Target; i: INTEGER;
    BEGIN
        NEW(t); t.leftSide := ident # "";
        IF t.leftSide THEN
            i := 0; WHILE ident[i] # 0X DO INC(i) END;
            NEW(t.ident, i + 1); t.ident^ := ident$
        ELSE
            t.ident := NIL
        END;
        RETURN t
    END NewTarget;

    PROCEDURE SetDir* (d: Directory);
    BEGIN
        ASSERT(d # NIL, 20);
        dir := d
    END SetDir;

    PROCEDURE Init;
        VAR font: Fonts.Font; d: StdDirectory;

        PROCEDURE DefaultAppearance;
        BEGIN font := Fonts.dir.Default(); iconFont := font.typeface;
            linkLeft := "Link"; linkRight := "~";
            targetLeft := "Targ"; targetRight :=  "~";
            coloredBackg := TRUE
        END DefaultAppearance;

    BEGIN
        NEW(d); dir := d; stdDir := d;
        IF Dialog.platform DIV 10 = 1 THEN (* Windows *)
            iconFont := "Wingdings";
            font := Fonts.dir.This(iconFont, 10*Fonts.point (*arbitrary*), {}, Fonts.normal);
            IF font.IsAlien() THEN DefaultAppearance
            ELSE
                linkLeft[0] := SHORT(CHR(246)); linkLeft[1] := 0X;
                linkRight[0] := SHORT(CHR(245)); linkRight[1] := 0X;
                targetLeft[0] := SHORT(CHR(164)); targetLeft[1] := 0X;
                targetRight[0] := SHORT(CHR(161)); targetRight[1] := 0X;
                coloredBackg := FALSE
            END
        ELSIF Dialog.platform DIV 10 = 2 THEN (* Mac *)
            DefaultAppearance
        ELSE
            DefaultAppearance
        END;
        NEW(cleaner);
        dialog.close.SetResources("#Std:links")
    END Init;

BEGIN
    Init
END StdLinks.
