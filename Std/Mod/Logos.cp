MODULE StdLogos;
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

    IMPORT Ports, Stores, Views, Controllers, Properties;

    CONST
        W = 4;
        baseSize = 24 * Ports.point;

        colBase = 00202020H;

        changeColorKey = "#System:ChangeColor";

        minVersion = 0; maxVersion = 0;


    TYPE
        View = POINTER TO RECORD (Views.View)
            c: Ports.Color
        END;
        
        ChangeSizeOp = POINTER TO RECORD (Stores.Operation)
            view: View;
            size: INTEGER;
        END;
    
        ChangeColorOp = POINTER TO RECORD (Stores.Operation)
            view: View;
            color: Ports.Color
        END;
        
    (* curve painting *)

    PROCEDURE Paint (f: Views.Frame; size: INTEGER; col, bgnd: Ports.Color);
        VAR i, d, s, g, m, a, b, l, l0, rl, rt, rr, rb: INTEGER; c: Ports.Color;
    BEGIN
        s := size DIV 10; d := size DIV 2; g := d DIV 8; m := size * W DIV 2;
        f.DrawOval(0, s * 2, size * W, size, Ports.fill, col);
        f.DrawOval(s * W, s * 11 DIV 4, (size - s) * W, size - s * 3 DIV 4, Ports.fill, bgnd);
        a := m; b := m + d; c := 7 * colBase; i := 0;
        WHILE i < 4 DO
            f.DrawOval(a, 0, b, d, Ports.fill, c);
            INC(a, g); DEC(b, g); DEC(c, colBase); INC(i)
        END;
        f.rider.GetRect(rl, rt, rr, rb);
        l0 := rl; l := (f.gx + m + d DIV 2) DIV f.unit;
        IF l < rr THEN
            f.rider.SetRect(l, rt, rr, rb);
            a := m; b := m + d; c := 0; i := 0;
            WHILE i < 4 DO
                f.DrawOval(a, 0, b, d, Ports.fill, c);
                INC(a, g); DEC(b, g); INC(c, colBase); INC(i)
            END;
            f.rider.SetRect(l0, rt, rr, rb)
        END
    END Paint;

    (* ChangeOp *)

    PROCEDURE (op: ChangeSizeOp) Do;
        VAR v: View; size, w: INTEGER;
    BEGIN
        v := op.view;
        size := op.size; v.context.GetSize(w, op.size); v.context.SetSize(size * W, size);
        Views.Update(v, Views.keepFrames)
    END Do;

    PROCEDURE (op: ChangeColorOp) Do;
        VAR v: View; color: Ports.Color;
    BEGIN
        v := op.view;
        color := op.color; op.color := v.c; v.c := color;
        Views.Update(v, Views.keepFrames)
    END Do;

    (* View *)

    PROCEDURE (v: View) Internalize (VAR rd: Stores.Reader);
        VAR thisVersion: INTEGER;
    BEGIN
        v.Internalize^(rd); IF rd.cancelled THEN RETURN END;
        rd.ReadVersion(minVersion, maxVersion, thisVersion); IF rd.cancelled THEN RETURN END;
        rd.ReadInt(v.c)
    END Internalize;

    PROCEDURE (v: View) Externalize (VAR wr: Stores.Writer);
    BEGIN
        v.Externalize^(wr);
        wr.WriteVersion(maxVersion);
        wr.WriteInt(v.c)
    END Externalize;

    PROCEDURE (v: View) CopyFromSimpleView (source: Views.View);
    BEGIN
        WITH source: View DO v.c := source.c END
    END CopyFromSimpleView;

    PROCEDURE (v: View) Restore (f: Views.Frame; l, t, r, b: INTEGER);
        VAR w, h: INTEGER; bgnd: Ports.Color; g: Views.Frame;
    BEGIN
        g := f;
        REPEAT
            g := Views.HostOf(g);
            bgnd := Views.transparent;
            g.view.GetBackground(bgnd)
        UNTIL bgnd # Views.transparent;
        v.context.GetSize(w, h);
        Paint(f, h, v.c, bgnd)
    END Restore;
    
    PROCEDURE (v: View) HandleCtrlMsg (f: Views.Frame; VAR msg: Controllers.Message;
                                                                VAR focus: Views.View);
    BEGIN
        WITH msg: Properties.CollectMsg DO
            Views.HandlePropMsg(v, msg.poll)
        | msg: Properties.EmitMsg DO
            Views.HandlePropMsg(v, msg.set)
        ELSE    (* ignore other messages *)
        END
    END HandleCtrlMsg;

    PROCEDURE (v: View) HandlePropMsg (VAR msg: Properties.Message);
        VAR q: Properties.Property; p: Properties.StdProp;
            cop: ChangeColorOp;
    BEGIN
        WITH msg: Properties.SizePref DO
            IF (msg.w > Views.undefined) & (msg.h > Views.undefined) THEN
                (* constrain proposed size *)
                Properties.ProportionalConstraint(W, 1, msg.fixedW, msg.fixedH, msg.w, msg.h)
            ELSE
                (* return default size *)
                msg.w := W * baseSize; msg.h := baseSize
            END
        | msg: Properties.PollMsg DO
            NEW(p); p.known := {Properties.color}; p.valid := p.known;
            p.color.val := v.c;
            msg.prop := p
        | msg: Properties.SetMsg DO
            q := msg.prop;
            WHILE q # NIL DO
                WITH q: Properties.StdProp DO
                    IF Properties.color IN q.valid THEN
                        NEW(cop); cop.view := v; cop.color := q.color.val;
                        Views.Do(v, changeColorKey, cop)
                    END;
                ELSE
                END;
                q :=q.next
            END
        ELSE
        END
    END HandlePropMsg;
    
    PROCEDURE Deposit*;
        VAR v: View;
    BEGIN
        NEW(v); v.c := Ports.grey50; Views.Deposit(v)
    END Deposit;

END StdLogos.
