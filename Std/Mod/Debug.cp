MODULE StdDebug;
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

    IMPORT SYSTEM,
        Kernel, Strings, Fonts, Services, Ports, Views, Properties, Dialog, Containers, StdFolds,
        TextModels, TextMappers, TextViews, TextRulers;
    
    CONST
        refViewSize = 9 * Ports.point;
        
        heap = 1; source = 2; module = 3; modules = 4;    (* RefView types *)
        
    TYPE
        Name = Kernel.Name;

        ArrayPtr = POINTER TO RECORD
            last, t, first: INTEGER;    (* gc header *)
            len: ARRAY 16 OF INTEGER    (* dynamic array length table *)
        END;

        RefView = POINTER TO RefViewDesc;

        RefViewDesc = RECORD
            type: SHORTINT;
            command: SHORTINT;
            back: RefView;
            adr: INTEGER;
            desc: Kernel.Type;
            ptr: ArrayPtr;
            name: Name
        END;
        
        Action = POINTER TO RECORD (Services.Action)
            text: TextModels.Model
        END;
        
        Cluster = POINTER TO RECORD [untagged]    (* must correspond to Kernel.Cluster *)
            size: INTEGER;
            next: Cluster
        END;
        
    
    VAR
        out: TextMappers.Formatter;
        path: ARRAY 4 OF Ports.Point;
        empty: Name;


    PROCEDURE NewRuler (): TextRulers.Ruler;
        CONST mm = Ports.mm;
        VAR r: TextRulers.Ruler;
    BEGIN
        r := TextRulers.dir.New(NIL);
        TextRulers.SetRight(r, 140 * mm);
        TextRulers.AddTab(r, 4 * mm); TextRulers.AddTab(r, 34 * mm); TextRulers.AddTab(r, 80 * mm);
        RETURN r
    END NewRuler;

    PROCEDURE OpenViewer (t: TextModels.Model; title: Views.Title; ruler:TextRulers.Ruler);
        VAR v: TextViews.View; c: Containers.Controller;
    BEGIN
        Dialog.MapString(title, title);
        v := TextViews.dir.New(t);
        v.SetDefaults(ruler, TextViews.dir.defAttr);
        c := v.ThisController();
        IF c # NIL THEN
            c.SetOpts(c.opts - {Containers.noFocus, Containers.noSelection} + {Containers.noCaret})
        END;
        Views.OpenAux(v, title)
    END OpenViewer;
    
    PROCEDURE OpenFold (hidden: ARRAY OF CHAR);
        VAR fold: StdFolds.Fold; t: TextModels.Model; w: TextMappers.Formatter;
    BEGIN
        Dialog.MapString(hidden, hidden);
        t := TextModels.dir.New();
        w.ConnectTo(t); w.WriteString(hidden);
        fold := StdFolds.dir.New(StdFolds.expanded, "", t);
        out.WriteView(fold)
    END OpenFold;
    
    PROCEDURE CloseFold (collaps: BOOLEAN);
        VAR fold: StdFolds.Fold; m: TextModels.Model;
    BEGIN
        fold := StdFolds.dir.New(StdFolds.expanded, "", NIL);
        out.WriteView(fold);
        IF collaps THEN fold.Flip(); m := out.rider.Base(); out.SetPos(m.Length()) END
    END CloseFold;
    
    PROCEDURE WriteHex (n: INTEGER);
    BEGIN
        out.WriteIntForm(n, TextMappers.hexadecimal, 9, "0", TextMappers.showBase)
    END WriteHex;
    
    PROCEDURE WriteString (adr, len, base: INTEGER; zterm, unicode: BOOLEAN);
        CONST beg = 0; char = 1; code = 2;
        VAR ch: CHAR; sc: SHORTCHAR; val, mode: INTEGER; str: ARRAY 16 OF CHAR;
    BEGIN
        mode := beg;
        IF base = 2 THEN SYSTEM.GET(adr, ch); val := ORD(ch) ELSE SYSTEM.GET(adr, sc); val := ORD(sc) END;
        IF zterm & (val = 0) THEN out.WriteSString('""')
        ELSE
            REPEAT
                IF (val >= ORD(" ")) & (val < 7FH) OR (val > 0A0H) & (val < 100H) OR unicode & (val >= 100H) THEN
                    IF mode # char THEN
                        IF mode = code THEN out.WriteSString(", ") END;
                        out.WriteChar(22X); mode := char
                    END;
                    out.WriteChar(CHR(val))
                ELSE
                    IF mode = char THEN out.WriteChar(22X) END;
                    IF mode # beg THEN out.WriteSString(", ") END;
                    mode := code; Strings.IntToStringForm(val, Strings.hexadecimal, 1, "0", FALSE, str);
                    IF str[0] > "9" THEN out.WriteChar("0") END;
                    out.WriteString(str); out.WriteChar("X")
                END;
                INC(adr, base); DEC(len);
                IF base = 2 THEN SYSTEM.GET(adr, ch); val := ORD(ch) ELSE SYSTEM.GET(adr, sc); val := ORD(sc) END
            UNTIL (len = 0) OR zterm & (val = 0)
        END;
        IF mode = char THEN out.WriteChar(22X) END
    END WriteString;
    
    PROCEDURE OutString (s: ARRAY OF CHAR);
        VAR str: Dialog.String;
    BEGIN
        Dialog.MapString(s, str);
        out.WriteString(str)
    END OutString;

    (* -------------------  variable display ------------------- *)
    
    PROCEDURE FormOf (t: Kernel.Type): SHORTCHAR;
    BEGIN
        IF SYSTEM.VAL(INTEGER, t) DIV 256 = 0 THEN
            RETURN SHORT(CHR(SYSTEM.VAL(INTEGER, t)))
        ELSE
            RETURN SHORT(CHR(16 + t.id MOD 4))
        END
    END FormOf;
    
    PROCEDURE LenOf (t: Kernel.Type; ptr: ArrayPtr): INTEGER;
    BEGIN
        IF t.size # 0 THEN RETURN t.size
        ELSIF ptr # NIL THEN RETURN ptr.len[t.id DIV 16 MOD 16 - 1]
        ELSE RETURN 0
        END
    END LenOf;
    
    PROCEDURE SizeOf (t: Kernel.Type; ptr: ArrayPtr): INTEGER;
    BEGIN
        CASE FormOf(t) OF
        | 0BX: RETURN 0
        | 1X, 2X, 4X: RETURN 1
        | 3X, 5X: RETURN 2
        | 8X, 0AX: RETURN 8
        | 11X: RETURN t.size
        | 12X: RETURN LenOf(t, ptr) * SizeOf(t.base[0], ptr)
        ELSE RETURN 4
        END
    END SizeOf;

    PROCEDURE WriteName (t: Kernel.Type; ptr: ArrayPtr);
        VAR name: Kernel.Name; f: SHORTCHAR;
    BEGIN
        f := FormOf(t);
        CASE f OF
        | 0X: OutString("#Dev:Unknown")
        | 1X: out.WriteSString("BOOLEAN")
        | 2X: out.WriteSString("SHORTCHAR")
        | 3X: out.WriteSString("CHAR")
        | 4X: out.WriteSString("BYTE")
        | 5X: out.WriteSString("SHORTINT")
        | 6X: out.WriteSString("INTEGER")
        | 7X: out.WriteSString("SHORTREAL")
        | 8X: out.WriteSString("REAL")
        | 9X: out.WriteSString("SET")
        | 0AX: out.WriteSString("LONGINT")
        | 0BX: out.WriteSString("ANYREC")
        | 0CX: out.WriteSString("ANYPTR")
        | 0DX: out.WriteSString("POINTER")
        | 0EX: out.WriteSString("PROCEDURE")
        | 0FX: out.WriteSString("STRING")
        | 10X..13X:
            Kernel.GetTypeName(t, name);
            IF name = "!" THEN
                IF f = 11X THEN out.WriteSString("RECORD")
                ELSIF f = 12X THEN out.WriteSString("ARRAY")
                ELSE OutString("#Dev:Unknown")
                END
            ELSIF (t.id DIV 256 # 0) & (t.mod.refcnt >= 0) THEN
                out.WriteSString(t.mod.name); out.WriteChar("."); out.WriteSString(name)
            ELSIF f = 11X THEN
                out.WriteSString(t.mod.name); out.WriteSString(".RECORD") 
            ELSIF f = 12X THEN
                out.WriteSString("ARRAY "); out.WriteInt(LenOf(t, ptr)); t := t.base[0];
                WHILE (FormOf(t) = 12X) & ((t.id DIV 256 = 0) OR (t.mod.refcnt < 0)) DO
                    out.WriteSString(", "); out.WriteInt(LenOf(t, ptr)); t := t.base[0]
                END;
                out.WriteSString(" OF "); WriteName(t, ptr)
            ELSIF f = 13X THEN
                out.WriteSString("POINTER")
            ELSE
                out.WriteSString("PROCEDURE")
            END
        | 20X: out.WriteSString("COM.IUnknown")
        | 21X: out.WriteSString("COM.GUID")
        | 22X: out.WriteSString("COM.RESULT")
        ELSE OutString("#Dev:UnknownFormat"); out.WriteInt(ORD(f))
        END
    END WriteName;
    
    PROCEDURE WriteGuid (a: INTEGER);

        PROCEDURE Hex (a: INTEGER);
            VAR x: SHORTCHAR;
        BEGIN
            SYSTEM.GET(a, x);
            out.WriteIntForm(ORD(x), TextMappers.hexadecimal, 2, "0", FALSE)
        END Hex;

    BEGIN
        out.WriteChar("{");
        Hex(a + 3); Hex(a + 2); Hex(a + 1); Hex(a);
        out.WriteChar("-");
        Hex(a + 5); Hex(a + 4);
        out.WriteChar("-");
        Hex(a + 7); Hex(a + 6);
        out.WriteChar("-");
        Hex(a + 8);
        Hex(a + 9);
        out.WriteChar("-");
        Hex(a + 10);
        Hex(a + 11);
        Hex(a + 12);
        Hex(a + 13);
        Hex(a + 14);
        Hex(a + 15);
        out.WriteChar("}")
    END WriteGuid;
    
    PROCEDURE^ ShowVar (ad, ind: INTEGER; f, c: SHORTCHAR; desc: Kernel.Type; ptr: ArrayPtr;
                                            back: RefView; VAR name, sel: Name);
    
    PROCEDURE ShowRecord (a, ind: INTEGER; desc: Kernel.Type; back: RefView; VAR sel: Name);
        VAR dir: Kernel.Directory; obj: Kernel.Object; name: Kernel.Name; i, j, n: INTEGER; base: Kernel.Type;
    BEGIN
        WriteName(desc, NIL); out.WriteTab;
        IF desc.mod.refcnt >= 0 THEN
            OpenFold("#Dev:Fields");
            n := desc.id DIV 16 MOD 16; j := 0;
            WHILE j <= n DO
                base := desc.base[j];
                IF base # NIL THEN
                    dir := base.fields; i := 0;
                    WHILE i < dir.num DO
                        obj := SYSTEM.VAL(Kernel.Object, SYSTEM.ADR(dir.obj[i]));
                        Kernel.GetObjName(base.mod, obj, name);
                        ShowVar(a + obj.offs, ind, FormOf(obj.struct), 1X, obj.struct, NIL, back, name, sel);
                        INC(i)
                    END
                END;
                INC(j)
            END;
            out.WriteSString("   "); CloseFold((ind > 1) OR (sel # ""))
        ELSE
            OutString("#Dev:Unloaded")
        END
    END ShowRecord;
    
    PROCEDURE ShowArray (a, ind: INTEGER; desc: Kernel.Type; ptr: ArrayPtr; back: RefView; VAR sel: Name);
        VAR f: SHORTCHAR; i, n, m, size, len: INTEGER; name: Kernel.Name; eltyp, t: Kernel.Type;
            vi: SHORTINT; vs: BYTE; str: Dialog.String; high: BOOLEAN;
    BEGIN
        WriteName(desc, ptr); out.WriteTab;
        len := LenOf(desc, ptr); eltyp := desc.base[0]; f := FormOf(eltyp); size := SizeOf(eltyp, ptr);
        IF (f = 2X) OR (f = 3X) THEN    (* string *)
            n := 0; m := len; high := FALSE;
            IF f = 2X THEN
                REPEAT SYSTEM.GET(a + n, vs); INC(n) UNTIL (n = 32) OR (n = len) OR (vs = 0);
                REPEAT DEC(m); SYSTEM.GET(a + m, vs) UNTIL (m = 0) OR (vs # 0)
            ELSE
                REPEAT
                    SYSTEM.GET(a + n * 2, vi); INC(n);
                    IF vi DIV 256 # 0 THEN high := TRUE END
                UNTIL (n = len) OR (vi = 0);
                n := MIN(n, 32);
                REPEAT DEC(m); SYSTEM.GET(a + m * 2, vi) UNTIL (m = 0) OR (vi # 0)
            END;
            WriteString(a, n, size, TRUE, TRUE);
            INC(m, 2);
            IF m > len THEN m := len END;
            IF high OR (m > n) THEN
                out.WriteSString("   "); OpenFold("...");
                out.WriteLn;
                IF high & (n = 32) THEN
                    WriteString(a, m, size, TRUE, TRUE);
                    out.WriteLn; out.WriteLn
                END;
                WriteString(a, m, size, FALSE, FALSE);
                IF m < len THEN out.WriteSString(", ..., 0X") END;
                out.WriteSString("   "); CloseFold(TRUE)
            END
        ELSE
            t := eltyp;
            WHILE FormOf(t) = 12X DO t := t.base[0] END;
            IF FormOf(t) # 0X THEN
                OpenFold("#Dev:Elements");
                i := 0;
                WHILE i < len DO
                    Strings.IntToString(i, str);
                    name := "[" + SHORT(str$) + "]";
                    ShowVar(a, ind, f, 1X, eltyp, ptr, back, name, sel);
                    INC(i); INC(a, size)
                END;
                out.WriteSString("   "); CloseFold(TRUE)
            END
        END
    END ShowArray;
    
    PROCEDURE ShowProcVar (a: INTEGER);
        VAR vli, n, ref: INTEGER; m: Kernel.Module; name: Kernel.Name;
    BEGIN
        SYSTEM.GET(a, vli);
        Kernel.SearchProcVar(vli, m, vli);
        IF m = NIL THEN
            IF vli = 0 THEN out.WriteSString("NIL")
            ELSE WriteHex(vli)
            END
        ELSE
            IF m.refcnt >= 0 THEN
                out.WriteSString(m.name); ref := m.refs;
                REPEAT Kernel.GetRefProc(ref, n, name) UNTIL (n = 0) OR (vli < n);
                IF vli < n THEN out.WriteChar("."); out.WriteSString(name) END
            ELSE
                OutString("#Dev:ProcInUnloadedMod");
                out.WriteSString(m.name); out.WriteSString(" !!!")
            END
        END
    END ShowProcVar;

    PROCEDURE ShowPointer (a: INTEGER; f: SHORTCHAR; desc: Kernel.Type; back: RefView; VAR sel: Name);
        VAR adr, x: INTEGER; ptr: ArrayPtr; c: Cluster; btyp: Kernel.Type;
    BEGIN
        SYSTEM.GET(a, adr);
        IF f = 13X THEN btyp := desc.base[0] ELSE btyp := NIL END;
        IF adr = 0 THEN out.WriteSString("NIL")
        ELSIF f = 20X THEN
            out.WriteChar("["); WriteHex(adr); out.WriteChar("]");
            out.WriteChar(" "); c := SYSTEM.VAL(Cluster, Kernel.Root());
            WHILE (c # NIL) & ((adr < SYSTEM.VAL(INTEGER, c)) OR (adr >= SYSTEM.VAL(INTEGER, c) + c.size)) DO c := c.next END;
            IF c # NIL THEN
                ptr := SYSTEM.VAL(ArrayPtr, adr)
            END
        ELSE
            IF (f = 13X) OR (f = 0CX) THEN x := adr - 4 ELSE x := adr END;
            IF ((adr < -4) OR (adr >= 65536)) & Kernel.IsReadable(x, adr + 16) THEN
                out.WriteChar("["); WriteHex(adr); out.WriteChar("]");
                IF (f = 13X) OR (f = 0CX) THEN
                    out.WriteChar(" "); c := SYSTEM.VAL(Cluster, Kernel.Root());
                    WHILE (c # NIL) & ((adr < SYSTEM.VAL(INTEGER, c)) OR (adr >= SYSTEM.VAL(INTEGER, c) + c.size)) DO
                        c := c.next
                    END;
                    IF c # NIL THEN
                        ptr := SYSTEM.VAL(ArrayPtr, adr);
                        IF (f = 13X) & (FormOf(btyp) = 12X) THEN    (* array *)
                            adr := SYSTEM.ADR(ptr.len[btyp.id DIV 16 MOD 16]) 
                        END
                    ELSE OutString("#Dev:IllegalPointer")
                    END
                END
            ELSE OutString("#Dev:IllegalAddress"); WriteHex(adr)
            END
        END
    END ShowPointer;
    
    PROCEDURE ShowSelector (ref: RefView);
        VAR b: RefView; n: SHORTINT; a, a0: TextModels.Attributes;
    BEGIN
        b := ref.back; n := 1;
        IF b # NIL THEN
            WHILE (b.name = ref.name) & (b.back # NIL) DO INC(n); b := b.back END;
            ShowSelector(b);
            IF n > 1 THEN out.WriteChar("(") END;
            out.WriteChar(".")
        END;
        out.WriteSString(ref.name);
        IF ref.type = heap THEN out.WriteChar("^") END;
        IF n > 1 THEN
            out.WriteChar(")");
            a0 := out.rider.attr; a := TextModels.NewOffset(a0, 2 * Ports.point);
            out.rider.SetAttr(a);
            out.WriteInt(n); out.rider.SetAttr(a0)
        END
    END ShowSelector;
    
    PROCEDURE ShowVar (ad, ind: INTEGER; f, c: SHORTCHAR; desc: Kernel.Type; ptr: ArrayPtr; back: RefView;
                                            VAR name, sel: Name);
        VAR i, j, vli, a: INTEGER; tsel: Name; a0: TextModels.Attributes;
            vc: SHORTCHAR; vsi: BYTE; vi: SHORTINT; vr: SHORTREAL; vlr: REAL; vs: SET;
    BEGIN
        out.WriteLn; out.WriteTab; i := 0;
        WHILE i < ind DO out.WriteSString("  "); INC(i) END;
        a := ad; i := 0; j := 0;
        IF sel # "" THEN
            WHILE sel[i] # 0X DO tsel[i] := sel[i]; INC(i) END;
            IF (tsel[i-1] # ":") & (name[0] # "[") THEN tsel[i] := "."; INC(i) END
        END;
        WHILE name[j] # 0X DO tsel[i] := name[j]; INC(i); INC(j) END;
        tsel[i] := 0X;
        a0 := out.rider.attr;
        IF c = 3X THEN    (* varpar *)
            SYSTEM.GET(ad, a);
            out.rider.SetAttr(TextModels.NewStyle(a0, {Fonts.italic}))
        END;
        IF name[0] # "[" THEN out.WriteChar(".") END;
        out.WriteSString(name);
        out.rider.SetAttr(a0); out.WriteTab;
        IF (c = 3X) & (a >= 0) & (a < 65536) THEN 
            out.WriteTab; out.WriteSString("NIL VARPAR")
        ELSIF f = 11X THEN
            Kernel.GetTypeName(desc, name);
            IF (c = 3X) & (name[0] # "!") THEN SYSTEM.GET(ad + 4, desc) END;    (* dynamic type *)
            ShowRecord(a, ind + 1, desc, back, tsel)
        ELSIF (c = 3X) & (f = 0BX) THEN    (* VAR anyrecord *)
            SYSTEM.GET(ad + 4, desc);
            ShowRecord(a, ind + 1, desc, back, tsel)
        ELSIF f = 12X THEN
            IF (desc.size = 0) & (ptr = NIL) THEN SYSTEM.GET(ad, a) END;    (* dyn array val par *)
            IF ptr = NIL THEN ptr := SYSTEM.VAL(ArrayPtr, ad - 8) END;
            ShowArray(a, ind + 1, desc, ptr, back, tsel)
        ELSE
            IF desc = NIL THEN desc := SYSTEM.VAL(Kernel.Type, ORD(f)) END;
            WriteName(desc, NIL); out.WriteTab;
            CASE f OF
            | 0X: (* SYSTEM.GET(a, vli); WriteHex(vli) *)
            | 1X: SYSTEM.GET(a, vc); 
                IF vc = 0X THEN out.WriteSString("FALSE")
                ELSIF vc = 1X THEN out.WriteSString("TRUE")
                ELSE OutString("#Dev:Undefined"); out.WriteInt(ORD(vc))
                END
            | 2X: WriteString(a, 1, 1, FALSE, FALSE)
            | 3X: WriteString(a, 1, 2, FALSE, TRUE);
                    SYSTEM.GET(a, vi);
                    IF vi DIV 256 # 0 THEN out.WriteString("  "); WriteString(a, 1, 2, FALSE, FALSE) END
            | 4X: SYSTEM.GET(a, vsi); out.WriteInt(vsi)
            | 5X: SYSTEM.GET(a, vi); out.WriteInt(vi)
            | 6X: SYSTEM.GET(a, vli); out.WriteInt(vli)
            | 7X: SYSTEM.GET(a, vr); out.WriteReal(vr)
            | 8X: SYSTEM.GET(a, vlr); out.WriteReal(vlr)
            | 9X: SYSTEM.GET(a, vs); out.WriteSet(vs)
            | 0AX: SYSTEM.GET(a, vli); SYSTEM.GET(a + 4, i);
                IF (vli >= 0) & (i = 0) OR (vli < 0) & (i = -1) THEN out.WriteInt(vli)
                ELSE out.WriteIntForm(i, TextMappers.hexadecimal, 8, "0", TextMappers.hideBase); WriteHex(vli)
                END
            | 0CX, 0DX, 13X, 20X: ShowPointer(a, f, desc, back, tsel)
            | 0EX, 10X: ShowProcVar(a)
            | 0FX: WriteString(a, 256, 1, TRUE, FALSE)
            | 21X: WriteGuid(a)
            | 22X: SYSTEM.GET(a, vli); WriteHex(vli)
            ELSE 
            END
        END
    END ShowVar;
    

    PROCEDURE ShowStack;
        VAR ref, end, i, j, x, a, b, c: INTEGER; m, f: SHORTCHAR; mod: Kernel.Module; name, sel: Kernel.Name;
            d: Kernel.Type;
    BEGIN
        a := Kernel.pc; b := Kernel.fp; c := 100;
        REPEAT
            mod := Kernel.modList;
            WHILE (mod # NIL) & ((a < mod.code) OR (a >= mod.code + mod.csize)) DO mod := mod.next END;
            IF mod # NIL THEN
                DEC(a, mod.code);
                IF mod.refcnt >= 0 THEN
                    out.WriteChar(" "); out.WriteSString(mod.name); ref := mod.refs;
                    REPEAT Kernel.GetRefProc(ref, end, name) UNTIL (end = 0) OR (a < end);
                    IF a < end THEN
                        out.WriteChar("."); out.WriteSString(name);
                        sel := mod.name$; i := 0;
                        WHILE sel[i] # 0X DO INC(i) END;
                        sel[i] := "."; INC(i); j := 0;
                        WHILE name[j] # 0X DO sel[i] := name[j]; INC(i); INC(j) END;
                        sel[i] := ":"; sel[i+1] := 0X;
                        out.WriteSString("   ["); WriteHex(a);
                        out.WriteSString("] ");
                        i := Kernel.SourcePos(mod, 0);
                        IF name # "$$" THEN
                            Kernel.GetRefVar(ref, m, f, d, x, name);
                            WHILE m # 0X DO
                                IF name[0] # "@" THEN ShowVar(b + x, 0, f, m, d, NIL, NIL, name, sel) END;
                                Kernel.GetRefVar(ref, m, f, d, x, name)
                            END
                        END;
                        out.WriteLn
                    ELSE out.WriteSString(".???"); out.WriteLn
                    END
                ELSE
                    out.WriteChar("("); out.WriteSString(mod.name);
                    out.WriteSString(")   (pc="); WriteHex(a);
                    out.WriteSString(",  fp="); WriteHex(b); out.WriteChar(")");
                    out.WriteLn
                END
            ELSE
                out.WriteSString("<system>   (pc="); WriteHex(a);
                out.WriteSString(",  fp="); WriteHex(b); out.WriteChar(")");
                out.WriteLn
            END;
            IF (b >= Kernel.fp) & (b < Kernel.stack) THEN
                SYSTEM.GET(b+4, a);    (* stacked pc *)
                SYSTEM.GET(b, b);    (* dynamic link *)
                DEC(a); DEC(c)
            ELSE c := 0
            END
        UNTIL c = 0
    END ShowStack;

    PROCEDURE (a: Action) Do;    (* delayed trap window open *)
    BEGIN
        Kernel.SetTrapGuard(TRUE);
        OpenViewer(a.text, "#Dev:Trap", NewRuler());
        Kernel.SetTrapGuard(FALSE);
    END Do;

    PROCEDURE GetTrapMsg(OUT msg: ARRAY OF CHAR);
        VAR ref, end, a: INTEGER; mod: Kernel.Module; name: Kernel.Name; head, tail, errstr: ARRAY 32 OF CHAR;
            key: ARRAY 128 OF CHAR;
    BEGIN
        a := Kernel.pc; mod := Kernel.modList;
        WHILE (mod # NIL) & ((a < mod.code) OR (a >= mod.code + mod.csize)) DO mod := mod.next END;
        IF mod # NIL THEN
            DEC(a, mod.code); ref := mod.refs;
            REPEAT Kernel.GetRefProc(ref, end, name) UNTIL (end = 0) OR (a < end);
            IF a < end THEN
                Kernel.SplitName (mod.name$, head, tail);
                IF head = "" THEN head := "System" END;
                Strings.IntToString(Kernel.err, errstr);
                key := tail + "." + name + "." + errstr;
                Dialog.MapString("#" + head + ":" + key, msg);
                (* IF key # msg THEN out.WriteString(" " + msg) END; *)
                IF key = msg THEN msg := "" END;
            END
        END
    END GetTrapMsg;

    PROCEDURE Trap;
        VAR a0: TextModels.Attributes; action: Action; msg: ARRAY 512 OF CHAR;
    BEGIN
        out.ConnectTo(TextModels.dir.New());
        a0 := out.rider.attr;
        out.rider.SetAttr(TextModels.NewWeight(a0, Fonts.bold));
        IF Kernel.err = 129 THEN out.WriteSString("invalid WITH")
        ELSIF Kernel.err = 130 THEN out.WriteSString("invalid CASE")
        ELSIF Kernel.err = 131 THEN out.WriteSString("function without RETURN")
        ELSIF Kernel.err = 132 THEN out.WriteSString("type guard")
        ELSIF Kernel.err = 133 THEN out.WriteSString("implied type guard")
        ELSIF Kernel.err = 134 THEN out.WriteSString("value out of range")
        ELSIF Kernel.err = 135 THEN out.WriteSString("index out of range")
        ELSIF Kernel.err = 136 THEN out.WriteSString("string too long")
        ELSIF Kernel.err = 137 THEN out.WriteSString("stack overflow")
        ELSIF Kernel.err = 138 THEN out.WriteSString("integer overflow")
        ELSIF Kernel.err = 139 THEN out.WriteSString("division by zero")
        ELSIF Kernel.err = 140 THEN out.WriteSString("infinite real result")
        ELSIF Kernel.err = 141 THEN out.WriteSString("real underflow")
        ELSIF Kernel.err = 142 THEN out.WriteSString("real overflow")
        ELSIF Kernel.err = 143 THEN out.WriteSString("undefined real result")
        ELSIF Kernel.err = 144 THEN out.WriteSString("not a number")
        ELSIF Kernel.err = 200 THEN out.WriteSString("keyboard interrupt")
        ELSIF Kernel.err = 201 THEN
            out.WriteSString("NIL dereference")
        ELSIF Kernel.err = 202 THEN
            out.WriteSString("illegal instruction: ");
            out.WriteIntForm(Kernel.val, TextMappers.hexadecimal, 5, "0", TextMappers.showBase)
        ELSIF Kernel.err = 203 THEN
            IF (Kernel.val >= -4) & (Kernel.val < 65536) THEN out.WriteSString("NIL dereference (read)")
            ELSE out.WriteSString("illegal memory read (ad = "); WriteHex(Kernel.val); out.WriteChar(")")
            END
        ELSIF Kernel.err = 204 THEN
            IF (Kernel.val >= -4) & (Kernel.val < 65536) THEN out.WriteSString("NIL dereference (write)")
            ELSE out.WriteSString("illegal memory write (ad = "); WriteHex(Kernel.val); out.WriteChar(")")
            END
        ELSIF Kernel.err = 205 THEN
            IF (Kernel.val >= -4) & (Kernel.val < 65536) THEN out.WriteSString("NIL procedure call")
            ELSE out.WriteSString("illegal execution (ad = "); WriteHex(Kernel.val); out.WriteChar(")")
            END
        ELSIF Kernel.err = 257 THEN out.WriteSString("out of memory")
        ELSIF Kernel.err = 10001H THEN out.WriteSString("bus error")
        ELSIF Kernel.err = 10002H THEN out.WriteSString("address error")
        ELSIF Kernel.err = 10007H THEN out.WriteSString("fpu error")
        ELSIF Kernel.err < 0 THEN
            out.WriteSString("Exception "); out.WriteIntForm(-Kernel.err, TextMappers.hexadecimal, 3, "0", TextMappers.showBase)
        ELSE
            out.WriteSString("TRAP "); out.WriteInt(Kernel.err);
            IF Kernel.err = 126 THEN out.WriteSString("  (not yet implemented)")
            ELSIF Kernel.err = 125 THEN out.WriteSString("  (call of obsolete procedure)")
            ELSIF Kernel.err >= 100 THEN out.WriteSString("  (invariant violated)")
            ELSIF Kernel.err >= 60 THEN out.WriteSString("  (postcondition violated)")
            ELSIF Kernel.err >= 20 THEN out.WriteSString("  (precondition violated)")
            END
        END;
        GetTrapMsg(msg);
        IF msg # "" THEN out.WriteLn; out.WriteString(msg) END;
        out.WriteLn; out.rider.SetAttr(a0);
        out.WriteLn; ShowStack;
        NEW(action); action.text := out.rider.Base();
        Services.DoLater(action, Services.now);
        out.ConnectTo(NIL)
    END Trap;

BEGIN
    Kernel.InstallTrapViewer(Trap);
    empty := "";
    path[0].x := refViewSize DIV 2; path[0].y := 0;
    path[1].x := refViewSize; path[1].y := refViewSize DIV 2;
    path[2].x := refViewSize DIV 2; path[2].y := refViewSize;
    path[3].x := 0; path[3].y := refViewSize DIV 2;
END StdDebug.
