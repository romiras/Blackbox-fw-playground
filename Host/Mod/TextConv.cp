MODULE HostTextConv;
(**
    project    = "BlackBox"
    organization    = "www.oberon.ch"
    contributors    = "Oberon microsystems, Alexander Iljin"
    version    = "System/Rsrc/About"
    copyright    = "System/Rsrc/About"
    license    = "Docu/BB-License"
    changes    = ""
    issues    = ""

**)

    IMPORT
        SYSTEM, WinApi, WinOle, COM,
        Files, Fonts, Ports, Stores, Views, Properties,
        HostFonts, HostClipboard, TextModels,
        TextRulers, TextViews, TextMappers;
    
    CONST
        CR = 0DX; LF = 0AX; FF = 0EX; TAB = 09X;
        halfpoint = Ports.point DIV 2;
        twips = Ports.point DIV 20;

    TYPE
        Context = POINTER TO RECORD
            next: Context;
            dest: INTEGER;
            uniCnt : INTEGER;
            attr: TextModels.Attributes;
            pattr: TextRulers.Attributes
        END;
        MemReader = POINTER TO RECORD (Files.Reader)
            adr, pos: INTEGER
        END;
    
    VAR 
        debug*: BOOLEAN;
        

    (* MemReader *)
    
    PROCEDURE (r: MemReader) Base (): Files.File;
    BEGIN
        RETURN NIL
    END Base;
    
    PROCEDURE (r: MemReader) Pos (): INTEGER;
    BEGIN
        RETURN r.pos
    END Pos;
    
    PROCEDURE (r: MemReader) SetPos (pos: INTEGER);
    BEGIN
        r.pos := pos
    END SetPos;
    
    PROCEDURE (r: MemReader) ReadByte (OUT x: BYTE);
    BEGIN
        SYSTEM.GET(r.adr + r.pos, x); INC(r.pos)
    END ReadByte;
    
    PROCEDURE (r: MemReader) ReadBytes (VAR x: ARRAY OF BYTE; beg, len: INTEGER);
    BEGIN
        HALT(126)
    END ReadBytes;
    
    
    PROCEDURE GenGlobalMedium (hg: WinApi.HGLOBAL; unk: COM.IUnknown; VAR sm: WinOle.STGMEDIUM);
    BEGIN
        sm.tymed := WinOle.TYMED_HGLOBAL;
        sm.u.hGlobal := hg;
        sm.pUnkForRelease := unk
    END GenGlobalMedium;
    
    PROCEDURE MediumGlobal (VAR sm: WinOle.STGMEDIUM): WinApi.HGLOBAL;
    BEGIN
        ASSERT(sm.tymed = WinOle.TYMED_HGLOBAL, 20);
        RETURN sm.u.hGlobal
    END MediumGlobal;
    
    
    PROCEDURE WriteWndChar (wr: TextModels.Writer; ch: CHAR);
    BEGIN
        CASE ch OF
        | CR, TAB, " "..7EX, 0A0X..0FFX: wr.WriteChar(ch)
        | LF:
        | 80X: wr.WriteChar(20ACX)    (* euro *)
        | 82X: wr.WriteChar(201AX)
        | 83X: wr.WriteChar(0192X)
        | 84X: wr.WriteChar(201EX)
        | 85X: wr.WriteChar(2026X)
        | 86X: wr.WriteChar(2020X)
        | 87X: wr.WriteChar(2021X)
        | 88X: wr.WriteChar(02C6X)
        | 89X: wr.WriteChar(2030X)
        | 8AX: wr.WriteChar(0160X)
        | 8BX: wr.WriteChar(2039X)
        | 8CX: wr.WriteChar(0152X)
        | 91X: wr.WriteChar(2018X)
        | 92X: wr.WriteChar(2019X)
        | 93X: wr.WriteChar(201CX)
        | 94X: wr.WriteChar(201DX)
        | 95X: wr.WriteChar(2022X)
        | 96X: wr.WriteChar(2013X)
        | 97X: wr.WriteChar(2014X)
        | 98X: wr.WriteChar(02DCX)
        | 99X: wr.WriteChar(2122X)
        | 9AX: wr.WriteChar(0161X)
        | 9BX: wr.WriteChar(203AX)
        | 9CX: wr.WriteChar(0153X)
        | 9FX: wr.WriteChar(0178X)
        | 0X..8X, 0BX, 0CX, 0EX..1FX, 7FX, 81X, 8DX..90X, 9DX, 9EX:
            wr.WriteChar(CHR(0EF00H + ORD(ch)))
        END
    END WriteWndChar;
    
    PROCEDURE ThisWndChar (ch: CHAR): CHAR;
    BEGIN
        IF ch >= 100X THEN
            IF (ch >= 0EF00X) & (ch <= 0EFFFX) THEN ch := CHR(ORD(ch) - 0EF00H)
            ELSIF ch =  20ACX THEN ch := 80X    (* euro *)
            ELSIF ch =  201AX THEN ch := 82X
            ELSIF ch =  0192X THEN ch := 83X
            ELSIF ch =  201EX THEN ch := 84X
            ELSIF ch =  2026X THEN ch := 85X
            ELSIF ch =  2020X THEN ch := 86X
            ELSIF ch =  2021X THEN ch := 87X
            ELSIF ch =  02C6X THEN ch := 88X
            ELSIF ch =  2030X THEN ch := 89X
            ELSIF ch =  0160X THEN ch := 8AX
            ELSIF ch =  2039X THEN ch := 8BX
            ELSIF ch =  0152X THEN ch := 8CX
            ELSIF ch =  2018X THEN ch := 91X
            ELSIF ch =  2019X THEN ch := 92X
            ELSIF ch =  201CX THEN ch := 93X
            ELSIF ch =  201DX THEN ch := 94X
            ELSIF ch =  2022X THEN ch := 95X
            ELSIF ch =  2013X THEN ch := 96X
            ELSIF ch =  2014X THEN ch := 97X
            ELSIF ch =  02DCX THEN ch := 98X
            ELSIF ch =  2122X THEN ch := 99X
            ELSIF ch =  0161X THEN ch := 9AX
            ELSIF ch =  203AX THEN ch := 9BX
            ELSIF ch =  0153X THEN ch := 9CX
            ELSIF ch =  0178X THEN ch := 9FX
            ELSE  ch := "?"
            END
        ELSIF ch = 08FX THEN ch := " "    (* digit space *)
        END;
        RETURN ch
    END ThisWndChar;
    
    PROCEDURE ParseRichText (rd: Files.Reader; wr: TextModels.Writer; VAR defRuler: TextRulers.Ruler);
        TYPE 
            FontInfo = POINTER TO RECORD id: INTEGER; f: Fonts.Typeface; next: FontInfo END;
            ColorInfo = POINTER TO RECORD id: INTEGER; c: Ports.Color; next: ColorInfo END;
        CONST text = 0; fonttab = 1; colortab = 2; skip = 3;
        VAR ch: CHAR; tabStyle: SET;
            fact, val, defFont, dest, idx, fnum, cnum, paraPos, i: INTEGER;
            fonts, font: FontInfo; colors: ColorInfo;
            hasNum, remPar, skipDest: BOOLEAN;
            f: Fonts.Font; comm: ARRAY 32 OF CHAR;
            c, con: Context; p0: Properties.Property; p: TextRulers.Prop;
            ruler: TextRulers.Ruler;
            pattr: TextRulers.Attributes;
            skipCnt, uniCnt : INTEGER;
            
        PROCEDURE Color(i: INTEGER): ColorInfo;
            VAR c: ColorInfo;
        BEGIN
            ASSERT(colors # NIL, 20);
            c := colors;
            WHILE (c # NIL) & (c.id # i) DO c := c.next END;
            ASSERT(c # NIL, 100);
            RETURN c
        END Color;
        
        PROCEDURE SetColor(i: INTEGER; c: Ports.Color);
            VAR ci: ColorInfo;
        BEGIN
            NEW(ci); ci.id := i; ci.c := c; ci.next := colors; colors := ci
        END SetColor;
            
        PROCEDURE Font(i: INTEGER): FontInfo;
            VAR f: FontInfo;
        BEGIN
            ASSERT(fonts # NIL, 20);
            f := fonts;
            WHILE (f # NIL) & (f.id # i) DO f := f.next END;
            ASSERT(f # NIL, 100);
            RETURN f
        END Font;
        
        PROCEDURE SetFont(i: INTEGER; tf: Fonts.Typeface);
            VAR f: FontInfo;
        BEGIN
            NEW(f); f.id := i; f.f := tf; f.next := fonts; fonts := f
        END SetFont;
        
        PROCEDURE Next (VAR ch: CHAR);
            VAR b: BYTE;
        BEGIN
            rd.ReadByte(b); ch := CHR(b MOD 256)
        END Next;
        
        PROCEDURE Write (ch: CHAR);
        BEGIN
            IF skipCnt > 0 THEN
                DEC(skipCnt)
            ELSIF dest = text THEN
                IF ch < 100X THEN WriteWndChar(wr, ch)
                ELSE wr.WriteChar(ch)
                END
            ELSIF dest = fonttab THEN
                ASSERT(font # NIL, 20);
                font.f[idx] := ch; INC(idx); font.f[idx] := 0X
            END
        END Write;
        
        PROCEDURE Paragraph;
            VAR v: Views.View;
        BEGIN
            IF ~pattr.Equals(ruler.style.attr) THEN    (* new ruler needed *)
                wr.SetPos(paraPos);
                v := Views.CopyOf(ruler, Views.deep); ruler := v(TextRulers.Ruler);
                ruler.style.SetAttr(pattr);
                wr.WriteView(ruler, Views.undefined, Views.undefined);
                wr.SetPos(wr.Base().Length())
            ELSIF (pattr.first # pattr.left)
                    OR (pattr.lead > 0)
                    OR (TextRulers.pageBreak IN pattr.opts) THEN    (* paragraph marker needed *)
                wr.SetPos(paraPos);
                wr.WriteChar(FF);
                wr.SetPos(wr.Base().Length())
            END;
            wr.WriteChar(CR);
            paraPos := wr.Pos()
        END Paragraph;
        
    BEGIN
        defFont := 0; fnum := 1; f := Fonts.dir.Default(); NEW(fonts); fonts.f := f.typeface; skipCnt := 0; uniCnt := 1;
        cnum := 1; NEW(colors); SetColor(0, Ports.defaultColor);
        dest := text; con := NIL; paraPos := 0; remPar := FALSE; skipDest := FALSE;
        defRuler := TextRulers.dir.New(NIL); ruler := defRuler; pattr := defRuler.style.attr; tabStyle := {};
        Next(ch);
        WHILE ch # 0X DO
            IF ch = "{" THEN
                skipCnt := 0;
                NEW(c); c.dest := dest; c.attr := wr.attr; c.pattr := pattr; c.uniCnt := uniCnt; c.next := con; con := c;
                Next(ch)
            ELSIF ch = "}" THEN
                skipCnt := 0;
                IF con # NIL THEN
                    dest := con.dest; uniCnt := con.uniCnt; wr.SetAttr(con.attr); pattr := con.pattr; con := con.next
                END;
                Next(ch)
            ELSIF ch = "\" THEN
                Next(ch); i := 0; val := 0;
                IF (ch >= "a") & (ch <= "z") THEN
                    WHILE (ch >= "a") & (ch <= "z") DO comm[i] := ch; INC(i); Next(ch) END;
                    comm[i] := 0X; fact := 1; hasNum := FALSE;
                    IF ch = "-" THEN fact := -1; Next(ch) END;
                    WHILE (ch >= "0") & (ch <= "9") DO
                        val := 10 * val + ORD(ch) - ORD("0"); Next(ch); hasNum := TRUE
                    END;
                    val := val * fact;
                    IF ch = " " THEN Next(ch) END;
                    (* special characters *)
                    IF skipCnt > 0 THEN DEC(skipCnt)    (* command skipped as single character *)
                    ELSIF comm = "tab" THEN Write(TAB)
                    ELSIF comm = "line" THEN Write(CR)
                    ELSIF comm = "par" THEN Paragraph
                    ELSIF comm = "sect" THEN Paragraph
                    ELSIF comm =  "ldblquote" THEN Write(201CX) (* unicode: left double quote *)
                    ELSIF comm = "rdblquote" THEN Write(201DX) (* unicode: right double quote *)
                    ELSIF comm = "lquote" THEN Write(2018X) (* unicode: left single quote *)
                    ELSIF comm = "rquote" THEN Write(2019X) (* unicode: right single quote *)
                    ELSIF comm = "enspace" THEN Write(2002X) (* unicode: en space *)
                    ELSIF comm = "emspace" THEN Write(2003X) (* unicode: em space *)
                    ELSIF comm = "endash" THEN Write(2013X) (* unicode: en dash *)
                    ELSIF comm = "emdash" THEN Write(2014X) (* unicode: em dash *)
                    ELSIF comm = "page" THEN
                        Paragraph; NEW(p);
                        p.valid := {TextRulers.opts}; p.opts.val := {TextRulers.pageBreak}; p.opts.mask := p.opts.val;
                        pattr := TextRulers.ModifiedAttr(pattr, p)
                    (* character attributes *)
                    ELSIF comm = "plain" THEN
                        wr.SetAttr(TextModels.NewWeight(wr.attr, Fonts.normal));
                        wr.SetAttr(TextModels.NewStyle(wr.attr, {}));
                        wr.SetAttr(TextModels.NewTypeface(wr.attr, Font(defFont).f));
                        wr.SetAttr(TextModels.NewSize(wr.attr, 24 * halfpoint));
                        wr.SetAttr(TextModels.NewColor(wr.attr, Ports.defaultColor));
                        wr.SetAttr(TextModels.NewOffset(wr.attr, 0))
                    ELSIF comm = "b" THEN
                        IF hasNum & (val = 0) THEN wr.SetAttr(TextModels.NewWeight(wr.attr, Fonts.normal))
                        ELSE wr.SetAttr(TextModels.NewWeight(wr.attr, Fonts.bold))
                        END
                    ELSIF comm = "i" THEN
                        IF hasNum & (val = 0) THEN
                            wr.SetAttr(TextModels.NewStyle(wr.attr, wr.attr.font.style - {Fonts.italic}))
                        ELSE wr.SetAttr(TextModels.NewStyle(wr.attr, wr.attr.font.style + {Fonts.italic}))
                        END
                    ELSIF comm = "ul" THEN
                        IF hasNum & (val = 0) THEN
                            wr.SetAttr(TextModels.NewStyle(wr.attr, wr.attr.font.style - {Fonts.underline}))
                        ELSE wr.SetAttr(TextModels.NewStyle(wr.attr, wr.attr.font.style + {Fonts.underline}))
                        END
                    ELSIF comm = "strike" THEN
                        IF hasNum & (val = 0) THEN
                            wr.SetAttr(TextModels.NewStyle(wr.attr, wr.attr.font.style - {Fonts.strikeout}))
                        ELSE wr.SetAttr(TextModels.NewStyle(wr.attr, wr.attr.font.style + {Fonts.strikeout}))
                        END
                    ELSIF comm = "f" THEN
                        IF ~hasNum THEN val := defFont END;
                        IF dest = fonttab THEN 
                            fnum := val; idx := 0; NEW(font); font.id := val; font.next := fonts; fonts := font
                        ELSE 
                            wr.SetAttr(TextModels.NewTypeface(wr.attr, Font(val).f))
                        END
                    ELSIF comm = "fs" THEN
                        IF ~hasNum THEN val := 24 END;
                        wr.SetAttr(TextModels.NewSize(wr.attr, val * halfpoint))
                    ELSIF comm = "cf" THEN
                        wr.SetAttr(TextModels.NewColor(wr.attr, Color(val).c))
                    ELSIF comm = "dn" THEN
                        IF ~hasNum THEN val := 6 END;
                        wr.SetAttr(TextModels.NewOffset(wr.attr, -(val * halfpoint)))
                    ELSIF comm = "up" THEN
                        IF ~hasNum THEN val := 6 END;
                        wr.SetAttr(TextModels.NewOffset(wr.attr, val * halfpoint))
                    (* paragraph attributes *)
                    ELSIF comm = "pard" THEN
                        pattr := defRuler.style.attr; tabStyle := {}
                    ELSIF comm = "fi" THEN
                        NEW(p);
                        p.valid := {TextRulers.first}; p.first := pattr.left + val * twips;
                        IF p.first < 0 THEN    (* change left indent to make the value legal *)
                            p.valid := {TextRulers.left, TextRulers.first};
                            p.left := pattr.left - p.first; p.first := 0
                        END;
                        pattr := TextRulers.ModifiedAttr(pattr, p)
                    ELSIF comm = "li" THEN
                        NEW(p);
                        p.valid := {TextRulers.left, TextRulers.first};
                        p.left := val * twips; p.first := p.left + pattr.first - pattr.left;
                        pattr := TextRulers.ModifiedAttr(pattr, p)
                    ELSIF comm = "ql" THEN
                        NEW(p);
                        p.valid := {TextRulers.opts}; p.opts.val := {TextRulers.leftAdjust};
                        p.opts.mask := {TextRulers.leftAdjust, TextRulers.rightAdjust};
                        pattr := TextRulers.ModifiedAttr(pattr, p)
                    ELSIF comm = "qr" THEN
                        NEW(p);
                        p.valid := {TextRulers.opts}; p.opts.val := {TextRulers.rightAdjust};
                        p.opts.mask := {TextRulers.leftAdjust, TextRulers.rightAdjust};
                        pattr := TextRulers.ModifiedAttr(pattr, p)
                    ELSIF comm = "qc" THEN
                        NEW(p);
                        p.valid := {TextRulers.opts}; p.opts.val := {};
                        p.opts.mask := {TextRulers.leftAdjust, TextRulers.rightAdjust};
                        pattr := TextRulers.ModifiedAttr(pattr, p)
                    ELSIF comm = "qj" THEN
                        NEW(p);
                        p.valid := {TextRulers.opts}; p.opts.val := {TextRulers.leftAdjust, TextRulers.rightAdjust};
                        p.opts.mask := {TextRulers.leftAdjust, TextRulers.rightAdjust};
                        pattr := TextRulers.ModifiedAttr(pattr, p)
                    ELSIF comm = "sb" THEN
                        NEW(p);
                        p.valid := {TextRulers.lead}; p.lead := val * twips;
                        pattr := TextRulers.ModifiedAttr(pattr, p)
                    ELSIF comm = "sl" THEN
                        NEW(p);
                        p.valid := {TextRulers.grid}; p.grid := val * twips;
                        pattr := TextRulers.ModifiedAttr(pattr, p)
                    ELSIF comm = "tqc" THEN
                        tabStyle := {TextRulers.centerTab}
                    ELSIF (comm = "tqr") OR (comm="tqdec") THEN
                        tabStyle := {TextRulers.rightTab}
                    ELSIF comm = "tb" THEN
                        p0 := pattr.Prop(); p := p0(TextRulers.Prop);
                        p.valid := {TextRulers.tabs};
                        p.tabs.tab[p.tabs.len].stop := val * twips; 
                        p.tabs.tab[p.tabs.len].type := {TextRulers.barTab}; tabStyle := {}; 
                        INC(p.tabs.len);
                        pattr := TextRulers.ModifiedAttr(pattr, p)
                    ELSIF comm = "tx" THEN
                        p0 := pattr.Prop(); p := p0(TextRulers.Prop);
                        p.valid := {TextRulers.tabs};
                        p.tabs.tab[p.tabs.len].stop := val * twips; 
                        p.tabs.tab[p.tabs.len].type := tabStyle; tabStyle := {}; 
                        INC(p.tabs.len);
                        pattr := TextRulers.ModifiedAttr(pattr, p)
                    ELSIF comm = "pagebb" THEN
                        NEW(p);
                        p.valid := {TextRulers.opts}; p.opts.val := {TextRulers.pageBreak}; p.opts.mask := p.opts.val;
                        pattr := TextRulers.ModifiedAttr(pattr, p)
                    (* header *)
                    ELSIF comm = "deff" THEN
                        IF hasNum THEN defFont := val END
                    ELSIF comm = "fonttbl" THEN
                        IF dest # skip THEN dest := fonttab END
                    ELSIF comm = "colortbl" THEN
                        IF dest # skip THEN dest := colortab; cnum := 0; SetColor(0, 0) END
                    ELSIF comm = "red" THEN
                        IF dest = colortab THEN SetColor(cnum, Color(cnum).c + val MOD 256) END
                    ELSIF comm = "green" THEN
                        IF dest = colortab THEN SetColor(cnum, Color(cnum).c + val MOD 256 * 256) END
                    ELSIF comm = "blue" THEN
                        IF dest = colortab THEN SetColor(cnum, Color(cnum).c + val MOD 256 *  65536) END
                    ELSIF comm = "rtf" THEN
                    ELSIF comm = "ansi" THEN
                    ELSIF comm = "lang" THEN
                    ELSIF comm = "langfe" THEN
                    ELSIF comm = "loch" THEN
                    ELSIF comm = "ltrch" THEN
                    ELSIF comm = "rtlch" THEN
                    ELSIF comm = "ansicpg" THEN
                    (* misc *)
                    ELSIF comm = "bin" THEN rd.SetPos(rd.Pos() + val - 1); Next(ch)
                    (* unicode *)
                    ELSIF comm = "u" THEN Write(CHR(val)); skipCnt := uniCnt
                    ELSIF comm = "uc" THEN IF hasNum THEN uniCnt := val END
                    ELSIF comm = "upr" THEN dest := skip    (* skip ANSI part *)
                    ELSIF comm = "ud" THEN dest := text    (* use Unicode part *)
                    (* unhandled destinations *)
                    ELSIF comm = "author" THEN dest := skip
                    ELSIF comm = "buptim" THEN dest := skip
                    ELSIF comm = "comment" THEN dest := skip
                    ELSIF comm = "creatim" THEN dest := skip
                    ELSIF comm = "doccomm" THEN dest := skip
                    ELSIF comm = "footer" THEN dest := skip
                    ELSIF comm = "footerl" THEN dest := skip
                    ELSIF comm = "footerr" THEN dest := skip
                    ELSIF comm = "footerf" THEN dest := skip
                    ELSIF comm = "footnote" THEN dest := skip
                    ELSIF comm = "ftnsep" THEN dest := skip
                    ELSIF comm = "ftnsepc" THEN dest := skip
                    ELSIF comm = "ftncn" THEN dest := skip
                    ELSIF comm = "header" THEN dest := skip
                    ELSIF comm = "headerl" THEN dest := skip
                    ELSIF comm = "headerr" THEN dest := skip
                    ELSIF comm = "headerf" THEN dest := skip
                    ELSIF comm = "info" THEN dest := skip
                    ELSIF comm = "keywords" THEN dest := skip
                    ELSIF comm = "object" THEN dest := skip
                    ELSIF comm = "operator" THEN dest := skip
                    ELSIF comm = "pict" THEN dest := skip
                    ELSIF comm = "printim" THEN dest := skip
                    ELSIF comm = "private1" THEN dest := skip
                    ELSIF comm = "revtim" THEN dest := skip
                    ELSIF comm = "rxe" THEN dest := skip
                    ELSIF comm = "stylesheet" THEN dest := skip
                    ELSIF comm = "subject" THEN dest := skip
                    ELSIF comm = "tc" THEN dest := skip
                    ELSIF comm = "title" THEN dest := skip
                    ELSIF comm = "txe" THEN dest := skip
                    ELSIF comm = "xe" THEN dest := skip
                    ELSE (* unknown *)
                        IF skipDest & (con # NIL) & (con.next # NIL) THEN dest := skip END
                    END;
                    skipDest := FALSE
                ELSIF ch = "'" THEN
                    Next(ch);
                    IF ch <= "9" THEN val := ORD(ch) - ORD("0") ELSE val := ORD(CAP(ch)) - ORD("A") + 10 END;
                    Next(ch);
                    IF ch <= "9" THEN val := 16 * val + ORD(ch) - ORD("0")
                    ELSE val := 16 * val + ORD(CAP(ch)) - ORD("A") + 10
                    END;
                    Write(CHR(val)); Next(ch)
                ELSE
                    IF ch = "~" THEN Write(0A0X)    (* nonbreaking space *)
                    ELSIF ch = "-" THEN Write(0ADX)    (* soft hyphen *)
                    ELSIF ch = "_" THEN Write(2011X)    (* nonbreaking hyphen *)
                    ELSIF ch = "*" THEN skipDest := TRUE
                    ELSIF (ch = LF) OR (ch = CR) THEN Paragraph
                    ELSIF (ch = "{") OR (ch = "}") OR (ch = "\") THEN Write(ch)
                    END;
                    Next(ch)
                END
            ELSIF ch = ";" THEN
                IF dest = fonttab THEN font := Font(fnum); font.f[idx] := 0X; INC(idx)
                ELSIF dest = colortab THEN INC(cnum); SetColor(cnum, 0)
                ELSIF dest = text THEN Write(";")
                END;
                Next(ch)
            ELSIF ch >= " " THEN
                Write(ch); Next(ch)
            ELSE 
                Next(ch)
            END
        END
    END ParseRichText;
    
    PROCEDURE ConvertToRichText (in: TextViews.View; beg, end: INTEGER; VAR out: TextModels.Model);
        VAR r: TextModels.Reader; w: TextMappers.Formatter; ch: CHAR; f: Fonts.Font;
            attr, attr0: TextModels.Attributes; col: Ports.Color; tf, atf: Fonts.Typeface; p, size, asize, offs: INTEGER;
            style, astyle: SET; weight, aweight: INTEGER; rattr, rattr0: TextRulers.Attributes; ruler: TextRulers.Ruler;
            text: TextModels.Model; firstLine, firstLine0: BOOLEAN; fonts: ARRAY 256 OF Fonts.Typeface;
            colors: ARRAY 256 OF Ports.Color; fnum, cnum, i: INTEGER;
    BEGIN
        out := TextModels.dir.New(); w.ConnectTo(out);
        f := Fonts.dir.Default(); tf := f.typeface;
        fnum := 1; fonts[0] := tf;
        cnum := 1; colors[0] := Ports.defaultColor;
        col := Ports.defaultColor; size := 12 * Ports.point;
        offs := 0; style := {}; weight := Fonts.normal;
        attr0 := NIL; rattr0 := NIL; firstLine := TRUE; firstLine0 := FALSE;
        text := in.ThisModel(); r := text.NewReader(NIL);
        ruler := TextViews.ThisRuler(in, beg); rattr := ruler.style.attr;
        r.SetPos(beg); r.ReadChar(ch);
        WHILE ~r.eot & (r.Pos() <= end) DO
            attr := r.attr;
            IF (r.view # NIL) & (r.view IS TextRulers.Ruler) THEN
                ruler := r.view(TextRulers.Ruler); rattr := ruler.style.attr;
                firstLine := TRUE
            ELSIF ch = FF THEN firstLine := TRUE
            END;
            IF (rattr # rattr0) OR (firstLine # firstLine0) THEN
                IF (rattr # rattr0) OR (rattr.first # rattr.left) OR (rattr.lead # 0) OR (TextRulers.pageBreak IN rattr.opts)
                THEN
                    w.WriteSString("\pard");
                    IF rattr.left # 0 THEN
                        w.WriteSString("\li"); w.WriteInt(rattr.left DIV twips)
                    END;
                    IF firstLine & (rattr.first # rattr.left) THEN
                        w.WriteSString("\fi"); w.WriteInt((rattr.first - rattr.left) DIV twips)
                    END;
                    IF firstLine & (rattr.lead # 0) THEN
                        w.WriteSString("\sb"); w.WriteInt(rattr.lead DIV twips)
                    END;
                    IF rattr.grid > Ports.point THEN
                        w.WriteSString("\sl"); w.WriteInt(rattr.grid DIV twips); w.WriteSString("\slmult1")
                    END;
                    IF {TextRulers.leftAdjust, TextRulers.rightAdjust} - rattr.opts = {} THEN w.WriteSString("\qj")
                    ELSIF TextRulers.rightAdjust IN rattr.opts THEN w.WriteSString("\qr")
                    ELSIF ~(TextRulers.leftAdjust IN rattr.opts) THEN w.WriteSString("\qc")
                    END;
                    IF firstLine & (TextRulers.pageBreak IN rattr.opts) THEN
                        w.WriteSString("\pagebb")
                    END;
                    i := 0;
                    WHILE i < rattr.tabs.len DO
                        IF TextRulers.centerTab IN rattr.tabs.tab[i].type THEN w.WriteSString("\tqc") END; 
                        IF TextRulers.rightTab IN rattr.tabs.tab[i].type THEN w.WriteSString("\tqr") END; 
                        IF TextRulers.barTab IN rattr.tabs.tab[i].type THEN w.WriteSString("\tb") END; 
                        w.WriteSString("\tx"); w.WriteInt(rattr.tabs.tab[i].stop DIV twips);
                        INC(i)
                    END;
                    w.WriteChar(" ")
                END;
                rattr0 := rattr; firstLine0 := firstLine
            END;
            IF attr # attr0 THEN
                p := w.Pos();
                IF attr.color # col THEN
                    i := 0; WHILE (i < cnum) & (colors[i] # attr.color) DO INC(i) END;
                    IF i = cnum THEN colors[i] := attr.color; INC(cnum) END;
                    w.WriteSString("\cf"); w.WriteInt(i);
                    col := attr.color
                END;
                atf := attr.font.typeface$; asize := attr.font.size; astyle := attr.font.style; aweight := attr.font.weight;
                IF atf # tf THEN
                    i := 0; WHILE (i < fnum) & (fonts[i] # atf) DO INC(i) END;
                    IF i = fnum THEN fonts[i] := atf; INC(fnum) END;
                    w.WriteSString("\f"); w.WriteInt(i);
                    tf := atf
                END;
                IF asize # size THEN
                    w.WriteSString("\fs"); w.WriteInt(asize DIV halfpoint);
                    size := asize
                END;
                IF astyle # style THEN
                    IF (Fonts.italic IN astyle) & ~(Fonts.italic IN style) THEN w.WriteSString("\i")
                    ELSIF ~(Fonts.italic IN astyle) & (Fonts.italic IN style) THEN w.WriteSString("\i0")
                    END;
                    IF (Fonts.underline IN astyle) & ~(Fonts.underline IN style) THEN w.WriteSString("\ul")
                    ELSIF ~(Fonts.underline IN astyle) & (Fonts.underline IN style) THEN w.WriteSString("\ul0")
                    END;
                    IF (Fonts.strikeout IN astyle) & ~(Fonts.strikeout IN style) THEN w.WriteSString("\strike")
                    ELSIF ~(Fonts.strikeout IN astyle) & (Fonts.strikeout IN style) THEN w.WriteSString("\strike0")
                    END;
                    style := astyle
                END;
                IF aweight # weight THEN
                    IF (aweight > Fonts.normal) & (weight = Fonts.normal) THEN w.WriteSString("\b")
                    ELSIF (aweight = Fonts.normal) & (weight > Fonts.normal) THEN w.WriteSString("\b0")
                    END;
                    weight := aweight
                END;
                IF attr.offset # offs THEN
                    IF attr.offset > 0 THEN w.WriteSString("\up"); w.WriteInt(attr.offset DIV halfpoint)
                    ELSIF attr.offset < 0 THEN w.WriteSString("\dn"); w.WriteInt(-(attr.offset DIV halfpoint))
                    ELSIF offs > 0 THEN w.WriteSString("\up0")
                    ELSE w.WriteSString("\dn0")
                    END;
                    offs := attr.offset
                END;
                IF w.Pos() # p THEN w.WriteChar(" ") END;
                attr0 := attr
            END;
            IF ch >= 100X THEN
                IF ch = 2002X THEN w.WriteSString("\enspace ")
                ELSIF ch = 2003X THEN w.WriteSString("\emspace ")
                ELSIF ch = 2013X THEN w.WriteSString("\endash ")
                ELSIF ch = 2014X THEN w.WriteSString("\emdash ")
                ELSIF ch = 2010X THEN w.WriteChar("-")
                ELSIF ch = 2011X THEN w.WriteSString("\_")
                ELSIF ch = 201CX THEN (* unicode: left double quote *) w.WriteSString("\ldblquote ")
                ELSIF ch = 201DX THEN (* unicode: right double quote *) w.WriteSString("\rdblquote ")
                ELSIF ch = 2018X THEN (* unicode: left single quote *) w.WriteSString("\lquote ")
                ELSIF ch = 2019X THEN (* unicode: right single quote *) w.WriteSString("\rquote ")                    
                ELSE
                    w.WriteSString("\u"); w.WriteInt(ORD(ch)); 
                    ch := ThisWndChar(ch);
                    IF ch >= 80X THEN
                        w.WriteSString("\'"); 
                        w.WriteIntForm(ORD(ch), TextMappers.hexadecimal, 2, "0", FALSE)
                    ELSE
                        w.WriteChar(ch)
                    END
                END
            ELSE
                CASE ch OF 
                | TAB: w.WriteSString("\tab ")
                | CR: w.WriteSString("\par "); w.WriteLn; firstLine := FALSE
                | " ".."[", "]".."z", "|", "~": w.WriteChar(ch)
                | "\": w.WriteSString("\\")
                | "{": w.WriteSString("\{")
                | "}": w.WriteSString("\}")
                | 8FX: (* digit space *) w.WriteChar(" ")
                | 90X: (* hyphen *) w.WriteChar("-")
                | 91X: (* non-breaking hyphen *) w.WriteSString("\_")
                | 0A0X: (* non-breaking space *) w.WriteSString("\~")
                | 0ADX: (* soft hyphen *) w.WriteSString("\-")
                | 0A1X..0ACX, 0AEX..0FFX:
                    w.WriteSString("\'"); w.WriteIntForm(ORD(ch), TextMappers.hexadecimal, 2, "0", FALSE)
                ELSE
                END
            END;
            r.ReadChar(ch)
        END;
        w.WriteChar("}");
        (* header *)
        w.SetPos(0);
        w.WriteSString("{\rtf1\ansi\ansicpg1252\deff0");
        w.WriteSString("{\fonttbl"); i := 0;
        WHILE i < fnum DO
            IF fonts[i] = Fonts.default THEN fonts[i] := HostFonts.defFont.alias$ END;
            w.WriteSString("{\f"); w.WriteInt(i); w.WriteSString("\fnil "); w.WriteString(fonts[i]); w.WriteSString(";}");
            INC(i)
        END;
        w.WriteChar("}"); w.WriteLn;
        w.WriteSString("{\colortbl;"); i := 1;
        WHILE i < cnum DO
            w.WriteSString("\red"); w.WriteInt(colors[i] MOD 256);
            w.WriteSString("\green"); w.WriteInt(colors[i] DIV 256 MOD 256);
            w.WriteSString("\blue"); w.WriteInt(colors[i] DIV 65536 MOD 256);
            w.WriteChar(";"); INC(i)
        END;
        w.WriteChar("}"); w.WriteLn;
        w.WriteSString("\deftab216 ");
        w.WriteSString("\plain")
    END ConvertToRichText;
    
    
    PROCEDURE ImportDText* (VAR med: WinOle.STGMEDIUM; OUT v: Views.View;
                                            OUT w, h: INTEGER; OUT isSingle: BOOLEAN);
        VAR t: TextModels.Model; res, adr: INTEGER; wr: TextModels.Writer; ch: SHORTCHAR;
            hnd: WinApi.HANDLE; attr: TextModels.Attributes; p: Properties.StdProp; pref: Properties.BoundsPref;
    BEGIN
        hnd := MediumGlobal(med);
        ASSERT(hnd # 0, 20);
        adr := WinApi.GlobalLock(hnd);
        t := TextModels.dir.New(); wr := t.NewWriter(NIL);
        IF HostClipboard.cloneAttributes THEN
            Properties.CollectStdProp(p);
            NEW(attr); attr.InitFromProp(p);
            wr.SetAttr(attr)
        END;
        SYSTEM.GET(adr, ch);
        WHILE ch # 0X DO
            WriteWndChar(wr, ch);
            INC(adr); SYSTEM.GET(adr, ch)
        END;
        res := WinApi.GlobalUnlock(hnd);
        v := TextViews.dir.New(t);
        pref.w := Views.undefined; pref.h := Views.undefined;
        Views.HandlePropMsg(v, pref);
        w := pref.w; h := pref.h; isSingle := FALSE
    END ImportDText;
        
    PROCEDURE ImportDRichText* (VAR med: WinOle.STGMEDIUM; OUT v: Views.View;
                                                OUT w, h: INTEGER; OUT isSingle: BOOLEAN);
        VAR t: TextModels.Model; res, adr: INTEGER; wr: TextModels.Writer; rd: MemReader;
            hnd: WinApi.HANDLE; ruler: TextRulers.Ruler; pref: Properties.BoundsPref;
    BEGIN
        IF debug THEN
            ImportDText(med, v, w, h, isSingle);
            RETURN
        END;        
        hnd := MediumGlobal(med);
        ASSERT(hnd # 0, 20);
        adr := WinApi.GlobalLock(hnd);
        NEW(rd); rd.adr := adr; rd.pos := 0;
        t := TextModels.dir.New(); wr := t.NewWriter(NIL);
        ParseRichText(rd, wr, ruler);
        res := WinApi.GlobalUnlock(hnd);
        v := TextViews.dir.New(t);
        v(TextViews.View).SetDefaults(ruler, TextModels.dir.attr);
        pref.w := Views.undefined; pref.h := Views.undefined;
        Views.HandlePropMsg(v, pref);
        w := pref.w; h := pref.h; isSingle := FALSE
    END ImportDRichText;
    
    PROCEDURE ImportDUnicode* (VAR med: WinOle.STGMEDIUM; OUT v: Views.View;
                                                OUT w, h: INTEGER; OUT isSingle: BOOLEAN);
        VAR t: TextModels.Model; res, adr: INTEGER; wr: TextModels.Writer; uc: CHAR;
            hnd: WinApi.HANDLE; attr: TextModels.Attributes; p: Properties.StdProp; pref: Properties.BoundsPref;
    BEGIN
        hnd := MediumGlobal(med);
        ASSERT(hnd # 0, 20);
        adr := WinApi.GlobalLock(hnd);
        t := TextModels.dir.New(); wr := t.NewWriter(NIL);
        IF HostClipboard.cloneAttributes THEN
            Properties.CollectStdProp(p);
            NEW(attr); attr.InitFromProp(p);
            wr.SetAttr(attr)
        END;
        SYSTEM.GET(adr, uc);
        WHILE uc # 0X DO
            ASSERT(uc # 0FFFEX, 100);
            IF uc < 100X THEN WriteWndChar(wr, uc)
            ELSIF uc # 0FEFFX THEN wr.WriteChar(uc)
            END;
            INC(adr, 2); SYSTEM.GET(adr, uc)
        END;
        res := WinApi.GlobalUnlock(hnd);
        v := TextViews.dir.New(t);
        pref.w := Views.undefined; pref.h := Views.undefined;
        Views.HandlePropMsg(v, pref);
        w := pref.w; h := pref.h; isSingle := FALSE
    END ImportDUnicode;

    PROCEDURE ExportDText* (
        v: Views.View; w, h, x, y: INTEGER; isSingle: BOOLEAN; VAR med: WinOle.STGMEDIUM
    );
        VAR t: TextModels.Model; r: TextModels.Reader; ch: CHAR;
            res, len, adr: INTEGER; hnd: WinApi.HANDLE;
    BEGIN
        ASSERT(v # NIL, 20);
        WITH v: TextViews.View DO
            t := v.ThisModel();
            hnd := WinApi.GlobalAlloc({1, 13}, 2 * t.Length() + 1);    (* movable, sharable *)
            IF hnd # 0 THEN
                adr  := WinApi.GlobalLock(hnd); len := 0;
                r := t.NewReader(NIL); r.ReadChar(ch);
                WHILE ~r.eot DO
                    IF (ch # TextModels.viewcode) & (ch # TextModels.para) THEN
                        ch := ThisWndChar(ch);
                        SYSTEM.PUT(adr, SHORT(ch)); INC(adr); INC(len);
                        IF ch = CR THEN SYSTEM.PUT(adr, LF); INC(adr); INC(len) END
                    END;
                    r.ReadChar(ch)
                END;
                SYSTEM.PUT(adr, 0X); INC(len);
                res := WinApi.GlobalUnlock(hnd);
                hnd := WinApi.GlobalReAlloc(hnd, len, {});
                GenGlobalMedium(hnd, NIL, med)
            END
        ELSE
        END
    END ExportDText;
    
    PROCEDURE ExportDRichText* (
        v: Views.View; w, h, x, y: INTEGER; isSingle: BOOLEAN; VAR med: WinOle.STGMEDIUM
    );
        VAR t: TextModels.Model; r: TextModels.Reader; ch: CHAR; res, adr: INTEGER; hnd: WinApi.HANDLE;
    BEGIN
        ASSERT(v # NIL, 20);
        WITH v: TextViews.View DO
            ConvertToRichText(v, 0, MAX(INTEGER), t);
            hnd := WinApi.GlobalAlloc({1, 13}, t.Length() + 1);    (* movable, sharable *)
            IF hnd # 0 THEN
                adr := WinApi.GlobalLock(hnd);
                r := t.NewReader(NIL); r.ReadChar(ch);
                WHILE ~r.eot DO
                    SYSTEM.PUT(adr, SHORT(ch)); INC(adr);
                    r.ReadChar(ch)
                END;
                SYSTEM.PUT(adr, 0X);
                res := WinApi.GlobalUnlock(hnd);
                GenGlobalMedium(hnd, NIL, med)
            END
        ELSE
        END
    END ExportDRichText;
    
    PROCEDURE ExportDUnicode* (
        v: Views.View; w, h, x, y: INTEGER; isSingle: BOOLEAN; VAR med: WinOle.STGMEDIUM
    );
        VAR t: TextModels.Model; r: TextModels.Reader; ch: CHAR; res, len, adr: INTEGER; hnd: WinApi.HANDLE;
    BEGIN
        ASSERT(v # NIL, 20);
        WITH v: TextViews.View DO
            t := v.ThisModel();
            hnd := WinApi.GlobalAlloc({1, 13}, 4 * t.Length() + 2);    (* movable, sharable *)
            IF hnd # 0 THEN
                adr  := WinApi.GlobalLock(hnd); len := 0;
                r := t.NewReader(NIL); r.ReadChar(ch);
                WHILE ~r.eot DO
                    IF ch = CR THEN
                        SYSTEM.PUT(adr, LONG(CR)); INC(adr, 2); INC(len, 2);
                        SYSTEM.PUT(adr, LONG(LF)); INC(adr, 2); INC(len, 2)
                    ELSIF (ch >= " ") OR (ch = TAB) THEN
                        IF (ch >= 0EF00X) & (ch <= 0EFFFX) THEN ch := CHR(ORD(ch) - 0EF00H) END;
                        SYSTEM.PUT(adr, ch); INC(adr, 2); INC(len, 2)
                    END;
                    r.ReadChar(ch)
                END;
                SYSTEM.PUT(adr, LONG(0X)); INC(len, 2);
                res := WinApi.GlobalUnlock(hnd);
                hnd := WinApi.GlobalReAlloc(hnd, len, {});
                GenGlobalMedium(hnd, NIL, med)
            END
        ELSE
        END
    END ExportDUnicode;

    PROCEDURE ImportText* (f: Files.File; OUT s: Stores.Store);
        VAR r: Stores.Reader; t: TextModels.Model; wr: TextModels.Writer; ch, nch: SHORTCHAR;
    BEGIN
        ASSERT(f # NIL, 20);
        r.ConnectTo(f); r.SetPos(0);
        t := TextModels.dir.New(); wr := t.NewWriter(NIL);
        r.ReadSChar(ch);
        WHILE ~r.rider.eof DO
            r.ReadSChar(nch);
            IF (ch = CR) & (nch = LF) THEN r.ReadSChar(nch)
            ELSIF ch = LF THEN ch := CR
            END;
            WriteWndChar(wr, ch); ch := nch
        END;
        s := TextViews.dir.New(t)
    END ImportText;

    PROCEDURE ImportTabText* (f: Files.File; OUT s: Stores.Store);
        VAR r: Stores.Reader; t: TextModels.Model; wr: TextModels.Writer; ch, nch: SHORTCHAR;
    BEGIN
        ASSERT(f # NIL, 20);
        r.ConnectTo(f); r.SetPos(0);
        t := TextModels.dir.New(); wr := t.NewWriter(NIL);
        r.ReadSChar(ch);
        WHILE ~r.rider.eof DO
            r.ReadSChar(nch);
            IF (ch = CR) & (nch = LF) THEN r.ReadSChar(nch)
            ELSIF ch = LF THEN ch := CR
            ELSIF (ch = " ") & (nch = " ") THEN ch := TAB; r.ReadSChar(nch)
            END;
            WriteWndChar(wr, ch); ch := nch
        END;
        s := TextViews.dir.New(t)
    END ImportTabText;

    PROCEDURE ImportRichText* (f: Files.File; OUT s: Stores.Store);
        VAR t: TextModels.Model; wr: TextModels.Writer; rd: Files.Reader; ruler: TextRulers.Ruler;
    BEGIN
        rd := f.NewReader(NIL); rd.SetPos(0);
        t := TextModels.dir.New(); wr := t.NewWriter(NIL);
        ParseRichText(rd, wr, ruler);
        s := TextViews.dir.New(t);
        s(TextViews.View).SetDefaults(ruler, TextModels.dir.attr)
    END ImportRichText;
    
    PROCEDURE ImportUnicode* (f: Files.File; OUT s: Stores.Store);
        VAR r: Stores.Reader; t: TextModels.Model; v: TextViews.View; w: TextModels.Writer;
            ch0, ch1: SHORTCHAR; len, res: INTEGER; uc: CHAR; rev: BOOLEAN;
    BEGIN
        ASSERT(f # NIL, 20);
        r.ConnectTo(f); r.SetPos(0);
        len := f.Length(); rev := FALSE;
        t := TextModels.dir.New(); w := t.NewWriter(NIL); w.SetPos(0);
        WHILE len > 0 DO
            r.ReadSChar(ch0); r.ReadSChar(ch1);
            IF rev THEN uc := CHR(ORD(ch1) + 256 * ORD(ch0))
            ELSE uc := CHR(ORD(ch0) + 256 * ORD(ch1))
            END;
            DEC(len, 2);
            IF uc = 0FFFEX THEN rev := ~rev
            ELSIF uc < 100X THEN WriteWndChar(w, uc)
            ELSIF uc # 0FEFFX THEN w.WriteChar(uc)
            END
        END;
        v := TextViews.dir.New(t);
        s := v
    END ImportUnicode;
    
    PROCEDURE ImportDosText* (f: Files.File; OUT s: Stores.Store);
        VAR r: Stores.Reader; t: TextModels.Model; wr: TextModels.Writer; ch, nch: SHORTCHAR;
        
        PROCEDURE ConvertChar (wr: TextModels.Writer; ch: CHAR);
        (* PC Code Page Mappings M4 (Latin) to Unicode Encoding *)
        (* Reference: The Unicode Standard, Version 1.0, Vol 1, Addison Wesley, p. 536 *)
        BEGIN        
            CASE ch OF
            | CR, TAB, " "..7EX: wr.WriteChar(ch)
            | LF:
            | 080X: wr.WriteChar(0C7X)
            | 081X: wr.WriteChar(0FCX)
            | 082X: wr.WriteChar(0E9X)
            | 083X: wr.WriteChar(0E2X)
            | 084X: wr.WriteChar(0E4X)
            | 085X: wr.WriteChar(0E0X)
            | 086X: wr.WriteChar(0E5X)
            | 087X: wr.WriteChar(0E7X)
            | 088X: wr.WriteChar(0EAX)
            | 089X: wr.WriteChar(0EBX)
            | 08AX: wr.WriteChar(0E8X)
            | 08BX: wr.WriteChar(0EFX)
            | 08CX: wr.WriteChar(0EEX)
            | 08DX: wr.WriteChar(0ECX)
            | 08EX: wr.WriteChar(0C4X)
            | 08FX: wr.WriteChar(0C5X)
            | 090X: wr.WriteChar(0C9X)
            | 091X: wr.WriteChar(0E6X)
            | 092X: wr.WriteChar(0C6X)
            | 093X: wr.WriteChar(0F4X)
            | 094X: wr.WriteChar(0F6X)
            | 095X: wr.WriteChar(0F2X)
            | 096X: wr.WriteChar(0FBX)
            | 097X: wr.WriteChar(0F9X)
            | 098X: wr.WriteChar(0FFX)
            | 099X: wr.WriteChar(0D6X)
            | 09AX: wr.WriteChar(0DCX)
            | 09BX: wr.WriteChar(0F8X)
            | 09CX: wr.WriteChar(0A3X)
            | 09DX: wr.WriteChar(0D8X)
            | 09EX: wr.WriteChar(0D7X)
            | 09FX: wr.WriteChar(0192X)
            | 0A0X: wr.WriteChar(0E1X)
            | 0A1X: wr.WriteChar(0EDX)
            | 0A2X: wr.WriteChar(0F3X)
            | 0A3X: wr.WriteChar(0FAX)
            | 0A4X: wr.WriteChar(0F1X)
            | 0A5X: wr.WriteChar(0D1X)
            | 0A6X: wr.WriteChar(0AAX)
            | 0A7X: wr.WriteChar(0BAX)
            | 0A8X: wr.WriteChar(0BFX)
            | 0A9X: wr.WriteChar(0AEX)
            | 0AAX: wr.WriteChar(0ACX)
            | 0ABX: wr.WriteChar(0BDX)
            | 0ACX: wr.WriteChar(0BCX)
            | 0ADX: wr.WriteChar(0A1X)
            | 0AEX: wr.WriteChar(0ABX)
            | 0AFX: wr.WriteChar(0BBX)
            | 0B5X: wr.WriteChar(0C1X)
            | 0B6X: wr.WriteChar(0C2X)
            | 0B7X: wr.WriteChar(0C0X)
            | 0B8X: wr.WriteChar(0A9X)
            | 0BDX: wr.WriteChar(0A2X)
            | 0BEX: wr.WriteChar(0A5X)
            | 0C6X: wr.WriteChar(0E3X)
            | 0C7X: wr.WriteChar(0C3X)
            | 0CFX: wr.WriteChar(0A4X)
            | 0D0X: wr.WriteChar(0F0X)
            | 0D1X: wr.WriteChar(0D0X)
            | 0D2X: wr.WriteChar(0CAX)
            | 0D3X: wr.WriteChar(0CBX)
            | 0D4X: wr.WriteChar(0C8X)
            | 0D5X: wr.WriteChar(0131X)
            | 0D6X: wr.WriteChar(0CDX)
            | 0D7X: wr.WriteChar(0CEX)
            | 0D8X: wr.WriteChar(0CFX)
            | 0DDX: wr.WriteChar(0A6X)
            | 0DEX: wr.WriteChar(0CCX)
            | 0E0X: wr.WriteChar(0D3X)
            | 0E1X: wr.WriteChar(0DFX)
            | 0E2X: wr.WriteChar(0D4X)
            | 0E3X: wr.WriteChar(0D2X)
            | 0E4X: wr.WriteChar(0F5X)
            | 0E5X: wr.WriteChar(0D5X)
            | 0E6X: wr.WriteChar(0B5X)
            | 0E7X: wr.WriteChar(0FEX)
            | 0E8X: wr.WriteChar(0DEX)
            | 0E9X: wr.WriteChar(0DAX)
            | 0EAX: wr.WriteChar(0DBX)
            | 0EBX: wr.WriteChar(0D9X)
            | 0ECX: wr.WriteChar(0FDX)
            | 0EDX: wr.WriteChar(0DDX)
            | 0EEX: wr.WriteChar(0AFX)
            | 0EFX: wr.WriteChar(0B4X)
            | 0F0X: wr.WriteChar(0ADX)
            | 0F1X: wr.WriteChar(0B1X)
            | 0F2X: wr.WriteChar(02017X)
            | 0F3X: wr.WriteChar(0BEX)
            | 0F4X: wr.WriteChar(0B6X)
            | 0F5X: wr.WriteChar(0A7X)
            | 0F6X: wr.WriteChar(0F7X)
            | 0F7X: wr.WriteChar(0B8X)
            | 0F8X: wr.WriteChar(0B0X)
            | 0F9X: wr.WriteChar(0A8X)
            | 0FAX: wr.WriteChar(0B7X)
            | 0FBX: wr.WriteChar(0B9X)
            | 0FCX: wr.WriteChar(0B3X)
            | 0FDX: wr.WriteChar(0B2X)
            | 0X..8X, 0BX, 0CX, 0EX..1FX, 7FX,
              0B0X..0B4X, 0B9X..0BCX, 0BFX..0C5X, 0C8X..0CEX, 0D9X..0DCX, 0DFX, 0FEX, 0FFX:
                wr.WriteChar(CHR(0EF00H + ORD(ch)))
            END
        END ConvertChar;
        
    BEGIN
        ASSERT(f # NIL, 20);
        r.ConnectTo(f); r.SetPos(0);
        t := TextModels.dir.New(); wr := t.NewWriter(NIL);
        r.ReadSChar(ch);
        WHILE ~r.rider.eof DO
            r.ReadSChar(nch);
            IF (ch = CR) & (nch = LF) THEN r.ReadSChar(nch)
            ELSIF ch = LF THEN ch := CR
            END;
            ConvertChar(wr, ch); ch := nch
        END;
        s := TextViews.dir.New(t)
    END ImportDosText;

    PROCEDURE TextView(s: Stores.Store): Stores.Store;
    BEGIN
        IF s IS Views.View THEN RETURN Properties.ThisType(s(Views.View), "TextViews.View")
        ELSE RETURN NIL
        END
    END TextView;

    PROCEDURE ExportText* (s: Stores.Store; f: Files.File);
        VAR w: Stores.Writer; t: TextModels.Model; r: TextModels.Reader; ch: CHAR;
    BEGIN
        ASSERT(s # NIL, 20); ASSERT(f # NIL, 21);
        s := TextView(s);
        IF s # NIL THEN
            w.ConnectTo(f); w.SetPos(0);
            t := s(TextViews.View).ThisModel();
            IF t # NIL THEN
                r := t.NewReader(NIL);
                r.ReadChar(ch);
                WHILE ~r.eot DO
                    IF (ch # TextModels.viewcode) & (ch # TextModels.para) THEN
                        ch := ThisWndChar(ch);
                        w.WriteSChar(SHORT(ch));
                        IF ch = CR THEN w.WriteSChar(LF) END
                    END;
                    r.ReadChar(ch)
                END
            END
        END
    END ExportText;
    
    PROCEDURE ExportTabText* (s: Stores.Store; f: Files.File);
        VAR w: Stores.Writer; t: TextModels.Model; r: TextModels.Reader; ch: CHAR;
    BEGIN
        ASSERT(s # NIL, 20); ASSERT(f # NIL, 21);
        s := TextView(s);
        IF s # NIL THEN
            w.ConnectTo(f); w.SetPos(0);
            t := s(TextViews.View).ThisModel();
            IF t # NIL THEN
                r := t.NewReader(NIL);
                r.ReadChar(ch);
                WHILE ~r.eot DO
                    IF (ch # TextModels.viewcode) & (ch # TextModels.para) THEN
                        ch := ThisWndChar(ch);
                        IF ch = CR THEN w.WriteSChar(CR); w.WriteSChar(LF)
                        ELSIF ch = TAB THEN w.WriteSChar(" "); w.WriteSChar(" ")
                        ELSE w.WriteSChar(SHORT(ch))
                        END
                    END;
                    r.ReadChar(ch)
                END
            END
        END
    END ExportTabText;
    
    PROCEDURE ExportRichText* (s: Stores.Store; f: Files.File);
        VAR t: TextModels.Model; r: TextModels.Reader; ch: CHAR; w: Stores.Writer;
    BEGIN
        ASSERT(s # NIL, 20); ASSERT(f # NIL, 21);
        WITH s: TextViews.View DO
            ConvertToRichText(s, 0, MAX(INTEGER), t);
            w.ConnectTo(f); w.SetPos(0);
            r := t.NewReader(NIL); r.ReadChar(ch);
            WHILE ~r.eot DO
                w.WriteSChar(SHORT(ch)); r.ReadChar(ch)
            END
(*
            w.WriteSChar(0X)
*)
        ELSE
        END
    END ExportRichText;
    
    PROCEDURE ExportUnicode* (s: Stores.Store; f: Files.File);
        VAR w: Stores.Writer; t: TextModels.Model; r: TextModels.Reader; ch: CHAR;
    BEGIN
        ASSERT(s # NIL, 20); ASSERT(f # NIL, 21);
        s := TextView(s);
        IF s # NIL THEN
            w.ConnectTo(f); w.SetPos(0);
            w.WriteChar(0FEFFX);    (* little endian *)
            t := s(TextViews.View).ThisModel();
            IF t # NIL THEN
                r := t.NewReader(NIL);
                r.ReadChar(ch);
                WHILE ~r.eot DO
                    IF ch = CR THEN
                        w.WriteChar(CR); w.WriteChar(LF)
                    ELSIF (ch >= " ") OR (ch = TAB) THEN
                        IF (ch >= 0EF00X) & (ch <= 0EFFFX) THEN ch := CHR(ORD(ch) - 0EF00H) END;
                        w.WriteChar(ch)
                    END;
                    r.ReadChar(ch)
                END
            END
        END
    END ExportUnicode;
    
    PROCEDURE ImportHex* (f: Files.File; OUT s: Stores.Store);
        VAR r: Stores.Reader; t: TextModels.Model; w: TextMappers.Formatter; ch: SHORTCHAR; a: INTEGER;
            i: INTEGER; str: ARRAY 17 OF CHAR;
    BEGIN
        ASSERT(f # NIL, 20);
        r.ConnectTo(f); r.SetPos(0);
        t := TextModels.dir.New();
        w.ConnectTo(t); w.SetPos(0);
        r.ReadSChar(ch); a := 0;
        WHILE ~r.rider.eof DO
            IF a MOD 16 = 0 THEN
                w.WriteChar("[");
                w.WriteIntForm(a, TextMappers.hexadecimal, 8, "0", FALSE);
                w.WriteSString("]")
            END;
            w.WriteIntForm(ORD(ch), TextMappers.hexadecimal, 2, "0", FALSE);
            IF ch > 20X THEN str[a MOD 16] := ch ELSE str[a MOD 16] := "" END;
            INC(a);
            IF a MOD 16 = 0 THEN
                str[16] := 0X; w.WriteString(""); w.WriteString(str);
                w.WriteLn
            ELSIF a MOD 4 = 0 THEN
                w.WriteString("")
            ELSE
                w.WriteChar("")
            END;
            r.ReadSChar(ch)
        END;
        IF a MOD 16 # 0 THEN
            str[a MOD 16] := 0X;
            i := (16 - a MOD 16) * 3 + (3 - a MOD 16 DIV 4) + 3;
            WHILE i # 0 DO w.WriteChar(""); DEC(i) END;
            w.WriteString(str)
        END;
        s := TextViews.dir.New(t)
    END ImportHex;

END HostTextConv.
