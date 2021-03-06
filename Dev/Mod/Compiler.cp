MODULE DevCompiler;
(**
    project    = "BlackBox"
    organization    = "www.oberon.ch"
    contributors    = "Oberon microsystems"
    version    = "System/Rsrc/About"
    copyright    = "System/Rsrc/About"
    license    = "Docu/BB-License"
    references    = "ftp://ftp.inf.ethz.ch/pub/software/Oberon/OberonV4/Docu/OP2.Paper.ps"
    changes    = ""
    issues    = ""

**)

    IMPORT Kernel,
        Files, Views, Dialog, Controls,
        TextModels, TextMappers, TextViews, TextControllers,
        StdLog, StdDialog,
        DevMarkers, DevCommanders, DevSelectors,
        DevCPM, DevCPT, DevCPB, DevCPP, DevCPE, DevCPV := DevCPV486;

    CONST
        (* compiler options: *)
        checks = 0; allchecks = 1; assert = 2; obj = 3; ref = 4; allref = 5; srcpos = 6; reallib = 7; signatures = 8;
        hint = 29; oberon = 30; errorTrap = 31;
        defopt = {checks, assert, obj, ref, allref, srcpos, signatures};

        (* additional scanner types *)
        import = 100; module = 101; semicolon = 102; becomes = 103; comEnd = 104;

    VAR
        sourceR: TextModels.Reader;
        s: TextMappers.Scanner;
        str: Dialog.String;
        found: BOOLEAN;    (* DevComDebug was found -> DTC *)

    PROCEDURE Module (source: TextModels.Reader; opt: SET; log: TextModels.Model; VAR error: BOOLEAN);
        VAR ext, new: BOOLEAN; p: DevCPT.Node;
    BEGIN
        DevCPM.Init(source, log);
        IF found THEN INCL(DevCPM.options, DevCPM.comAware) END;
        IF errorTrap IN opt THEN INCL(DevCPM.options, DevCPM.trap) END;
        IF oberon IN opt THEN INCL(DevCPM.options, DevCPM.oberon) END;
        DevCPT.Init(opt);
        DevCPB.typSize := DevCPV.TypeSize;
        DevCPT.processor := DevCPV.processor;
        DevCPP.Module(p);
        IF DevCPM.noerr THEN
            IF DevCPT.libName # "" THEN EXCL(opt, obj) END;
(*
            IF errorTrap IN opt THEN DevCPDump.DumpTree(p) END;
*)
            DevCPV.Init(opt); DevCPV.Allocate; DevCPT.Export(ext, new);
            IF DevCPM.noerr & (obj IN opt) THEN
                DevCPV.Module(p)
            END;
            DevCPV.Close
        END;
        IF DevCPM.noerr & (new OR ext) THEN DevCPM.RegisterNewSym
        ELSE DevCPM.DeleteNewSym
        END;
        DevCPT.Close;
        error := ~DevCPM.noerr;
        DevCPM.Close;
        p := NIL;
        Kernel.FastCollect;
        IF error THEN
            DevCPM.InsertMarks(source.Base());
            DevCPM.LogWLn; DevCPM.LogWStr(" ");
            IF DevCPM.errors = 1 THEN
                Dialog.MapString("#Dev:OneErrorDetected", str)
            ELSE
                DevCPM.LogWNum(DevCPM.errors, 0); Dialog.MapString("#Dev:ErrorsDetected", str)
            END;
            StdLog.String(str)
        ELSE
            IF hint IN opt THEN DevCPM.InsertMarks(source.Base()) END;
            DevCPM.LogWStr("  "); DevCPM.LogWNum(DevCPE.pc, 8);
            DevCPM.LogWStr("  "); DevCPM.LogWNum(DevCPE.dsize, 8)
        END;
        DevCPM.LogWLn
    END Module;

    PROCEDURE Scan (VAR s: TextMappers.Scanner);
    BEGIN
        s.Scan;
        IF s.type = TextMappers.string THEN
            IF s.string = "MODULE" THEN s.type := module END
        ELSIF s.type = TextMappers.char THEN
            IF s.char = "(" THEN
                IF s.rider.char = "*" THEN
                    s.rider.Read;
                    REPEAT Scan(s) UNTIL (s.type = TextMappers.eot) OR (s.type = comEnd);
                    Scan(s)
                END
            ELSIF s.char = "*" THEN
                IF s.rider.char = ")" THEN s.rider.Read; s.type := comEnd END
            END
        END
    END Scan;

    PROCEDURE Do (source, log: TextModels.Model; beg: INTEGER; opt: SET; VAR error: BOOLEAN);
        VAR s: TextMappers.Scanner;
    BEGIN
        Dialog.MapString("#Dev:Compiling", str);
        StdLog.String(str); StdLog.Char(" ");
        s.ConnectTo(source); s.SetPos(beg);
        Scan(s);
        WHILE (s.type # TextMappers.eot) & (s.type # module) DO Scan(s) END;
        IF s.type = module THEN
            Scan(s);
            IF s.type = TextMappers.string THEN
                StdLog.Char('"'); StdLog.String(s.string); StdLog.Char('"')
            END
        END;
        sourceR := source.NewReader(NIL); sourceR.SetPos(beg);
        Module(sourceR, opt, log, error)
    END Do;


    PROCEDURE Open;
    BEGIN
        Dialog.ShowStatus("#Dev:Compiling");
        StdLog.buf.Delete(0, StdLog.buf.Length())
    END Open;

    PROCEDURE Close;
    BEGIN
        StdLog.text.Append(StdLog.buf);
        IF DevCPM.noerr THEN Dialog.ShowStatus("#Dev:Ok")
        END;
        sourceR := NIL;
        Kernel.Cleanup
    END Close;

    PROCEDURE Compile*;
        VAR t: TextModels.Model; error: BOOLEAN;
    BEGIN
        Open;
        t := TextViews.FocusText();
        IF t # NIL THEN
            Do(t, StdLog.text, 0, defopt, error);
            IF error THEN DevMarkers.ShowFirstError(t, TextViews.focusOnly) END
        ELSE Dialog.ShowMsg("#Dev:NoTextViewFound")
        END;
        Close
    END Compile;

    PROCEDURE CompileOpt* (opt: ARRAY OF CHAR);
        VAR t: TextModels.Model; error: BOOLEAN; i: INTEGER; opts: SET;
    BEGIN
        i := 0; opts := defopt;
        WHILE opt[i] # 0X DO
            IF opt[i] = "-" THEN
                IF srcpos IN opts THEN EXCL(opts, srcpos)
                ELSIF allref IN opts THEN EXCL(opts, allref)
                ELSIF ref IN opts THEN EXCL(opts, ref)
                ELSE EXCL(opts, obj)
                END
            ELSIF opt[i] = "!" THEN
                IF assert IN opts THEN EXCL(opts, assert)
                ELSE EXCL(opts, checks)
                END
            ELSIF opt[i] = "+" THEN INCL(opts, allchecks)
            ELSIF opt[i] = "?" THEN INCL(opts, hint)
            ELSIF opt[i] = "@" THEN INCL(opts, errorTrap)
            ELSIF opt[i] = "$" THEN INCL(opts, oberon)
            END;
            INC(i)
        END;
        Open;
        t := TextViews.FocusText();
        IF t # NIL THEN
            Do(t, StdLog.text, 0, opts, error);
            IF error THEN DevMarkers.ShowFirstError(t, TextViews.focusOnly) END
        ELSE Dialog.ShowMsg("#Dev:NoTextViewFound")
        END;
        Close
    END CompileOpt;

    PROCEDURE CompileText* (text: TextModels.Model; beg: INTEGER; OUT error: BOOLEAN);
    BEGIN
        ASSERT(text # NIL, 20); ASSERT((beg >= 0) & (beg < text.Length()), 21);
        Open;
        Do(text, StdLog.text, beg, defopt, error);
        IF error THEN DevMarkers.ShowFirstError(text, TextViews.focusOnly) END;
        Close
    END CompileText;

    PROCEDURE CompileAndUnload*;
        VAR t: TextModels.Model; error: BOOLEAN; mod: Kernel.Module; n: ARRAY 256 OF CHAR;
    BEGIN
        Open;
        t := TextViews.FocusText();
        IF t # NIL THEN
            Do(t, StdLog.text, 0, defopt, error);
            IF error THEN DevMarkers.ShowFirstError(t, TextViews.focusOnly)
            ELSE
                mod := Kernel.ThisLoadedMod(DevCPT.SelfName);
                IF mod # NIL THEN
                    Kernel.UnloadMod(mod);
                    n := DevCPT.SelfName$;
                    IF mod.refcnt < 0 THEN
                        Dialog.MapParamString("#Dev:Unloaded", n, "", "", str);
                        StdLog.String(str); StdLog.Ln;
                        Controls.Relink
                    ELSE
                        Dialog.MapParamString("#Dev:UnloadingFailed", n, "", "", str);
                        StdLog.String(str); StdLog.Ln
                    END
                END
            END
        ELSE Dialog.ShowMsg("#Dev:NoTextViewFound")
        END;
        Close
    END CompileAndUnload;

    PROCEDURE CompileSelection*;
        VAR c: TextControllers.Controller; t: TextModels.Model; beg, end: INTEGER; error: BOOLEAN;
    BEGIN
        Open;
        c := TextControllers.Focus();
        IF c # NIL THEN
            t := c.text;
            IF c.HasSelection() THEN
                c.GetSelection(beg, end); Do(t, StdLog.text, beg, defopt, error);
                IF error THEN DevMarkers.ShowFirstError(t, TextViews.focusOnly) END
            ELSE Dialog.ShowMsg("#Dev:NoSelectionFound")
            END
        ELSE Dialog.ShowMsg("#Dev:NoTextViewFound")
        END;
        Close
    END CompileSelection;

    PROCEDURE CompileList (beg, end: INTEGER; c: TextControllers.Controller);
        VAR v: Views.View; i: INTEGER; error, one: BOOLEAN; name: Files.Name; loc: Files.Locator;
            t: TextModels.Model; opts: SET; title, entry: ARRAY 64 OF CHAR;
    BEGIN
        s.SetPos(beg); s.Scan; one := FALSE;
        WHILE (s.start < end) & (s.type = TextMappers.string) & (s.len < LEN(name)) DO
            s.Scan; one := TRUE;
            WHILE (s.start < end) & (s.type = TextMappers.char) &
                ((s.char = "-") OR (s.char = "+") OR
                (s.char = "!") OR (s.char = "*") OR (s.char = "?") OR (s.char = "^") OR (s.char = "("))
            DO
                IF s.char = "(" THEN
                    WHILE (s.start < end) & ((s.type # TextMappers.char) OR (s.char # ")")) DO s.Scan END
                END;
                s.Scan
            END
        END;
        IF one & (s.start >= end) THEN
            s.SetPos(beg); s.Scan; error := FALSE;
            WHILE (s.start < end) & (s.type = TextMappers.string) & ~error DO
                i := 0; WHILE i < LEN(name) DO name[i] := 0X; INC(i) END;
                StdDialog.GetSubLoc(s.string, "Mod", loc, name);
                t := NIL;
                IF loc # NIL THEN
                    v := Views.OldView(loc, name);
                    IF v # NIL THEN
                        WITH v: TextViews.View DO t := v.ThisModel()
                        ELSE Dialog.ShowParamMsg("#Dev:NoTextFileFound", name, "", ""); error := TRUE
                        END
                    ELSE Dialog.ShowParamMsg("#Dev:CannotOpenFile", name, "", ""); error := TRUE
                    END
                ELSE Dialog.ShowParamMsg("#System:FileNotFound", name, "", ""); error := TRUE
                END;
                s.Scan; opts := defopt;
                WHILE (s.start < end) & (s.type = TextMappers.char) DO
                    IF s.char = "-" THEN
                        IF srcpos IN opts THEN EXCL(opts, srcpos)
                        ELSIF allref IN opts THEN EXCL(opts, allref)
                        ELSIF ref IN opts THEN EXCL(opts, ref)
                        ELSE EXCL(opts, obj)
                        END
                    ELSIF s.char = "!" THEN
                        IF assert IN opts THEN EXCL(opts, assert)
                        ELSE EXCL(opts, checks)
                        END
                    ELSIF s.char = "+" THEN INCL(opts, allchecks)
                    ELSIF s.char = "?" THEN INCL(opts, hint)
                    ELSIF s.char = "@" THEN INCL(opts, errorTrap)
                    ELSIF s.char = "$" THEN INCL(opts, oberon)
                    ELSIF s.char = "(" THEN
                        s.Scan;
                        WHILE (s.start < end) & (s.type = TextMappers.string) DO
                            title := s.string$; s.Scan;
                            IF (s.start < end) & (s.type = TextMappers.char) & (s.char = ":") THEN
                                s.Scan;
                                IF (s.start < end) & (s.type = TextMappers.string) THEN
                                    entry := s.string$; s.Scan;
                                    IF t # NIL THEN DevSelectors.ChangeTo(t, title, entry) END
                                END
                            END;
                            IF (s.start < end) & (s.type = TextMappers.char) & (s.char = ",") THEN s.Scan END
                        END
                    END;
                    s.Scan
                END;
                IF t # NIL THEN
                    Do(t, StdLog.text, 0, opts, error)
                END
            END
        ELSE Dialog.ShowMsg("#Dev:NotOnlyFileNames")
        END;
        s.ConnectTo(NIL);
        IF error & (c # NIL) & c.HasSelection() & (s.start < end) THEN
            c.SetSelection(s.start, end)
        END;
        IF error & (v # NIL) THEN
            Views.Open(v, loc, name, NIL);
            DevMarkers.ShowFirstError(t, TextViews.any)
        END
    END CompileList;

    PROCEDURE CompileModuleList*;
        VAR c: TextControllers.Controller; beg, end: INTEGER;
    BEGIN
        Open;
        c := TextControllers.Focus();
        IF c # NIL THEN
            s.ConnectTo(c.text);
            IF c.HasSelection() THEN c.GetSelection(beg, end)
            ELSE beg := 0; end := c.text.Length()
            END;
            CompileList(beg, end, c)
        ELSE Dialog.ShowMsg("#Dev:NoTextViewFound")
        END;
        Close
    END CompileModuleList;

    PROCEDURE CompileThis*;
        VAR p: DevCommanders.Par; beg, end: INTEGER;
    BEGIN
        Open;
        p := DevCommanders.par;
        IF p # NIL THEN
            DevCommanders.par := NIL;
            s.ConnectTo(p.text); beg := p.beg; end := p.end;
            CompileList(beg, end, NIL)
        ELSE Dialog.ShowMsg("#Dev:NoTextViewFound")
        END;
        Close
    END CompileThis;

    PROCEDURE Init;
        VAR loc: Files.Locator; f: Files.File;
    BEGIN
        loc := Files.dir.This("Dev"); loc := loc.This("Code");
        f := Files.dir.Old(loc, "ComDebug.ocf", TRUE);
        found := f # NIL;
        IF f # NIL THEN f.Close END
    END Init;

BEGIN
    Init
END DevCompiler.
