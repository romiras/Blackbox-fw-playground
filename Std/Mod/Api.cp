MODULE StdApi;
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
    
    IMPORT
        Kernel, Views, Files, Dialog, Converters, Windows, Sequencers, Stores, Meta,
        Containers, StdDialog, Documents;

    (* Auxiliary procedures *)

    PROCEDURE CheckQualident (VAR str, mod, name: ARRAY OF CHAR);
        VAR i, j: INTEGER; ch: CHAR;
    BEGIN
        i := 0; 
        REPEAT
            ch := str[i]; mod[i] := ch; INC(i)
        UNTIL (i = LEN(str)) OR (i = LEN(mod)) OR (ch < "0") OR (ch > "9") & (CAP(ch) < "A") OR (CAP(ch) > "Z");
        IF ch = "." THEN
            mod[i - 1] := 0X; j := 0;
            REPEAT
                ch := str[i]; name[j] := ch; INC(i); INC(j)
            UNTIL (i = LEN(str)) OR (j = LEN(name)) OR (ch < "0") OR (ch > "9") & (CAP(ch) < "A") OR (CAP(ch) > "Z");
            IF ch # 0X THEN mod[0] := 0X; name[0] := 0X END
        ELSE mod[0] := 0X; name[0] := 0X
        END
    END CheckQualident;
    
    PROCEDURE PathToSpec (VAR path: ARRAY OF CHAR; VAR loc: Files.Locator; VAR name: Files.Name);
        VAR i, j: INTEGER; ch: CHAR;
    BEGIN
        i := 0; j := 0; loc := Files.dir.This("");
        WHILE (loc.res = 0) & (i < LEN(path) - 1) & (j < LEN(name) - 1) & (path[i] # 0X) DO
            ch := path[i]; INC(i);
            IF (j > 0) & ((ch = "/") OR (ch = "\")) THEN
                name[j] := 0X; j := 0;
                IF name = "*" THEN
                    IF Dialog.language # "" THEN loc := loc.This(Dialog.language) END
                ELSE loc := loc.This(name)
                END
            ELSE
                name[j] := ch; INC(j)
            END
        END;
        IF path[i] = 0X THEN name[j] := 0X
        ELSE loc.res := 1; name := ""
        END
    END PathToSpec;
    
    PROCEDURE ThisDialog (dialog: ARRAY OF CHAR): Views.View;
        VAR fname, submod, sub, mod: Files.Name; canCreate: BOOLEAN; conv: Converters.Converter;
            loc: Files.Locator; file: Files.File; v: Views.View; s: Stores.Store; var: Meta.Item;
    BEGIN
        ASSERT(dialog # "", 20);
        v := NIL; file := NIL; canCreate := FALSE;
        CheckQualident(dialog, submod, fname);
        IF submod # "" THEN    (* is qualident *)
            Meta.LookupPath(dialog, var);
            IF var.obj = Meta.varObj THEN    (* variable exists *)
                canCreate := TRUE;
                Kernel.SplitName(submod, sub, mod);
                loc := Files.dir.This(sub);
                IF loc # NIL THEN
                    Kernel.MakeFileName(fname, "");
                    loc := loc.This("Rsrc");
                    IF loc # NIL THEN file := Files.dir.Old(loc, fname, Files.shared) END;
                    IF (file = NIL) & (sub = "") THEN
                        loc := Files.dir.This("System"); ASSERT(loc # NIL, 100);
                        IF loc # NIL THEN
                            loc := loc.This("Rsrc");
                            IF loc # NIL THEN file := Files.dir.Old(loc, fname, Files.shared) END
                        END
                    END
                END
            END
        END;
        IF (file = NIL) & ~canCreate THEN    (* try file name *)
            PathToSpec(dialog, loc, fname);
            IF loc.res = 0 THEN
                Kernel.MakeFileName(fname, "");
                file := Files.dir.Old(loc, fname, Files.shared)
            END
        END;
        IF file # NIL THEN
            Kernel.MakeFileName(fname, "");
            conv := NIL; Converters.Import(loc, fname, conv, s);
            IF s # NIL THEN
                v := s(Views.View)
            END
        ELSE Dialog.ShowParamMsg("#System:FileNotFound", dialog, "", "")
        END;
        RETURN v
    END ThisDialog;

    PROCEDURE ThisMask (param: ARRAY OF CHAR): Views.View;
        VAR v: Views.View; c: Containers.Controller;
    BEGIN
        v := ThisDialog(param);
        IF v # NIL THEN
            WITH v: Containers.View DO
                c := v.ThisController();
                IF c # NIL THEN
                    c.SetOpts(c.opts - {Containers.noFocus} + {Containers.noCaret, Containers.noSelection})
                ELSE Dialog.ShowMsg("#System:NotEditable")
                END
            ELSE Dialog.ShowMsg("#System:ContainerExpected")
            END
        END;
        RETURN v
    END ThisMask;
    
    (* Interface procedures *)
    
    PROCEDURE CloseDialog* (OUT closedView: Views.View);
        CONST canClose = {Windows.neverDirty, Windows.isTool, Windows.isAux};
        VAR w: Windows.Window; msg: Sequencers.CloseMsg;
    BEGIN
        closedView := NIL;
        w := Windows.dir.First();
        IF w # NIL THEN
            IF w.sub THEN
                closedView := w.frame.view;
                Windows.dir.Close(w);
            ELSIF (w.flags * canClose = {}) & w.seq.Dirty() THEN
                Dialog.ShowMsg("#System:CannotCloseDirtyWindow")
            ELSE
                msg.sticky := FALSE; w.seq.Notify(msg);
                IF ~msg.sticky THEN closedView := w.frame.view; Windows.dir.Close(w) END
            END
        END
    END CloseDialog;
    
    PROCEDURE OpenAux* (file, title: ARRAY OF CHAR; OUT v: Views.View);
        VAR loc: Files.Locator; name: Files.Name; t: Views.Title;
    BEGIN
        PathToSpec(file, loc, name);
        IF loc.res = 0 THEN
            loc.res := 77; v := Views.OldView(loc, name); loc.res := 0;
            IF v # NIL THEN t := title$; Views.OpenAux(v, t)
            ELSE Dialog.ShowParamMsg("#System:FileNotFound", file, "", "")
            END
        ELSE Dialog.ShowParamMsg("#System:FileNotFound", file, "", "")
        END
    END OpenAux;
    
    PROCEDURE OpenAuxDialog* (file, title: ARRAY OF CHAR; OUT v: Views.View);
        VAR t0: Views.Title; done: BOOLEAN;
    BEGIN
        Dialog.MapString(title, t0);
        Windows.SelectByTitle(NIL, {Windows.isAux}, t0, done);
        IF ~done THEN
            v := ThisMask(file);
            IF v # NIL THEN
                StdDialog.Open(v, title, NIL, "", NIL, FALSE, TRUE, TRUE, FALSE, TRUE)
            END
        END
    END OpenAuxDialog;
    
    PROCEDURE OpenBrowser* (file, title: ARRAY OF CHAR; OUT v: Views.View);
        VAR loc: Files.Locator; name: Files.Name; t: Views.Title;
            c: Containers.Controller;
    BEGIN
        PathToSpec(file, loc, name);
        IF loc.res = 0 THEN
            loc.res := 77; v := Views.OldView(loc, name); loc.res := 0;
            IF v # NIL THEN
                WITH v: Containers.View DO
                    c := v.ThisController();
                    IF c # NIL THEN
                        c.SetOpts(c.opts - {Containers.noFocus, Containers.noSelection} + {Containers.noCaret})
                    END
                ELSE
                END;
                t := title$;
                StdDialog.Open(v, t, NIL, "", NIL, FALSE, TRUE, FALSE, TRUE, FALSE)
            ELSE Dialog.ShowParamMsg("#System:FileNotFound", file, "", "")
            END
        ELSE Dialog.ShowParamMsg("#System:FileNotFound", file, "", "")
        END
    END OpenBrowser;
    
    PROCEDURE OpenDoc* (file: ARRAY OF CHAR; OUT v: Views.View);
        VAR loc: Files.Locator; name: Files.Name; conv: Converters.Converter;
    BEGIN
        PathToSpec(file, loc, name);
        IF loc.res = 0 THEN
            conv := NIL; v := Views.Old(Views.dontAsk, loc, name, conv);
            IF loc.res = 78 THEN loc := NIL; name := "" END;    (* stationery *)
            IF v # NIL THEN Views.Open(v, loc, name, conv)
            ELSE Dialog.ShowParamMsg("#System:FileNotFound", file, "", "")
            END
        ELSE Dialog.ShowParamMsg("#System:FileNotFound", file, "", "")
        END
    END OpenDoc;
    
    PROCEDURE OpenCopyOf* (file: ARRAY OF CHAR; OUT v: Views.View);
        VAR loc: Files.Locator; name: Files.Name; conv: Converters.Converter;
    BEGIN
        PathToSpec(file, loc, name);
        IF loc.res = 0 THEN
            conv := NIL; v := Views.Old(Views.dontAsk, loc, name, conv);
            IF loc.res = 78 THEN loc := NIL; name := "" END;    (* stationary *)
            IF v # NIL THEN 
                IF v.context # NIL THEN
                    v := Views.CopyOf(v.context(Documents.Context).ThisDoc(), Views.deep);
                    Stores.InitDomain(v)
                ELSE v := Views.CopyOf(v, Views.deep)
                END;
                Views.Open(v, NIL, "", conv)
            ELSE Dialog.ShowParamMsg("#System:FileNotFound", file, "", "")
            END
        ELSE Dialog.ShowParamMsg("#System:FileNotFound", file, "", "")
        END
    END OpenCopyOf;
    
    PROCEDURE OpenToolDialog* (file, title: ARRAY OF CHAR; OUT v: Views.View);
        VAR t0: Views.Title; done: BOOLEAN;
    BEGIN
        Dialog.MapString(title, t0);
        Windows.SelectByTitle(NIL, {Windows.isTool}, t0, done);
        IF ~done THEN
            v := ThisMask(file);
            IF v # NIL THEN
                StdDialog.Open(v, title, NIL, "", NIL, TRUE, FALSE, TRUE, FALSE, TRUE)
            END
        END
    END OpenToolDialog;
    
END StdApi.
