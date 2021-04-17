MODULE OSA;

	(* Oberon/F System Abstraction layer *)

	IMPORT SYSTEM, Kernel;

	CONST
		(** Alien.cause - internally detected **)
		inconsistentVersion* = -1; inconsistentType* = -2;
		moduleFileNotFound* = -3; invalidModuleFile* = -4;
		inconsModuleVersion* = -5; typeNotFound* = -6;
	
	TYPE
		Type* = Kernel.Type;
		Name* = Kernel.Name;
		TrapCleaner* = Kernel.TrapCleaner;
	
		TypeName* = ARRAY 64 OF CHAR;
		TypePath* = ARRAY 16 OF TypeName;
	
	VAR
		thisTypeRes: INTEGER;	(* side-effect res code of ThisType *)

	(* types *)

	PROCEDURE TypeOf* (IN rec: ANYREC): Type;
	BEGIN
		RETURN Kernel.TypeOf(rec)
	END TypeOf;

	PROCEDURE LevelOf* (t: Type): SHORTINT;
	BEGIN
		RETURN Kernel.LevelOf(t)
	END LevelOf;

	PROCEDURE GetThisTypeName* (t: Type; OUT type: TypeName);
		VAR i, j: INTEGER; ch: CHAR; name: Name;
	BEGIN
		Kernel.GetTypeName(t, name); type := t.mod.name$;
		i := 0; ch := type[0]; WHILE ch # 0X DO INC(i); ch := type[i] END;
		type[i] := "."; INC(i);
		j := 0; REPEAT ch := name[j]; type[i] := ch; INC(i); INC(j) UNTIL ch = 0X
	END GetThisTypeName;

	PROCEDURE ThisType* (type: ARRAY OF CHAR): Type;
		VAR m: Kernel.Module; t: Type; i, j: INTEGER; ch: CHAR;
			typ: Name; mod: ARRAY 256 OF CHAR; res: INTEGER; str: ARRAY 256 OF CHAR;
	BEGIN
		ASSERT(type # "", 20);
		i := 0; ch := type[0]; t:=NIL;
		WHILE (ch # ".") & (ch # 0X) DO mod[i] := SHORT(ch); INC(i); ch := type[i] END;
		ASSERT(ch = ".", 21);
		mod[i] := 0X; INC(i);
		m := Kernel.ThisMod(mod);
		IF m # NIL THEN
			j := 0; REPEAT ch := type[i]; typ[j] := SHORT(ch); INC(i); INC(j) UNTIL ch = 0X;
			t := Kernel.ThisType(m, typ);
			IF (t = NIL) & (j >= 5) THEN	(* try pointer type *)
				IF (typ[j-5] = "D") & (typ[j-4] = "e") & (typ[j-3] = "s") & (typ[j-2] = "c") THEN
					typ[j-5] := "^"; typ[j-4] := 0X;
					t := Kernel.ThisType(m, typ)
				END
			END;
			IF t = NIL THEN thisTypeRes := typeNotFound END
		ELSE
			t := NIL;
			Kernel.GetLoaderResult(res, str, str, str);
			CASE res OF
			| Kernel.fileNotFound: thisTypeRes := moduleFileNotFound
			| Kernel.syntaxError: thisTypeRes := invalidModuleFile
			| Kernel.objNotFound: thisTypeRes := inconsModuleVersion
			| Kernel.illegalFPrint: thisTypeRes := inconsModuleVersion
			| Kernel.cyclicImport: thisTypeRes := invalidModuleFile	(* cyclic import ... *)
			ELSE thisTypeRes := invalidModuleFile
			END
		END;
		RETURN t
	END ThisType;

	PROCEDURE SameType* (IN x, y: ARRAY OF CHAR): BOOLEAN;
		VAR i: INTEGER;
	BEGIN
		IF x = y THEN RETURN TRUE
		ELSE
			i := 0; WHILE x[i] = y[i] DO INC(i) END;
			RETURN
				(x[i] = "^") & (x[i+1] = 0X) & (y[i] = "D") & (y[i+1] = "e") & (y[i+2] = "s") & (y[i+3] = "c") & (y[i+4] = 0X)
				OR (y[i] = "^") & (y[i+1] = 0X) & (x[i] = "D") & (x[i+1] = "e") & (x[i+2] = "s") & (x[i+3] = "c") & (x[i+4] = 0X)
		END
	END SameType;

	PROCEDURE SamePath* (t: Type; VAR path: TypePath): BOOLEAN;
	(* check whether t coincides with path *)
		VAR tn: TypeName; i, n: INTEGER;
	BEGIN
		i := -1; n := Kernel.LevelOf(t);
		REPEAT
			GetThisTypeName(t.base[n], tn);
			DEC(n); INC(i)
		UNTIL (n < 0) OR ~SameType(tn, path[i]);
		RETURN SameType(tn, path[i])
	END SamePath;
	
	PROCEDURE NewObj* (VAR o: SYSTEM.PTR; t: Type);
	BEGIN
		Kernel.NewObj(o, t)
	END NewObj;
	
	PROCEDURE GetTypePath* (VAR path: TypePath; t: Type);
		VAR n, i: INTEGER;
	BEGIN
		i := 0; n := Kernel.LevelOf(t);
		WHILE n >= 0 DO
			GetThisTypeName(t.base[n], path[i]);
			DEC(n); INC(i)
		END;
		path[i] := ""
	END GetTypePath;
	
END OSA.
