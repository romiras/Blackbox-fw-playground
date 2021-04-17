MODULE FwSto;

	(* text file format:

	text = 0					  CHAR
			textoffset			INTEGER (> 0)
			{ run }
			-1					CHAR
			{ char }

	run = attrno				 BYTE (0..32)
			[ attr ]			    attr.Internalize
			( piece | lpiece | viewref )

	piece = length			   INTEGER (> 0)

	lpiece = -length		     INTEGER (< 0, length MOD 2 = 0)

	viewref = 0				   INTEGER
			w					  INTEGER
			h					   INTEGER
			view				   view.Internalize
	*)

	IMPORT SYSTEM, Kernel, OSA, Strings, Files, Dialog;

	CONST
		(** Alien.cause, Reader.TurnIntoAlien cause - flagged by internalization procs **)
		alienVersion* = 1; alienComponent* = 2;
		(** Alien.cause - internally detected **)
		inconsistentVersion* = -1; inconsistentType* = -2;
		moduleFileNotFound* = -3; invalidModuleFile* = -4;
		inconsModuleVersion* = -5; typeNotFound* = -6;

		dictLineLen = 32;	(* length of type & elem dict lines *)

		newBase = 0F0X;	(* new base type (level = 0), i.e. not yet in dict *)
		newExt = 0F1X;	(* new extension type (level = 1), i.e. not yet in dict *)
		oldType = 0F2X;	(* old type, i.e. already in dict *)

		nil = 080X;	(* nil store *)
		link = 081X;	(* link to another elem in same file *)
		store = 082X;	(* general store *)
		elem = 083X;	(* elem store *)
		newlink = 084X;	(* link to another non-elem store in same file *)

		minVersion = 0; maxStoreVersion = 0; maxVersion = 0;

		elemTName = "Stores.ElemDesc";		(* type of pre-1.3 elems *)
		modelTName = "Models.ModelDesc";	(* the only known family of pre-1.3 elems *)

	TYPE
		TypeName* = OSA.TypeName;
		TypePath* = OSA.TypePath;
		OpName* = ARRAY 32 OF CHAR;

		FilesReader* = POINTER TO ABSTRACT RECORD
			eof*: BOOLEAN;
		END;

		StoresStore* = POINTER TO ABSTRACT RECORD
			(*
			dlink: Domain;
			*)
			era, id: INTEGER;	(* externalization era and id *)
			isElem: BOOLEAN	(* to preserve file format: is this an elem in the old sense? *)
		END;

		ModelsModel* = POINTER TO ABSTRACT RECORD (StoresStore) END;

		ModelsContext* = POINTER TO ABSTRACT RECORD END;

		ContainersModel* = POINTER TO ABSTRACT RECORD (ModelsModel) END;

		StoresReaderState = RECORD
			next: INTEGER;	(* position of next store in current level *)
			end: INTEGER	(* position just after last read store *)
		END;

		StoresWriterState = RECORD
			linkpos: INTEGER	(* address of threading link *)
		END;

		StoresTypeDict = POINTER TO RECORD
			next: StoresTypeDict;
			org: INTEGER;	(* origin id of this dict line *)
			type: ARRAY dictLineLen OF TypeName;	(* type[org] .. type[org + dictLineLen - 1] *)
			baseId: ARRAY dictLineLen OF INTEGER
		END;

		StoresStoreDict = POINTER TO RECORD
			next: StoresStoreDict;
			org: INTEGER;	(* origin id of this dict line *)
			elem: ARRAY dictLineLen OF StoresStore	(* elem[org] .. elem[org + dictLineLen - 1] *)
		END;

		StoresAlienComp* = POINTER TO LIMITED RECORD
			next-: StoresAlienComp
		END;

		StoresAlienPiece* = POINTER TO LIMITED RECORD (StoresAlienComp)
			pos-, len-: INTEGER
		END;

		StoresAlienPart* = POINTER TO LIMITED RECORD (StoresAlienComp)
			store-: StoresStore
		END;

		StoresAlien* = POINTER TO LIMITED RECORD (StoresStore)
			path-: TypePath;	(** the type this store would have if it were not an alien **)
			cause-: INTEGER;	(** # 0, the cause that turned this store into an alien **)
			file-: Files.File;	(** base file holding alien pieces **)
			comps-: StoresAlienComp	(** the constituent components of this alien store **)
		END;

		StoresReader* = RECORD
			rider-: Files.Reader;
			cancelled-: BOOLEAN;	(** current Internalize has been cancelled **)
			readAlien-: BOOLEAN;	(** at least one alien read since ConnectTo **)
			cause: INTEGER;
			nextTypeId, nextElemId, nextStoreId: INTEGER;	(* next id of non-dict type, "elem", store *)
			tDict, tHead: StoresTypeDict;	(* mapping (id <-> type) - self-organizing list *)
			eDict, eHead: StoresStoreDict;	(* mapping (id -> elem) - self-organizing list *)
			sDict, sHead: StoresStoreDict;	(* mapping (id -> store) - self-organizing list *)
			st: StoresReaderState;
			noDomain: BOOLEAN;
			store: StoresStore
		END;

(*
		Writer* = RECORD
			rider-: StoresFilesWriter;
			writtenStore-: StoresStore;
			era: INTEGER;	(* current externalization era *)
			noDomain: BOOLEAN;	(* no domain encountered yet *)
			modelType: OSA.Type;
			domain: StoresDomain;	(* domain of current era *)
			nextTypeId, nextElemId, nextStoreId: INTEGER;	(* next id of non-dict type or elem *)
			tDict, tHead: StoresTypeDict;	(* mapping (id -> type) - self-organizing list *)
			st: StoresWriterState
		END;
*)


	VAR
		nextEra: INTEGER;	(* next externalization era *)
		thisTypeRes: INTEGER;	(* side-effect res code of ThisType *)
		logReports: BOOLEAN;


	PROCEDURE (r: FilesReader) Base* (): Files.File, NEW, ABSTRACT;
	PROCEDURE (r: FilesReader) Pos* (): INTEGER, NEW, ABSTRACT;
	PROCEDURE (r: FilesReader) SetPos* (pos: INTEGER), NEW, ABSTRACT;
	PROCEDURE (r: FilesReader) ReadByte* (OUT x: BYTE), NEW, ABSTRACT;
	PROCEDURE (r: FilesReader) ReadBytes* (VAR x: ARRAY OF BYTE; beg, len: INTEGER), NEW, ABSTRACT;

	PROCEDURE^ Report* (IN msg, p0, p1, p2: ARRAY OF CHAR);

	PROCEDURE^ (VAR rd: StoresReader) SetPos* (pos: INTEGER), NEW;
	PROCEDURE^ (VAR rd: StoresReader) ReadVersion* (min, max: INTEGER; OUT version: INTEGER), NEW;
	PROCEDURE^ (VAR rd: StoresReader) ReadSChar* (OUT x: SHORTCHAR), NEW;
	PROCEDURE^ (VAR rd: StoresReader) ReadInt* (OUT x: INTEGER), NEW;
	PROCEDURE^ (VAR rd: StoresReader) ReadXString* (OUT x: ARRAY OF CHAR), NEW;
	PROCEDURE^ (VAR rd: StoresReader) ReadStore* (OUT x: StoresStore), NEW;

	(* type dictionary *)

	PROCEDURE GetThisType (VAR d: StoresTypeDict; id: INTEGER; VAR type: TypeName);
	(* pre: (id, t) IN dict *)
		VAR h, p: StoresTypeDict; org, k: INTEGER;
	BEGIN
		k := id MOD dictLineLen; org := id - k;
		h := NIL; p := d; WHILE p.org # org DO h := p; p := p.next END;
		IF h # NIL THEN h.next := p.next; p.next := d; d := p END;
		type := p.type[k]$;
		ASSERT(type # "", 100)
	END GetThisType;

	PROCEDURE ThisId (VAR d: StoresTypeDict; t: TypeName): INTEGER;
	(* pre: t # "" *)
	(* post: res = id if (t, id) in dict, res = -1 else *)
		VAR h, p: StoresTypeDict; k, id: INTEGER;
	BEGIN
		h := NIL; p := d; id := -1;
		WHILE (p # NIL) & (id < 0) DO
			k := 0; WHILE (k < dictLineLen) & (p.type[k, 0] # 0X) & (p.type[k] # t) DO INC(k) END;
			IF (k < dictLineLen) & (p.type[k, 0] # 0X) THEN id := p.org + k
			ELSE h := p; p := p.next
			END
		END;
		IF (id >= 0) & (h # NIL) THEN h.next := p.next; p.next := d; d := p END;
		RETURN id
	END ThisId;

	PROCEDURE ThisBaseId (VAR d: StoresTypeDict; id: INTEGER): INTEGER;
	(* post: res = id if base(t) # NIL, res = -1 if base(t) = NIL; res >= 0 => T(res) = base(t) *)
		VAR h, p: StoresTypeDict; k, org, baseId: INTEGER;
	BEGIN
		k := id MOD dictLineLen; org := id - k;
		h := NIL; p := d; WHILE p.org # org DO h := p; p := p.next END;
		IF h # NIL THEN h.next := p.next; p.next := d; d := p END;
		baseId := p.baseId[k];
		RETURN baseId
	END ThisBaseId;

	PROCEDURE AddType (VAR d, h: StoresTypeDict; id: INTEGER; type: TypeName);
		VAR k: INTEGER;
	BEGIN
		k := id MOD dictLineLen;
		IF (h = NIL) OR ((k = 0) & (h.org # id)) THEN
			NEW(h); h.org := id - k; h.next := d; d := h
		END;
		h.type[k] := type$; h.baseId[k] := -1
	END AddType;

	PROCEDURE AddBaseId (h: StoresTypeDict; id, baseId: INTEGER);
		VAR k: INTEGER;
	BEGIN
		k := id MOD dictLineLen;
		h.baseId[k] := baseId
	END AddBaseId;

	PROCEDURE InitTypeDict (VAR d, h: StoresTypeDict; VAR nextID: INTEGER);
	BEGIN
		d := NIL; h := NIL; nextID := 0
	END InitTypeDict;


	(* store dictionary - used to maintain referential sharing *)

	PROCEDURE ThisStore (VAR d: StoresStoreDict; id: INTEGER): StoresStore;
	(* pre: (id, s) IN dict *)
		VAR h, p: StoresStoreDict; s: StoresStore; k, org: INTEGER;
	BEGIN
		k := id MOD dictLineLen; org := id - k;
		h := NIL; p := d; WHILE p.org # org DO h := p; p := p.next END;
		IF h # NIL THEN h.next := p.next; p.next := d; d := p END;
		s := p.elem[k];
		ASSERT(s # NIL, 100);
		RETURN s
	END ThisStore;

	PROCEDURE AddStore (VAR d, h: StoresStoreDict; s: StoresStore);
		VAR k: INTEGER;
	BEGIN
		k := s.id MOD dictLineLen;
		IF (h = NIL) OR ((k = 0) & (h.org # s.id)) THEN
			NEW(h); h.org := s.id - k; h.next := d; d := h
		END;
		h.elem[k] := s
	END AddStore;

	PROCEDURE InitStoreDict (VAR d, h: StoresStoreDict; VAR nextID: INTEGER);
	BEGIN
		d := NIL; h := NIL; nextID := 0
	END InitStoreDict;


	PROCEDURE Join* (s0, s1: StoresStore);
		(*VAR d0, d1: Domain;*)
	BEGIN
(*
		ASSERT(s0 # NIL, 20); ASSERT(s1 # NIL, 21);
		d0 := DomainOf(s0); d1 := DomainOf(s1);
		IF (d0 = NIL) & (d1 = NIL) THEN
			s0.dlink := NewDomain(anonymousDomain); s1.dlink := s0.dlink
		ELSIF d0 = NIL THEN
			s0.dlink := d1; d1.copyDomain := FALSE
		ELSIF d1 = NIL THEN
			s1.dlink := d0; d0.copyDomain := FALSE
		ELSIF d0 # d1 THEN
			ASSERT(~d0.initialized OR ~d1.initialized, 22);
				(* PRE 22	s0.Domain() = NIL OR s1.Domain() = NIL OR s0.Domain() = s1.Domain() *)
			IF ~d0.initialized & (d0.level = 0) THEN d0.dlink := d1; d1.copyDomain := FALSE
			ELSIF ~d1.initialized & (d1.level = 0) THEN d1.dlink := d0; d0.copyDomain := FALSE
			ELSE HALT(100)
			END
		END
*)
	END Join;


	(* support for alien mapping *)

	PROCEDURE InternalizeAlien (VAR rd: StoresReader; VAR comps: StoresAlienComp; down, pos, len: INTEGER);
		VAR h, p: StoresAlienComp; piece: StoresAlienPiece; part: StoresAlienPart; file: Files.File;
			next, end, max: INTEGER;
	BEGIN
		file := rd.rider.Base(); max := file.Length();
		end := pos + len; h := NIL;
		IF down # 0 THEN next := down ELSE next := end END;
		WHILE pos < end DO
			ASSERT(end <= max, 100);
			IF pos < next THEN
				NEW(piece); piece.pos := pos; piece.len := next - pos;
				p := piece; pos := next
			ELSE
				ASSERT(pos = next, 101);
				rd.SetPos(next);
				NEW(part); rd.ReadStore(part.store);
				ASSERT(rd.st.end > next, 102);
				p := part; pos := rd.st.end;
				IF rd.st.next > 0 THEN
					ASSERT(rd.st.next > next, 103); next := rd.st.next
				ELSE next := end
				END
			END;
			IF h = NIL THEN comps := p ELSE h.next := p END;
			h := p
		END;
		ASSERT(pos = end, 104);
		rd.SetPos(end)
	END InternalizeAlien;

(*
	PROCEDURE ExternalizePiece (VAR wr: Writer; file: Files.File; p: AlienPiece);
		VAR r: Files.Reader; w: Files.Writer; b: BYTE; l, len: INTEGER;
	BEGIN
		l := file.Length(); len := p.len;
		ASSERT(0 <= p.pos, 100); ASSERT(p.pos <= l, 101);
		ASSERT(0 <= len, 102); ASSERT(len <= l - p.pos, 103);
		r := file.NewReader(NIL); r.SetPos(p.pos);
		w := wr.rider;
		WHILE len # 0 DO r.ReadByte(b); w.WriteByte(b); DEC(len) END
	END ExternalizePiece;

	PROCEDURE ExternalizeAlien (VAR wr: Writer; file: Files.File; comps: StoresAlienComp);
		VAR p: StoresAlienComp;
	BEGIN
		p := comps;
		WHILE p # NIL DO
			WITH p: AlienPiece DO
				ExternalizePiece(wr, file, p)
			| p: AlienPart DO
				wr.WriteStore(p.store)
			END;
			p := p.next
		END
	END ExternalizeAlien;
	*)


	PROCEDURE (s: StoresStore) Internalize- (VAR rd: StoresReader), NEW, EXTENSIBLE;
		VAR thisVersion: INTEGER;
	BEGIN
		rd.ReadVersion(minVersion, maxStoreVersion, thisVersion);
		IF ~rd.cancelled & s.isElem THEN
			rd.ReadVersion(minVersion, maxStoreVersion, thisVersion)
			(* works since maxStoreVersion = maxElemVersion = 0 in pre-1.3 *)
		END
	END Internalize;


	PROCEDURE (VAR rd: StoresReader) ConnectTo* (f: Files.File), NEW;
	(** pre: rd.rider = NIL  OR  f = NIL **)
	BEGIN
		IF f = NIL THEN
			rd.rider := NIL
		ELSE
			ASSERT(rd.rider = NIL, 20);
			rd.rider := f.NewReader(rd.rider); rd.SetPos(0);
			InitTypeDict(rd.tDict, rd.tHead, rd.nextTypeId);
			InitStoreDict(rd.eDict, rd.eHead, rd.nextElemId);
			InitStoreDict(rd.sDict, rd.sHead, rd.nextStoreId);
			rd.noDomain := TRUE
		END;
		rd.readAlien := FALSE
	END ConnectTo;

	PROCEDURE (VAR rd: StoresReader) SetPos* (pos: INTEGER), NEW;
	BEGIN
		rd.rider.SetPos(pos)
	END SetPos;

	PROCEDURE (VAR rd: StoresReader) Pos* (): INTEGER, NEW;
	BEGIN
		RETURN rd.rider.Pos()
	END Pos;

	PROCEDURE (VAR rd: StoresReader) ReadBool* (OUT x: BOOLEAN), NEW;
		VAR b: BYTE;
	BEGIN
		rd.rider.ReadByte(b); x := b # 0
	END ReadBool;

	PROCEDURE (VAR rd: StoresReader) ReadSChar* (OUT x: SHORTCHAR), NEW;
	BEGIN
		rd.rider.ReadByte(SYSTEM.VAL(BYTE, x))
	END ReadSChar;

	PROCEDURE (VAR rd: StoresReader) ReadXChar* (OUT x: CHAR), NEW;
		VAR c: SHORTCHAR;
	BEGIN
		rd.rider.ReadByte(SYSTEM.VAL(BYTE,c)); x := c
	END ReadXChar;

	PROCEDURE (VAR rd: StoresReader) ReadChar* (OUT x: CHAR), NEW;
		VAR le: ARRAY 2 OF BYTE;	(* little endian, big endian *)
	BEGIN
		rd.rider.ReadBytes(le, 0, 2);
		x := CHR(le[0] MOD 256 + (le[1] MOD 256) * 256)
	END ReadChar;

	PROCEDURE (VAR rd: StoresReader) ReadByte* (OUT x: BYTE), NEW;
	BEGIN
		rd.rider.ReadByte(x)
	END ReadByte;

	PROCEDURE (VAR rd: StoresReader) ReadSInt* (OUT x: SHORTINT), NEW;
		VAR le, be: ARRAY 2 OF BYTE;	(* little endian, big endian *)
	BEGIN
		rd.rider.ReadBytes(le, 0, 2);
		IF Kernel.littleEndian THEN
			x := SYSTEM.VAL(SHORTINT, le)
		ELSE
			be[0] := le[1]; be[1] := le[0];
			x := SYSTEM.VAL(SHORTINT, be)
		END
	END ReadSInt;

	PROCEDURE (VAR rd: StoresReader) ReadXInt* (OUT x: INTEGER), NEW;
		VAR le, be: ARRAY 2 OF BYTE;	(* little endian, big endian *)
	BEGIN
		rd.rider.ReadBytes(le, 0, 2);
		IF Kernel.littleEndian THEN
			x := SYSTEM.VAL(SHORTINT, le)
		ELSE
			be[0] := le[1]; be[1] := le[0];
			x := SYSTEM.VAL(SHORTINT, be)
		END
	END ReadXInt;

	PROCEDURE (VAR rd: StoresReader) ReadInt* (OUT x: INTEGER), NEW;
		VAR le, be: ARRAY 4 OF BYTE;	(* little endian, big endian *)
	BEGIN
		rd.rider.ReadBytes(le, 0, 4);
		IF Kernel.littleEndian THEN
			x := SYSTEM.VAL(INTEGER, le)
		ELSE
			be[0] := le[3]; be[1] := le[2]; be[2] := le[1]; be[3] := le[0];
			x := SYSTEM.VAL(INTEGER, be)
		END
	END ReadInt;

	PROCEDURE (VAR rd: StoresReader) ReadLong* (OUT x: LONGINT), NEW;
		VAR le, be: ARRAY 8 OF BYTE;	(* little endian, big endian *)
	BEGIN
		rd.rider.ReadBytes(le, 0, 8);
		IF Kernel.littleEndian THEN
			x := SYSTEM.VAL(LONGINT, le)
		ELSE
			be[0] := le[7]; be[1] := le[6]; be[2] := le[5]; be[3] := le[4];
			be[4] := le[3]; be[5] := le[2]; be[6] := le[1]; be[7] := le[0];
			x := SYSTEM.VAL(LONGINT, be)
		END
	END ReadLong;

	PROCEDURE (VAR rd: StoresReader) ReadSReal* (OUT x: SHORTREAL), NEW;
		VAR le, be: ARRAY 4 OF BYTE;	(* little endian, big endian *)
	BEGIN
		rd.rider.ReadBytes(le, 0, 4);
		IF Kernel.littleEndian THEN
			x := SYSTEM.VAL(SHORTREAL, le)
		ELSE
			be[0] := le[3]; be[1] := le[2]; be[2] := le[1]; be[3] := le[0];
			x := SYSTEM.VAL(SHORTREAL, be)
		END
	END ReadSReal;

	PROCEDURE (VAR rd: StoresReader) ReadXReal* (OUT x: REAL), NEW;
		VAR le, be: ARRAY 4 OF BYTE;	(* little endian, big endian *)
	BEGIN
		rd.rider.ReadBytes(le, 0, 4);
		IF Kernel.littleEndian THEN
			x := SYSTEM.VAL(SHORTREAL, le)
		ELSE
			be[0] := le[3]; be[1] := le[2]; be[2] := le[1]; be[3] := le[0];
			x := SYSTEM.VAL(SHORTREAL, be)
		END
	END ReadXReal;

	PROCEDURE (VAR rd: StoresReader) ReadReal* (OUT x: REAL), NEW;
		VAR le, be: ARRAY 8 OF BYTE;	(* little endian, big endian *)
	BEGIN
		rd.rider.ReadBytes(le, 0, 8);
		IF Kernel.littleEndian THEN
			x := SYSTEM.VAL(REAL, le)
		ELSE
			be[0] := le[7]; be[1] := le[6]; be[2] := le[5]; be[3] := le[4];
			be[4] := le[3]; be[5] := le[2]; be[6] := le[1]; be[7] := le[0];
			x := SYSTEM.VAL(REAL, be)
		END
	END ReadReal;

	PROCEDURE (VAR rd: StoresReader) ReadSet* (OUT x: SET), NEW;
		VAR le, be: ARRAY 4 OF BYTE;	(* little endian, big endian *)
	BEGIN
		rd.rider.ReadBytes(le, 0, 4);
		IF Kernel.littleEndian THEN
			x := SYSTEM.VAL(SET, le)
		ELSE
			be[0] := le[3]; be[1] := le[2]; be[2] := le[1]; be[3] := le[0];
			x := SYSTEM.VAL(SET, be)
		END
	END ReadSet;

	PROCEDURE (VAR rd: StoresReader) ReadSString* (OUT x: ARRAY OF SHORTCHAR), NEW;
		VAR i: INTEGER; ch: SHORTCHAR;
	BEGIN
		i := 0; REPEAT rd.ReadSChar(ch); x[i] := ch; INC(i) UNTIL ch = 0X
	END ReadSString;

	PROCEDURE (VAR rd: StoresReader) ReadXString* (OUT x: ARRAY OF CHAR), NEW;
		VAR i: INTEGER; ch: CHAR;
	BEGIN
		i := 0; REPEAT rd.ReadXChar(ch); x[i] := ch; INC(i) UNTIL ch = 0X
	END ReadXString;

	PROCEDURE (VAR rd: StoresReader) ReadString* (OUT x: ARRAY OF CHAR), NEW;
		VAR i: INTEGER; ch: CHAR;
	BEGIN
		i := 0; REPEAT rd.ReadChar(ch); x[i] := ch; INC(i) UNTIL ch = 0X
	END ReadString;

	PROCEDURE AlienReport (cause: INTEGER);
		VAR s, e: ARRAY 32 OF CHAR;
	BEGIN
		CASE cause OF
		| alienVersion: s := "#System:AlienVersion"
		| alienComponent: s := "#System:AlienComponent"
		| inconsistentVersion: s := "#System:InconsistentVersion"
		ELSE s := "#System:UnknownCause"
		END;
		Strings.IntToString(cause, e);
		Report("#System:AlienCause ^0 ^1 ^2", s, e, "")
	END AlienReport;

	PROCEDURE AlienTypeReport (cause: INTEGER; t: ARRAY OF CHAR);
		VAR s: ARRAY 64 OF CHAR; 
	BEGIN
		CASE cause OF
		| inconsistentType: s := "#System:InconsistentType ^0"
		| moduleFileNotFound: s := "#System:CodeFileNotFound ^0"
		| invalidModuleFile: s := "#System:InvalidCodeFile ^0"
		| inconsModuleVersion: s := "#System:InconsistentModuleVersion ^0"
		| typeNotFound: s := "#System:TypeNotFound ^0"
		END;
		Report(s, t, "", "")
	END AlienTypeReport;


	(* types *)

	PROCEDURE NewStore (t: OSA.Type): StoresStore;
		VAR p: ANYPTR;
	BEGIN
		ASSERT(t # NIL, 20); p:=NIL;
		OSA.NewObj(p, t); ASSERT(p # NIL, 100);
		ASSERT(p IS StoresStore, 21);
		RETURN p(StoresStore)
	END NewStore;


	(* support for type mapping *)

	PROCEDURE ReadPath (VAR rd: StoresReader; VAR path: TypePath);
		VAR h: StoresTypeDict; id, extId: INTEGER; i: INTEGER; kind: SHORTCHAR;

		PROCEDURE AddPathComp (VAR rd: StoresReader);
		BEGIN
			IF h # NIL THEN AddBaseId(h, extId, rd.nextTypeId) END;
			AddType(rd.tDict, rd.tHead, rd.nextTypeId, path[i]);
			h := rd.tHead; extId := rd.nextTypeId
		END AddPathComp;

	BEGIN
		h := NIL; i := 0; rd.ReadSChar(kind); extId:=0;
		WHILE kind = newExt DO
			rd.ReadXString(path[i]);
			AddPathComp(rd); INC(rd.nextTypeId);
			IF path[i] # elemTName THEN INC(i) END;
			rd.ReadSChar(kind)
		END;
		IF kind = newBase THEN
			rd.ReadXString(path[i]);
			AddPathComp(rd); INC(rd.nextTypeId); INC(i)
		ELSE
			ASSERT(kind = oldType, 100);
			rd.ReadInt(id);
			IF h # NIL THEN AddBaseId(h, extId, id) END;
			REPEAT
				GetThisType(rd.tDict, id, path[i]); id := ThisBaseId(rd.tDict, id);
				IF path[i] # elemTName THEN INC(i) END
			UNTIL id = -1
		END;
		path[i] := ""
	END ReadPath;

	PROCEDURE (VAR rd: StoresReader) TurnIntoAlien* (cause: INTEGER), NEW;
	BEGIN
		ASSERT(cause > 0, 20);
		rd.cancelled := TRUE; rd.readAlien := TRUE; rd.cause := cause;
		AlienReport(cause)
	END TurnIntoAlien;

	PROCEDURE (VAR rd: StoresReader) ReadVersion* (min, max: INTEGER; OUT version: INTEGER), NEW;
		VAR v: BYTE;
	BEGIN
		rd.ReadByte(v); version := v;
		IF (version < min) OR (version > max) THEN
			rd.TurnIntoAlien(alienVersion)
		END
	END ReadVersion;

	PROCEDURE (VAR rd: StoresReader) ReadStore* (OUT x: StoresStore), NEW;
		VAR a: StoresAlien; t: OSA.Type;
			len, pos, pos1, id, comment, next, down, downPos, nextTypeId, nextElemId, nextStoreId: INTEGER;
			kind: SHORTCHAR; path: TypePath; type: TypeName;
			save: StoresReaderState;
	BEGIN
		x := NIL;
		rd.ReadSChar(kind);
		IF kind = nil THEN
			rd.ReadInt(comment); rd.ReadInt(next);
			rd.st.end := rd.Pos();
			IF (next > 0) OR ((next = 0) & ODD(comment)) THEN rd.st.next := rd.st.end + next ELSE rd.st.next := 0 END;
			x := NIL
		ELSIF kind = link THEN
			rd.ReadInt(id); rd.ReadInt(comment); rd.ReadInt(next);
			rd.st.end := rd.Pos();
			IF (next > 0) OR ((next = 0) & ODD(comment)) THEN rd.st.next := rd.st.end + next ELSE rd.st.next := 0 END;
			x := ThisStore(rd.eDict, id)
		ELSIF kind = newlink THEN
			rd.ReadInt(id); rd.ReadInt(comment); rd.ReadInt(next);
			rd.st.end := rd.Pos();
			IF (next > 0) OR ((next = 0) & ODD(comment)) THEN rd.st.next := rd.st.end + next ELSE rd.st.next := 0 END;
			x := ThisStore(rd.sDict, id)
		ELSIF (kind = store) OR (kind = elem) THEN
			IF kind = elem THEN
				id := rd.nextElemId; INC(rd.nextElemId)
			ELSE
				id := rd.nextStoreId; INC(rd.nextStoreId)
			END;
			ReadPath(rd, path); type := path[0];
			nextTypeId := rd.nextTypeId; nextElemId := rd.nextElemId; nextStoreId := rd.nextStoreId;
			rd.ReadInt(comment);
			pos1 := rd.Pos();
			rd.ReadInt(next); rd.ReadInt(down); rd.ReadInt(len);
			pos := rd.Pos();
			IF next > 0 THEN rd.st.next := pos1 + next + 4 ELSE rd.st.next := 0 END;
			IF down > 0 THEN downPos := pos1 + down + 8 ELSE downPos := 0 END;
			rd.st.end := pos + len;
			rd.cause := 0;
			ASSERT(len >= 0, 101);
			IF next # 0 THEN
				ASSERT(rd.st.next > pos1, 102);
				IF down # 0 THEN
					ASSERT(downPos < rd.st.next, 103)
				END
			END;
			IF down # 0 THEN
				ASSERT(downPos > pos1, 104);
				ASSERT(downPos < rd.st.end, 105)
			END;
			t := OSA.ThisType(type);
			IF t # NIL THEN
				x := NewStore(t); x.isElem := kind = elem
			ELSE
				rd.cause := thisTypeRes; AlienTypeReport(rd.cause, type);
				x := NIL
			END;
			IF x # NIL THEN
				IF OSA.SamePath(t, path) THEN
					IF kind = elem THEN
						x.id := id; AddStore(rd.eDict, rd.eHead, x)
					ELSE
						x.id := id; AddStore(rd.sDict, rd.sHead, x)
					END;
					save := rd.st; rd.cause := 0; rd.cancelled :=  FALSE;
					x.Internalize(rd);
					rd.st := save;
					IF rd.cause # 0 THEN x := NIL
					ELSIF (rd.Pos() # rd.st.end) OR rd.rider.eof THEN
						rd.cause := inconsistentVersion; AlienReport(rd.cause);
						x := NIL
					END
				ELSE
					rd.cause := inconsistentType; AlienTypeReport(rd.cause, type);
					x := NIL
				END
			END;
			
			IF x # NIL THEN
				IF rd.noDomain THEN
					rd.store := x;
					rd.noDomain := FALSE
				ELSE
					Join(rd.store, x)
				END
			ELSE	(* x is an alien *)
				rd.SetPos(pos);
				ASSERT(rd.cause # 0, 107);
				NEW(a); a.path := path; a.cause := rd.cause; a.file := rd.rider.Base();
				IF rd.noDomain THEN
					rd.store := a;
					rd.noDomain := FALSE
				ELSE
					Join(rd.store, a)
				END;
				IF kind = elem THEN
					a.id := id; AddStore(rd.eDict, rd.eHead, a)
				ELSE
					a.id := id; AddStore(rd.sDict, rd.sHead, a)
				END;
				save := rd.st;
				rd.nextTypeId := nextTypeId; rd.nextElemId := nextElemId; rd.nextStoreId := nextStoreId;
				InternalizeAlien(rd, a.comps, downPos, pos, len);
				rd.st := save;
				x := a;
				ASSERT(rd.Pos() = rd.st.end, 108);
				rd.cause := 0; rd.cancelled :=  FALSE; rd.readAlien := TRUE
			END
		ELSE
			pos := rd.Pos();
			HALT(20)
		END
	END ReadStore;

	(** miscellaneous **)

	PROCEDURE Report* (IN msg, p0, p1, p2: ARRAY OF CHAR);
	BEGIN
		IF logReports THEN
			Dialog.ShowParamMsg(msg, p0, p1, p2)
		END
	END Report;

	(** ModelsModel **)

	PROCEDURE (m: ModelsModel) Internalize- (VAR rd: StoresReader), EXTENSIBLE;
		VAR thisVersion: INTEGER;
	BEGIN
		m.Internalize^(rd);
		IF rd.cancelled THEN RETURN END;
		rd.ReadVersion(minVersion, maxVersion, thisVersion)
	END Internalize;

(*
	PROCEDURE (m: ModelsModel) Externalize- (VAR wr: StoresWriter), EXTENSIBLE;
	BEGIN
		m.Externalize^(wr);
		wr.WriteVersion(maxVersion)
	END Externalize;
*)

	(** ModelsContext **)

	PROCEDURE (c: ModelsContext) ThisModel* (): ModelsModel, NEW, ABSTRACT;
(*
	PROCEDURE (c: ModelsContext) Normalize* (): BOOLEAN, NEW, ABSTRACT;
	PROCEDURE (c: ModelsContext) GetSize* (OUT w, h: INTEGER), NEW, ABSTRACT;
	PROCEDURE (c: ModelsContext) SetSize* (w, h: INTEGER), NEW, EMPTY;
	PROCEDURE (c: ModelsContext) MakeVisible* (l, t, r, b: INTEGER), NEW, EMPTY;
	PROCEDURE (c: ModelsContext) Consider* (VAR p: Proposal), NEW, EMPTY;
*)

END FwSto.