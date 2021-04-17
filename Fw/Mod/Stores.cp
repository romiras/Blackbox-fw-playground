MODULE FwStores;
(**
	project	= "BlackBox"
	organization	= "www.oberon.ch"
	contributors	= "Oberon microsystems, Fyodor Tkachov"
	version	= "System/Rsrc/About"
	copyright	= "System/Rsrc/About"
	license	= "Docu/BB-License"
	changes	= ""
	issues	= ""

**)

	IMPORT SYSTEM, OSA, Dialog, Strings, Files;

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

		minVersion = 0; maxStoreVersion = 0;

		elemTName = "Stores.ElemDesc";		(* type of pre-1.3 elems *)
		modelTName = "Models.ModelDesc";	(* the only known family of pre-1.3 elems *)
		
		inited = TRUE; anonymousDomain = FALSE;	(* values to be used when calling NewDomain *)
		
		compatible = TRUE;


	TYPE
		TypeName* = OSA.TypeName;
		TypePath* = OSA.TypePath;
		OpName* = ARRAY 32 OF CHAR;

		Domain* = POINTER TO LIMITED RECORD 
			sequencer: ANYPTR;
			dlink: Domain;
			initialized, copyDomain: BOOLEAN;
			level, copyera, nextElemId:  INTEGER;
			sDict: StoreDict;
			cleaner: TrapCleaner;
			s: Store	(* used for CopyOf *)
		END;

		Operation* = POINTER TO ABSTRACT RECORD END;

		Store* = POINTER TO ABSTRACT RECORD
			dlink: Domain;
			era, id: INTEGER;	(* externalization era and id *)
			isElem: BOOLEAN	(* to preserve file format: is this an elem in the old sense? *)
		END;


		AlienComp* = POINTER TO LIMITED RECORD
			next-: AlienComp
		END;

		AlienPiece* = POINTER TO LIMITED RECORD (AlienComp)
			pos-, len-: INTEGER
		END;

		AlienPart* = POINTER TO LIMITED RECORD (AlienComp)
			store-: Store
		END;

		Alien* = POINTER TO LIMITED RECORD (Store)
			path-: TypePath;	(** the type this store would have if it were not an alien **)
			cause-: INTEGER;	(** # 0, the cause that turned this store into an alien **)
			file-: Files.File;	(** base file holding alien pieces **)
			comps-: AlienComp	(** the constituent components of this alien store **)
		END;

		ReaderState = RECORD
			next: INTEGER;	(* position of next store in current level *)
			end: INTEGER	(* position just after last read store *)
		END;

		WriterState = RECORD
			linkpos: INTEGER	(* address of threading link *)
		END;

		TypeDict = POINTER TO RECORD
			next: TypeDict;
			org: INTEGER;	(* origin id of this dict line *)
			type: ARRAY dictLineLen OF TypeName;	(* type[org] .. type[org + dictLineLen - 1] *)
			baseId: ARRAY dictLineLen OF INTEGER
		END;

		StoreDict = POINTER TO RECORD
			next: StoreDict;
			org: INTEGER;	(* origin id of this dict line *)
			elem: ARRAY dictLineLen OF Store	(* elem[org] .. elem[org + dictLineLen - 1] *)
		END;

		Reader* = RECORD
			rider-: Files.Reader;
			cancelled-: BOOLEAN;	(** current Internalize has been cancelled **)
			readAlien-: BOOLEAN;	(** at least one alien read since ConnectTo **)
			cause: INTEGER;
			nextTypeId, nextElemId, nextStoreId: INTEGER;	(* next id of non-dict type, "elem", store *)
			tDict, tHead: TypeDict;	(* mapping (id <-> type) - self-organizing list *)
			eDict, eHead: StoreDict;	(* mapping (id -> elem) - self-organizing list *)
			sDict, sHead: StoreDict;	(* mapping (id -> store) - self-organizing list *)
			st: ReaderState;
			noDomain: BOOLEAN;
			store: Store
		END;

		Writer* = RECORD
			rider-: Files.Writer;
			writtenStore-: Store;
			era: INTEGER;	(* current externalization era *)
			noDomain: BOOLEAN;	(* no domain encountered yet *)
			modelType: OSA.Type;
			domain: Domain;	(* domain of current era *)
			nextTypeId, nextElemId, nextStoreId: INTEGER;	(* next id of non-dict type or elem *)
			tDict, tHead: TypeDict;	(* mapping (id -> type) - self-organizing list *)
			st: WriterState
		END;

		TrapCleaner = POINTER TO RECORD (OSA.TrapCleaner) 
			d: Domain
		END;

	VAR
		nextEra: INTEGER;	(* next externalization era *)
		thisTypeRes: INTEGER;	(* side-effect res code of ThisType *)
		logReports: BOOLEAN;


	(** Cleaner **)

	PROCEDURE (c: TrapCleaner) Cleanup;
	BEGIN
		c.d.level := 0;
		c.d.sDict := NIL;
		c.d.s := NIL
	END Cleanup;

	PROCEDURE (d: Domain) SetSequencer* (sequencer: ANYPTR), NEW;
	BEGIN
		ASSERT(d.sequencer = NIL);
		d.sequencer := sequencer
	END SetSequencer;
	
	PROCEDURE (d: Domain) GetSequencer*(): ANYPTR, NEW;
	BEGIN
		RETURN d.sequencer
	END GetSequencer;


	PROCEDURE^ Report* (IN msg, p0, p1, p2: ARRAY OF CHAR);
	
	PROCEDURE^ (VAR rd: Reader) SetPos* (pos: INTEGER), NEW;
	PROCEDURE^ (VAR rd: Reader) ReadVersion* (min, max: INTEGER; OUT version: INTEGER), NEW;
	PROCEDURE^ (VAR rd: Reader) ReadSChar* (OUT x: SHORTCHAR), NEW;
	PROCEDURE^ (VAR rd: Reader) ReadInt* (OUT x: INTEGER), NEW;
	PROCEDURE^ (VAR rd: Reader) ReadXString* (OUT x: ARRAY OF CHAR), NEW;
	PROCEDURE^ (VAR rd: Reader) ReadStore* (OUT x: Store), NEW;
	
	PROCEDURE^ (VAR wr: Writer) SetPos* (pos: INTEGER), NEW;
	PROCEDURE^ (VAR wr: Writer) WriteVersion* (version: INTEGER), NEW;
	PROCEDURE^ (VAR wr: Writer) WriteSChar* (x: SHORTCHAR), NEW;
	PROCEDURE^ (VAR wr: Writer) WriteInt* (x: INTEGER), NEW;
	PROCEDURE^ (VAR wr: Writer) WriteXString* (IN x: ARRAY OF CHAR), NEW;
	PROCEDURE^ (VAR wr: Writer) WriteStore* (x: Store), NEW;
	
	PROCEDURE^ Join* (s0, s1: Store);


	(** Operation **)

	PROCEDURE (op: Operation) Do* (), NEW, ABSTRACT;


	(** Store **)

	PROCEDURE NewDomain (initialized: BOOLEAN): Domain;
		VAR d: Domain;
	BEGIN
		NEW(d); d.level := 0; d.sDict := NIL; d.cleaner := NIL;
		d.initialized := initialized; d.copyDomain := FALSE;
		RETURN d
	END NewDomain;

	PROCEDURE DomainOf (s: Store): Domain;
		VAR d, p, q, r: Domain;
	BEGIN
		d := s.dlink;
		IF (d # NIL) & (d.dlink # NIL) THEN
			p := NIL; q := d; r := q.dlink;
			WHILE r # NIL DO q.dlink := p; p := q; q := r; r := q.dlink END;
			d := q;
			WHILE p # NIL DO q := p; p := q.dlink; q.dlink := d END;
			s.dlink := d
		END;
		RETURN d
	END DomainOf;

	PROCEDURE (s: Store) Domain*(): Domain, NEW;
		VAR d: Domain;
	BEGIN
		d := DomainOf(s);
		IF (d # NIL) & ~d.initialized THEN d := NIL END;
		RETURN d
	END Domain;
	
	PROCEDURE (s: Store) CopyFrom- (source: Store), NEW, EMPTY;

	PROCEDURE (s: Store) Internalize- (VAR rd: Reader), NEW, EXTENSIBLE;
		VAR thisVersion: INTEGER;
	BEGIN
		rd.ReadVersion(minVersion, maxStoreVersion, thisVersion);
		IF ~rd.cancelled & s.isElem THEN
			rd.ReadVersion(minVersion, maxStoreVersion, thisVersion)
			(* works since maxStoreVersion = maxElemVersion = 0 in pre-1.3 *)
		END
	END Internalize;

	PROCEDURE (s: Store) ExternalizeAs- (VAR s1: Store), NEW, EMPTY;

	PROCEDURE (s: Store) Externalize- (VAR wr: Writer), NEW, EXTENSIBLE;
	BEGIN
		wr.WriteVersion(maxStoreVersion);
		IF s.isElem THEN wr.WriteVersion(maxStoreVersion) END
	END Externalize;


	(** Alien **)

	PROCEDURE^ CopyOf* (s: Store): Store;

	PROCEDURE (a: Alien) CopyFrom- (source: Store);
		VAR s, c, cp: AlienComp; piece: AlienPiece; part: AlienPart;
	BEGIN
		WITH source: Alien DO
			a.path := source.path;
			a.cause := source.cause;
			a.file := source.file;
			a.comps := NIL;
			s := source.comps; cp := NIL;
			WHILE s # NIL DO
				WITH s: AlienPiece DO
					NEW(piece); c := piece;
					piece.pos := s.pos; piece.len := s.len
				| s: AlienPart DO
					NEW(part); c := part;
					IF s.store # NIL THEN part.store := CopyOf(s.store); Join(part.store, a) END
				END;
				IF cp # NIL THEN cp.next := c ELSE a.comps := c END;
				cp := c;
				s := s.next
			END
		END
	END CopyFrom;

	PROCEDURE (a: Alien) Internalize- (VAR rd: Reader);
	BEGIN
		HALT(100)
	END Internalize;

	PROCEDURE (a: Alien) Externalize- (VAR w: Writer);
	BEGIN
		HALT(100)
	END Externalize;


	(* types *)

	PROCEDURE NewStore (t: OSA.Type): Store;
		VAR p: ANYPTR;
	BEGIN
		ASSERT(t # NIL, 20); p:=NIL;
		OSA.NewObj(p, t); ASSERT(p # NIL, 100);
		ASSERT(p IS Store, 21);
		RETURN p(Store)
	END NewStore;


	(* type dictionary *)

	PROCEDURE GetThisType (VAR d: TypeDict; id: INTEGER; VAR type: TypeName);
	(* pre: (id, t) IN dict *)
		VAR h, p: TypeDict; org, k: INTEGER;
	BEGIN
		k := id MOD dictLineLen; org := id - k;
		h := NIL; p := d; WHILE p.org # org DO h := p; p := p.next END;
		IF h # NIL THEN h.next := p.next; p.next := d; d := p END;
		type := p.type[k]$;
		ASSERT(type # "", 100)
	END GetThisType;

	PROCEDURE ThisId (VAR d: TypeDict; t: TypeName): INTEGER;
	(* pre: t # "" *)
	(* post: res = id if (t, id) in dict, res = -1 else *)
		VAR h, p: TypeDict; k, id: INTEGER;
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

	PROCEDURE ThisBaseId (VAR d: TypeDict; id: INTEGER): INTEGER;
	(* post: res = id if base(t) # NIL, res = -1 if base(t) = NIL; res >= 0 => T(res) = base(t) *)
		VAR h, p: TypeDict; k, org, baseId: INTEGER;
	BEGIN
		k := id MOD dictLineLen; org := id - k;
		h := NIL; p := d; WHILE p.org # org DO h := p; p := p.next END;
		IF h # NIL THEN h.next := p.next; p.next := d; d := p END;
		baseId := p.baseId[k];
		RETURN baseId
	END ThisBaseId;

	PROCEDURE AddType (VAR d, h: TypeDict; id: INTEGER; type: TypeName);
		VAR k: INTEGER;
	BEGIN
		k := id MOD dictLineLen;
		IF (h = NIL) OR ((k = 0) & (h.org # id)) THEN
			NEW(h); h.org := id - k; h.next := d; d := h
		END;
		h.type[k] := type$; h.baseId[k] := -1
	END AddType;

	PROCEDURE AddBaseId (h: TypeDict; id, baseId: INTEGER);
		VAR k: INTEGER;
	BEGIN
		k := id MOD dictLineLen;
		h.baseId[k] := baseId
	END AddBaseId;

	PROCEDURE InitTypeDict (VAR d, h: TypeDict; VAR nextID: INTEGER);
	BEGIN
		d := NIL; h := NIL; nextID := 0
	END InitTypeDict;


	(* store dictionary - used to maintain referential sharing *)

	PROCEDURE ThisStore (VAR d: StoreDict; id: INTEGER): Store;
	(* pre: (id, s) IN dict *)
		VAR h, p: StoreDict; s: Store; k, org: INTEGER;
	BEGIN
		k := id MOD dictLineLen; org := id - k;
		h := NIL; p := d; WHILE p.org # org DO h := p; p := p.next END;
		IF h # NIL THEN h.next := p.next; p.next := d; d := p END;
		s := p.elem[k];
		ASSERT(s # NIL, 100);
		RETURN s
	END ThisStore;

	PROCEDURE AddStore (VAR d, h: StoreDict; s: Store);
		VAR k: INTEGER;
	BEGIN
		k := s.id MOD dictLineLen;
		IF (h = NIL) OR ((k = 0) & (h.org # s.id)) THEN
			NEW(h); h.org := s.id - k; h.next := d; d := h
		END;
		h.elem[k] := s
	END AddStore;

	PROCEDURE InitStoreDict (VAR d, h: StoreDict; VAR nextID: INTEGER);
	BEGIN
		d := NIL; h := NIL; nextID := 0
	END InitStoreDict;


	(* support for type mapping *)

	PROCEDURE ReadPath (VAR rd: Reader; VAR path: TypePath);
		VAR h: TypeDict; id, extId: INTEGER; i: INTEGER; kind: SHORTCHAR;

		PROCEDURE AddPathComp (VAR rd: Reader);
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

	PROCEDURE WritePath (VAR wr: Writer; VAR path: TypePath);
		VAR h: TypeDict; id, extId: INTEGER; i, n: INTEGER;
	BEGIN
		h := NIL; extId:=0;
		n := 0; WHILE path[n] # "" DO INC(n) END;
		i := 0;
		WHILE i < n DO
			id := ThisId(wr.tDict, path[i]);
			IF id >= 0 THEN
				IF h # NIL THEN AddBaseId(h, extId, id) END;
				wr.WriteSChar(oldType); wr.WriteInt(id); n := i
			ELSE
				IF i + 1 < n THEN wr.WriteSChar(newExt) ELSE wr.WriteSChar(newBase) END;
				wr.WriteXString(path[i]);
				IF h # NIL THEN AddBaseId(h, extId, wr.nextTypeId) END;
				AddType(wr.tDict, wr.tHead, wr.nextTypeId, path[i]);
				h := wr.tHead; extId := wr.nextTypeId;
				INC(wr.nextTypeId);
				IF path[i] = modelTName THEN
					id := ThisId(wr.tDict, elemTName); ASSERT(id < 0, 100); ASSERT(i + 2 = n, 101);
					wr.WriteSChar(newExt); wr.WriteXString(elemTName);
					IF h # NIL THEN AddBaseId(h, extId, wr.nextTypeId) END;
					AddType(wr.tDict, wr.tHead, wr.nextTypeId, elemTName);
					h := wr.tHead; extId := wr.nextTypeId;
					INC(wr.nextTypeId)
				END
			END;
			INC(i)
		END
	END WritePath;

	PROCEDURE WriteType (VAR wr: Writer; t: OSA.Type);
		VAR path: TypePath;
	BEGIN
		OSA.GetTypePath(path, t);
		WritePath(wr, path)
	END WriteType;


	(* support for alien mapping *)

	PROCEDURE InternalizeAlien (VAR rd: Reader; VAR comps: AlienComp; down, pos, len: INTEGER);
		VAR h, p: AlienComp; piece: AlienPiece; part: AlienPart; file: Files.File;
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

	PROCEDURE ExternalizeAlien (VAR wr: Writer; file: Files.File; comps: AlienComp);
		VAR p: AlienComp;
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


	(** Reader **)

	PROCEDURE (VAR rd: Reader) ConnectTo* (f: Files.File), NEW;
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

	PROCEDURE (VAR rd: Reader) SetPos* (pos: INTEGER), NEW;
	BEGIN
		rd.rider.SetPos(pos)
	END SetPos;

	PROCEDURE (VAR rd: Reader) Pos* (): INTEGER, NEW;
	BEGIN
		RETURN rd.rider.Pos()
	END Pos;

	PROCEDURE (VAR rd: Reader) ReadBool* (OUT x: BOOLEAN), NEW;
		VAR b: BYTE;
	BEGIN
		rd.rider.ReadByte(b); x := b # 0
	END ReadBool;

	PROCEDURE (VAR rd: Reader) ReadSChar* (OUT x: SHORTCHAR), NEW;
	BEGIN
		rd.rider.ReadByte(SYSTEM.VAL(BYTE, x))
	END ReadSChar;

	PROCEDURE (VAR rd: Reader) ReadXChar* (OUT x: CHAR), NEW;
		VAR c: SHORTCHAR;
	BEGIN
		rd.rider.ReadByte(SYSTEM.VAL(BYTE,c)); x := c
	END ReadXChar;

	PROCEDURE (VAR rd: Reader) ReadChar* (OUT x: CHAR), NEW;
		VAR le: ARRAY 2 OF BYTE;	(* little endian, big endian *)
	BEGIN
		rd.rider.ReadBytes(le, 0, 2);
		x := CHR(le[0] MOD 256 + (le[1] MOD 256) * 256)
	END ReadChar;

	PROCEDURE (VAR rd: Reader) ReadByte* (OUT x: BYTE), NEW;
	BEGIN
		rd.rider.ReadByte(x)
	END ReadByte;

	PROCEDURE (VAR rd: Reader) ReadSInt* (OUT x: SHORTINT), NEW;
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

	PROCEDURE (VAR rd: Reader) ReadXInt* (OUT x: INTEGER), NEW;
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

	PROCEDURE (VAR rd: Reader) ReadInt* (OUT x: INTEGER), NEW;
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

	PROCEDURE (VAR rd: Reader) ReadLong* (OUT x: LONGINT), NEW;
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

	PROCEDURE (VAR rd: Reader) ReadSReal* (OUT x: SHORTREAL), NEW;
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

	PROCEDURE (VAR rd: Reader) ReadXReal* (OUT x: REAL), NEW;
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

	PROCEDURE (VAR rd: Reader) ReadReal* (OUT x: REAL), NEW;
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

	PROCEDURE (VAR rd: Reader) ReadSet* (OUT x: SET), NEW;
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

	PROCEDURE (VAR rd: Reader) ReadSString* (OUT x: ARRAY OF SHORTCHAR), NEW;
		VAR i: INTEGER; ch: SHORTCHAR;
	BEGIN
		i := 0; REPEAT rd.ReadSChar(ch); x[i] := ch; INC(i) UNTIL ch = 0X
	END ReadSString;

	PROCEDURE (VAR rd: Reader) ReadXString* (OUT x: ARRAY OF CHAR), NEW;
		VAR i: INTEGER; ch: CHAR;
	BEGIN
		i := 0; REPEAT rd.ReadXChar(ch); x[i] := ch; INC(i) UNTIL ch = 0X
	END ReadXString;

	PROCEDURE (VAR rd: Reader) ReadString* (OUT x: ARRAY OF CHAR), NEW;
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

	PROCEDURE (VAR rd: Reader) TurnIntoAlien* (cause: INTEGER), NEW;
	BEGIN
		ASSERT(cause > 0, 20);
		rd.cancelled := TRUE; rd.readAlien := TRUE; rd.cause := cause;
		AlienReport(cause)
	END TurnIntoAlien;

	PROCEDURE (VAR rd: Reader) ReadVersion* (min, max: INTEGER; OUT version: INTEGER), NEW;
		VAR v: BYTE;
	BEGIN
		rd.ReadByte(v); version := v;
		IF (version < min) OR (version > max) THEN
			rd.TurnIntoAlien(alienVersion)
		END
	END ReadVersion;

	PROCEDURE (VAR rd: Reader) ReadStore* (OUT x: Store), NEW;
		VAR a: Alien; t: OSA.Type;
			len, pos, pos1, id, comment, next, down, downPos, nextTypeId, nextElemId, nextStoreId: INTEGER;
			kind: SHORTCHAR; path: TypePath; type: TypeName;
			save: ReaderState;
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
			t := ThisType(type);
			IF t # NIL THEN
				x := NewStore(t); x.isElem := kind = elem
			ELSE
				rd.cause := thisTypeRes; AlienTypeReport(rd.cause, type);
				x := NIL
			END;
			IF x # NIL THEN
				IF SamePath(t, path) THEN
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


	(** Writer **)

	PROCEDURE (VAR wr: Writer) ConnectTo* (f: Files.File), NEW;
	(** pre: wr.rider = NIL  OR  f = NIL **)
	BEGIN
		IF f = NIL THEN
			wr.rider := NIL
		ELSE
			ASSERT(wr.rider = NIL, 20);
			wr.rider := f.NewWriter(wr.rider); wr.SetPos(f.Length());
			wr.era := nextEra; INC(nextEra);
			wr.noDomain := TRUE;
			wr.modelType := ThisType(modelTName);
			InitTypeDict(wr.tDict, wr.tHead, wr.nextTypeId);
			wr.nextElemId := 0; wr.nextStoreId := 0;
			wr.st.linkpos := -1
		END;
		wr.writtenStore := NIL
	END ConnectTo;

	PROCEDURE (VAR wr: Writer) SetPos* (pos: INTEGER), NEW;
	BEGIN
		wr.rider.SetPos(pos)
	END SetPos;

	PROCEDURE (VAR wr: Writer) Pos* (): INTEGER, NEW;
	BEGIN
		RETURN wr.rider.Pos()
	END Pos;

	PROCEDURE (VAR wr: Writer) WriteBool* (x: BOOLEAN), NEW;
	BEGIN
		IF x THEN wr.rider.WriteByte(1) ELSE wr.rider.WriteByte(0) END
	END WriteBool;

	PROCEDURE (VAR wr: Writer) WriteSChar* (x: SHORTCHAR), NEW;
	BEGIN
		wr.rider.WriteByte(SYSTEM.VAL(BYTE, x))
	END WriteSChar;

	PROCEDURE (VAR wr: Writer) WriteXChar* (x: CHAR), NEW;
		VAR c: SHORTCHAR;
	BEGIN
		c := SHORT(x); wr.rider.WriteByte(SYSTEM.VAL(BYTE, c))
	END WriteXChar;

	PROCEDURE (VAR wr: Writer) WriteChar* (x: CHAR), NEW;
		TYPE a = ARRAY 2 OF BYTE;
		VAR le, be: a;	(* little endian, big endian *)
	BEGIN
		IF Kernel.littleEndian THEN
			le := SYSTEM.VAL(a, x)
		ELSE
			be := SYSTEM.VAL(a, x);
			le[0] := be[1]; le[1] := be[0]
		END;
		wr.rider.WriteBytes(le, 0, 2)
	END WriteChar;

	PROCEDURE (VAR wr: Writer) WriteByte* (x: BYTE), NEW;
	BEGIN
		wr.rider.WriteByte(x)
	END WriteByte;

	PROCEDURE (VAR wr: Writer) WriteSInt* (x: SHORTINT), NEW;
		TYPE a = ARRAY 2 OF BYTE;
		VAR le, be: a;	(* little endian, big endian *)
	BEGIN
		IF Kernel.littleEndian THEN
			le := SYSTEM.VAL(a, x)
		ELSE
			be := SYSTEM.VAL(a, x);
			le[0] := be[1]; le[1] := be[0]
		END;
		wr.rider.WriteBytes(le, 0, 2)
	END WriteSInt;

	PROCEDURE (VAR wr: Writer) WriteXInt* (x: INTEGER), NEW;
		TYPE a = ARRAY 2 OF BYTE;
		VAR y: SHORTINT; le, be: a;	(* little endian, big endian *)
	BEGIN
		y := SHORT(x);
		IF Kernel.littleEndian THEN
			le := SYSTEM.VAL(a, y)
		ELSE
			be := SYSTEM.VAL(a, y);
			le[0] := be[1]; le[1] := be[0]
		END;
		wr.rider.WriteBytes(le, 0, 2)
	END WriteXInt;

	PROCEDURE (VAR wr: Writer) WriteInt* (x: INTEGER), NEW;
		TYPE a = ARRAY 4 OF BYTE;
		VAR le, be: a;	(* little endian, big endian *)
	BEGIN
		IF Kernel.littleEndian THEN
			le := SYSTEM.VAL(a, x)
		ELSE
			be := SYSTEM.VAL(a, x);
			le[0] := be[3]; le[1] := be[2]; le[2] := be[1]; le[3] := be[0]
		END;
		wr.rider.WriteBytes(le, 0, 4)
	END WriteInt;

	PROCEDURE (VAR wr: Writer) WriteLong* (x: LONGINT), NEW;
		TYPE a = ARRAY 8 OF BYTE;
		VAR le, be: a;	(* little endian, big endian *)
	BEGIN
		IF Kernel.littleEndian THEN
			le := SYSTEM.VAL(a, x)
		ELSE
			be := SYSTEM.VAL(a, x);
			le[0] := be[7]; le[1] := be[6]; le[2] := be[5]; le[3] := be[4];
			le[4] := be[3]; le[5] := be[2]; le[6] := be[1]; le[7] := be[0]
		END;
		wr.rider.WriteBytes(le, 0, 8)
	END WriteLong;

	PROCEDURE (VAR wr: Writer) WriteSReal* (x: SHORTREAL), NEW;
		TYPE a = ARRAY 4 OF BYTE;
		VAR le, be: a;	(* little endian, big endian *)
	BEGIN
		IF Kernel.littleEndian THEN
			le := SYSTEM.VAL(a, x)
		ELSE
			be := SYSTEM.VAL(a, x);
			le[0] := be[3]; le[1] := be[2]; le[2] := be[1]; le[3] := be[0]
		END;
		wr.rider.WriteBytes(le, 0, 4)
	END WriteSReal;

	PROCEDURE (VAR wr: Writer) WriteXReal* (x: REAL), NEW;
		TYPE a = ARRAY 4 OF BYTE;
		VAR y: SHORTREAL; le, be: a;	(* little endian, big endian *)
	BEGIN
		y := SHORT(x);
		IF Kernel.littleEndian THEN
			le := SYSTEM.VAL(a, y)
		ELSE
			be := SYSTEM.VAL(a, y);
			le[0] := be[3]; le[1] := be[2]; le[2] := be[1]; le[3] := be[0]
		END;
		wr.rider.WriteBytes(le, 0, 4)
	END WriteXReal;

	PROCEDURE (VAR wr: Writer) WriteReal* (x: REAL), NEW;
		TYPE a = ARRAY 8 OF BYTE;
		VAR le, be: a;	(* little endian, big endian *)
	BEGIN
		IF Kernel.littleEndian THEN
			le := SYSTEM.VAL(a, x)
		ELSE
			be := SYSTEM.VAL(a, x);
			le[0] := be[7]; le[1] := be[6]; le[2] := be[5]; le[3] := be[4];
			le[4] := be[3]; le[5] := be[2]; le[6] := be[1]; le[7] := be[0]
		END;
		wr.rider.WriteBytes(le, 0, 8)
	END WriteReal;

	PROCEDURE (VAR wr: Writer) WriteSet* (x: SET), NEW;
		(* SIZE(SET) = 4 *)
		TYPE a = ARRAY 4 OF BYTE;
		VAR le, be: a;	(* little endian, big endian *)
	BEGIN
		IF Kernel.littleEndian THEN
			le := SYSTEM.VAL(a, x)
		ELSE
			be := SYSTEM.VAL(a, x);
			le[0] := be[3]; le[1] := be[2]; le[2] := be[1]; le[3] := be[0]
		END;
		wr.rider.WriteBytes(le, 0, 4)
	END WriteSet;

	PROCEDURE (VAR wr: Writer) WriteSString* (IN x: ARRAY OF SHORTCHAR), NEW;
		VAR i: INTEGER; ch: SHORTCHAR;
	BEGIN
		i := 0; ch := x[0]; WHILE ch # 0X DO wr.WriteSChar(ch); INC(i); ch := x[i] END;
		wr.WriteSChar(0X)
	END WriteSString;

	PROCEDURE (VAR wr: Writer) WriteXString* (IN x: ARRAY OF CHAR), NEW;
		VAR i: INTEGER; ch: CHAR;
	BEGIN
		i := 0; ch := x[0]; WHILE ch # 0X DO wr.WriteXChar(ch); INC(i); ch := x[i] END;
		wr.WriteSChar(0X)
	END WriteXString;

	PROCEDURE (VAR wr: Writer) WriteString* (IN x: ARRAY OF CHAR), NEW;
		VAR i: INTEGER; ch: CHAR;
	BEGIN
		i := 0; ch := x[0]; WHILE ch # 0X DO wr.WriteChar(ch); INC(i); ch := x[i] END;
		wr.WriteChar(0X)
	END WriteString;

	PROCEDURE (VAR wr: Writer) WriteVersion* (version: INTEGER), NEW;
	BEGIN
		wr.WriteByte(SHORT(SHORT(version)))
	END WriteVersion;

	PROCEDURE (VAR wr: Writer) WriteStore* (x: Store), NEW;
		VAR t: Kernel.Type; pos1, pos2, pos: INTEGER;
			save: WriterState;
	BEGIN
		ASSERT(wr.rider # NIL, 20);
		IF x # NIL THEN
			IF wr.noDomain THEN
				wr.domain := x.Domain(); wr.noDomain := FALSE
			ELSE ASSERT(x.Domain() = wr.domain, 21)
			END;
			x.ExternalizeAs(x); IF x = NIL THEN wr.writtenStore := NIL; RETURN END
		END;
		IF wr.st.linkpos > 0 THEN	(* link to previous block's <next> or up block's <down> *)
			pos := wr.Pos();
			IF pos - wr.st.linkpos = 4 THEN
				(* hack to resolve ambiguity between next = 0 because of end-of-chain, or because of offset = 0.
					above guard holds only if for the latter case.
					ASSUMPTION:
						this can happen only if linkpos points to a next (not a down)
						and there is a comment byte just before
				*)
				wr.SetPos(wr.st.linkpos - 4); wr.WriteInt(1); wr.WriteInt(pos - wr.st.linkpos - 4)
			ELSE
				wr.SetPos(wr.st.linkpos); wr.WriteInt(pos - wr.st.linkpos - 4)
			END;
			wr.SetPos(pos)
		END;
		IF x = NIL THEN
			wr.WriteSChar(nil);
			wr.WriteInt(0);	(* <comment> *)
			wr.st.linkpos := wr.Pos();
			wr.WriteInt(0)	(* <next> *)
		ELSIF x.era >= wr.era THEN
			ASSERT(x.era = wr.era, 23);
			IF x.isElem THEN wr.WriteSChar(link) ELSE wr.WriteSChar(newlink) END;
			wr.WriteInt(x.id);
			wr.WriteInt(0);	(* <comment> *)
			wr.st.linkpos := wr.Pos();
			wr.WriteInt(0)	(* <next> *)
		ELSE
			x.era := wr.era;
			WITH x: Alien DO
				IF x.isElem THEN
					wr.WriteSChar(elem); x.id := wr.nextElemId; INC(wr.nextElemId)
				ELSE
					wr.WriteSChar(store); x.id := wr.nextStoreId; INC(wr.nextStoreId)
				END;
				WritePath(wr, x.path)
			ELSE
				t := Kernel.TypeOf(x);
				x.isElem := t.base[1] = wr.modelType;
				IF x.isElem THEN
					wr.WriteSChar(elem); x.id := wr.nextElemId; INC(wr.nextElemId)
				ELSE
					wr.WriteSChar(store); x.id := wr.nextStoreId; INC(wr.nextStoreId)
				END;
				WriteType(wr, t)
			END;
			wr.WriteInt(0);	(* <comment> *)
			pos1 := wr.Pos(); wr.WriteInt(0); wr.WriteInt(0);	(* <next>, <down> *)
			pos2 := wr.Pos(); wr.WriteInt(0);	(* <len> *)
			save := wr.st;	(* push current writer state; switch to structured *)
			wr.st.linkpos := pos1 + 4;
			WITH x: Alien DO ExternalizeAlien(wr, x.file, x.comps)
			ELSE 
				x.Externalize(wr)
			END;
			wr.st := save;	(* pop writer state *)
			wr.st.linkpos := pos1;
			pos := wr.Pos();
			wr.SetPos(pos2); wr.WriteInt(pos - pos2 - 4);	(* patch <len> *)
			wr.SetPos(pos)
		END;
		wr.writtenStore := x
	END WriteStore;


	(** miscellaneous **)

	PROCEDURE Report* (IN msg, p0, p1, p2: ARRAY OF CHAR);
	BEGIN
		IF logReports THEN
			Dialog.ShowParamMsg(msg, p0, p1, p2)
		END
	END Report;

	PROCEDURE BeginCloning (d: Domain);
	BEGIN
		ASSERT(d # NIL, 20);
		INC(d.level);
		IF d.level = 1 THEN
			d.copyera := nextEra; INC(nextEra); d.nextElemId := 0;
			IF d.cleaner = NIL THEN NEW(d.cleaner); d.cleaner.d := d END;
			Kernel.PushTrapCleaner(d.cleaner)
		END
	END BeginCloning;

	PROCEDURE EndCloning (d: Domain);
	BEGIN
		ASSERT(d # NIL, 20);
		DEC(d.level);
		IF d.level = 0 THEN 
			d.sDict := NIL;
			Kernel.PopTrapCleaner(d.cleaner);
			d.s := NIL
		END
	END EndCloning;

	PROCEDURE CopyOf* (s: Store): Store;
		VAR h: Store; c: StoreDict; d: Domain; k, org: INTEGER;
	BEGIN
		ASSERT(s # NIL, 20);
		
		d := DomainOf(s);
		IF d = NIL THEN d := NewDomain(anonymousDomain); s.dlink := d; d.copyDomain := TRUE END;

		BeginCloning(d);
		IF s.era >= d.copyera THEN	(* s has already been copied *)
			ASSERT(s.era = d.copyera, 21);
			k := s.id MOD dictLineLen; org := s.id - k;
			c := d.sDict;
			WHILE (c # NIL) & (c.org # org) DO c := c.next END;
			ASSERT((c # NIL) & (c.elem[k] # NIL), 100);
			h := c.elem[k]
		ELSE
			s.era := d.copyera;
			s.id := d.nextElemId; INC(d.nextElemId);
			Kernel.NewObj(h, Kernel.TypeOf(s));
			k := s.id MOD dictLineLen;
			IF k = 0 THEN NEW(c); c.org := s.id; c.next := d.sDict; d.sDict := c 
			ELSE c := d.sDict
			END;
			ASSERT((c # NIL) & (c.org = s.id - k) & (c.elem[k] = NIL), 101);
			c.elem[k] := h;
			IF d.s = NIL THEN d.s := h ELSE Join(h, d.s) END;
			h.CopyFrom(s)
		END;
		EndCloning(d);
		RETURN h
	END CopyOf;

	PROCEDURE ExternalizeProxy* (s: Store): Store;
	BEGIN
		IF s # NIL THEN s.ExternalizeAs(s) END;
		RETURN s
	END ExternalizeProxy;

	PROCEDURE InitDomain* (s: Store);
		VAR d: Domain;
	BEGIN
		ASSERT(s # NIL, 20);
		d := DomainOf(s);
		IF d = NIL THEN d := NewDomain(inited); s.dlink := d
		ELSE d.initialized := TRUE
		END
	END InitDomain;
	
	PROCEDURE Join* (s0, s1: Store);
		VAR d0, d1: Domain;
	BEGIN
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
	END Join;

	PROCEDURE Joined* (s0, s1: Store): BOOLEAN;
		VAR d0, d1: Domain;
	BEGIN
		ASSERT(s0 # NIL, 20); ASSERT(s1 # NIL, 21);
		d0 := DomainOf(s0); d1 := DomainOf(s1);
		RETURN (s0 = s1) OR ((d0 = d1) & (d0 # NIL))
	END Joined;

	PROCEDURE Unattached* (s: Store): BOOLEAN;
	BEGIN
		ASSERT(s # NIL, 20);
		RETURN  (s.dlink = NIL) OR s.dlink.copyDomain
	END Unattached;

BEGIN
	nextEra := 1; logReports := FALSE
END Stores.
