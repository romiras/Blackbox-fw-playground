MODULE FwStoresReader;

	IMPORT Stores := FwStores, FileReader := FwFileReader;

	TYPE
		StoresReader* = RECORD
			rider-: FileReader.Reader;
			(*
			cancelled-: BOOLEAN;	(** current Internalize has been cancelled **)
			readAlien-: BOOLEAN;	(** at least one alien read since ConnectTo **)
			cause: INTEGER;
			nextTypeId, nextElemId, nextStoreId: INTEGER;	(* next id of non-dict type, "elem", store *)
			tDict, tHead: TypeDict;	(* mapping (id <-> type) - self-organizing list *)
			eDict, eHead: StoreDict;	(* mapping (id -> elem) - self-organizing list *)
			sDict, sHead: StoreDict;	(* mapping (id -> store) - self-organizing list *)
			st: ReaderState;
			noDomain: BOOLEAN;
			*)
			store: Stores.Store
		END;

	PROCEDURE^ (VAR rd: StoresReader) SetPos* (pos: INTEGER), NEW;
	PROCEDURE^ (VAR rd: StoresReader) ReadVersion* (min, max: INTEGER; OUT version: INTEGER), NEW;
	PROCEDURE^ (VAR rd: StoresReader) ReadStore* (OUT x: Stores.Store), NEW;

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

	PROCEDURE (VAR rd: StoresReader) ReadStore* (OUT x: Store), NEW;
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

END FwStoresReader.
