MODULE FwFilesReaderWriter;

	IMPORT SYSTEM, Kernel, Files;
	
	PROCEDURE ReadBool* (rider: Files.Reader; OUT x: BOOLEAN);
		VAR b: BYTE;
	BEGIN
		rider.ReadByte(b); x := b # 0
	END ReadBool;

	PROCEDURE ReadSChar* (rider: Files.Reader; OUT x: SHORTCHAR);
	BEGIN
		rider.ReadByte(SYSTEM.VAL(BYTE, x))
	END ReadSChar;

	PROCEDURE ReadXChar* (rider: Files.Reader; OUT x: CHAR);
		VAR c: SHORTCHAR;
	BEGIN
		rider.ReadByte(SYSTEM.VAL(BYTE,c)); x := c
	END ReadXChar;

	PROCEDURE ReadChar* (rider: Files.Reader; OUT x: CHAR);
		VAR le: ARRAY 2 OF BYTE;	(* little endian, big endian *)
	BEGIN
		rider.ReadBytes(le, 0, 2);
		x := CHR(le[0] MOD 256 + (le[1] MOD 256) * 256)
	END ReadChar;

	PROCEDURE ReadByte* (rider: Files.Reader; OUT x: BYTE);
	BEGIN
		rider.ReadByte(x)
	END ReadByte;

	PROCEDURE ReadSInt* (rider: Files.Reader; OUT x: SHORTINT);
		VAR le, be: ARRAY 2 OF BYTE;	(* little endian, big endian *)
	BEGIN
		rider.ReadBytes(le, 0, 2);
		IF Kernel.littleEndian THEN
			x := SYSTEM.VAL(SHORTINT, le)
		ELSE
			be[0] := le[1]; be[1] := le[0];
			x := SYSTEM.VAL(SHORTINT, be)
		END
	END ReadSInt;

	PROCEDURE ReadXInt* (rider: Files.Reader; OUT x: INTEGER);
		VAR le, be: ARRAY 2 OF BYTE;	(* little endian, big endian *)
	BEGIN
		rider.ReadBytes(le, 0, 2);
		IF Kernel.littleEndian THEN
			x := SYSTEM.VAL(SHORTINT, le)
		ELSE
			be[0] := le[1]; be[1] := le[0];
			x := SYSTEM.VAL(SHORTINT, be)
		END
	END ReadXInt;

	PROCEDURE ReadInt* (rider: Files.Reader; OUT x: INTEGER);
		VAR le, be: ARRAY 4 OF BYTE;	(* little endian, big endian *)
	BEGIN
		rider.ReadBytes(le, 0, 4);
		IF Kernel.littleEndian THEN
			x := SYSTEM.VAL(INTEGER, le)
		ELSE
			be[0] := le[3]; be[1] := le[2]; be[2] := le[1]; be[3] := le[0];
			x := SYSTEM.VAL(INTEGER, be)
		END
	END ReadInt;

	PROCEDURE ReadLong* (rider: Files.Reader; OUT x: LONGINT);
		VAR le, be: ARRAY 8 OF BYTE;	(* little endian, big endian *)
	BEGIN
		rider.ReadBytes(le, 0, 8);
		IF Kernel.littleEndian THEN
			x := SYSTEM.VAL(LONGINT, le)
		ELSE
			be[0] := le[7]; be[1] := le[6]; be[2] := le[5]; be[3] := le[4];
			be[4] := le[3]; be[5] := le[2]; be[6] := le[1]; be[7] := le[0];
			x := SYSTEM.VAL(LONGINT, be)
		END
	END ReadLong;

	PROCEDURE ReadSReal* (rider: Files.Reader; OUT x: SHORTREAL);
		VAR le, be: ARRAY 4 OF BYTE;	(* little endian, big endian *)
	BEGIN
		rider.ReadBytes(le, 0, 4);
		IF Kernel.littleEndian THEN
			x := SYSTEM.VAL(SHORTREAL, le)
		ELSE
			be[0] := le[3]; be[1] := le[2]; be[2] := le[1]; be[3] := le[0];
			x := SYSTEM.VAL(SHORTREAL, be)
		END
	END ReadSReal;

	PROCEDURE ReadXReal* (rider: Files.Reader; OUT x: REAL);
		VAR le, be: ARRAY 4 OF BYTE;	(* little endian, big endian *)
	BEGIN
		rider.ReadBytes(le, 0, 4);
		IF Kernel.littleEndian THEN
			x := SYSTEM.VAL(SHORTREAL, le)
		ELSE
			be[0] := le[3]; be[1] := le[2]; be[2] := le[1]; be[3] := le[0];
			x := SYSTEM.VAL(SHORTREAL, be)
		END
	END ReadXReal;

	PROCEDURE ReadReal* (rider: Files.Reader; OUT x: REAL);
		VAR le, be: ARRAY 8 OF BYTE;	(* little endian, big endian *)
	BEGIN
		rider.ReadBytes(le, 0, 8);
		IF Kernel.littleEndian THEN
			x := SYSTEM.VAL(REAL, le)
		ELSE
			be[0] := le[7]; be[1] := le[6]; be[2] := le[5]; be[3] := le[4];
			be[4] := le[3]; be[5] := le[2]; be[6] := le[1]; be[7] := le[0];
			x := SYSTEM.VAL(REAL, be)
		END
	END ReadReal;

	PROCEDURE ReadSet* (rider: Files.Reader; OUT x: SET);
		VAR le, be: ARRAY 4 OF BYTE;	(* little endian, big endian *)
	BEGIN
		rider.ReadBytes(le, 0, 4);
		IF Kernel.littleEndian THEN
			x := SYSTEM.VAL(SET, le)
		ELSE
			be[0] := le[3]; be[1] := le[2]; be[2] := le[1]; be[3] := le[0];
			x := SYSTEM.VAL(SET, be)
		END
	END ReadSet;

	PROCEDURE ReadSString* (rider: Files.Reader; OUT x: ARRAY OF SHORTCHAR);
		VAR i: INTEGER; ch: SHORTCHAR;
	BEGIN
		i := 0; REPEAT ReadSChar(rider, ch); x[i] := ch; INC(i) UNTIL ch = 0X
	END ReadSString;

	PROCEDURE ReadXString* (rider: Files.Reader; OUT x: ARRAY OF CHAR);
		VAR i: INTEGER; ch: CHAR;
	BEGIN
		i := 0; REPEAT ReadXChar(rider, ch); x[i] := ch; INC(i) UNTIL ch = 0X
	END ReadXString;

	PROCEDURE ReadString* (rider: Files.Reader; OUT x: ARRAY OF CHAR);
		VAR i: INTEGER; ch: CHAR;
	BEGIN
		i := 0; REPEAT ReadChar(rider, ch); x[i] := ch; INC(i) UNTIL ch = 0X
	END ReadString;
	

	PROCEDURE WriteBool* (rider: Files.Writer; x: BOOLEAN);
	BEGIN
		IF x THEN rider.WriteByte(1) ELSE rider.WriteByte(0) END
	END WriteBool;

	PROCEDURE WriteSChar* (rider: Files.Writer; x: SHORTCHAR);
	BEGIN
		rider.WriteByte(SYSTEM.VAL(BYTE, x))
	END WriteSChar;

	PROCEDURE WriteXChar* (rider: Files.Writer; x: CHAR);
		VAR c: SHORTCHAR;
	BEGIN
		c := SHORT(x); rider.WriteByte(SYSTEM.VAL(BYTE, c))
	END WriteXChar;

	PROCEDURE WriteChar* (rider: Files.Writer; x: CHAR);
		TYPE a = ARRAY 2 OF BYTE;
		VAR le, be: a;	(* little endian, big endian *)
	BEGIN
		IF Kernel.littleEndian THEN
			le := SYSTEM.VAL(a, x)
		ELSE
			be := SYSTEM.VAL(a, x);
			le[0] := be[1]; le[1] := be[0]
		END;
		rider.WriteBytes(le, 0, 2)
	END WriteChar;

	PROCEDURE WriteByte* (rider: Files.Writer; x: BYTE);
	BEGIN
		rider.WriteByte(x)
	END WriteByte;

	PROCEDURE WriteSInt* (rider: Files.Writer; x: SHORTINT);
		TYPE a = ARRAY 2 OF BYTE;
		VAR le, be: a;	(* little endian, big endian *)
	BEGIN
		IF Kernel.littleEndian THEN
			le := SYSTEM.VAL(a, x)
		ELSE
			be := SYSTEM.VAL(a, x);
			le[0] := be[1]; le[1] := be[0]
		END;
		rider.WriteBytes(le, 0, 2)
	END WriteSInt;

	PROCEDURE WriteXInt* (rider: Files.Writer; x: INTEGER);
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
		rider.WriteBytes(le, 0, 2)
	END WriteXInt;

	PROCEDURE WriteInt* (rider: Files.Writer; x: INTEGER);
		TYPE a = ARRAY 4 OF BYTE;
		VAR le, be: a;	(* little endian, big endian *)
	BEGIN
		IF Kernel.littleEndian THEN
			le := SYSTEM.VAL(a, x)
		ELSE
			be := SYSTEM.VAL(a, x);
			le[0] := be[3]; le[1] := be[2]; le[2] := be[1]; le[3] := be[0]
		END;
		rider.WriteBytes(le, 0, 4)
	END WriteInt;

	PROCEDURE WriteLong* (rider: Files.Writer; x: LONGINT);
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
		rider.WriteBytes(le, 0, 8)
	END WriteLong;

	PROCEDURE WriteSReal* (rider: Files.Writer; x: SHORTREAL);
		TYPE a = ARRAY 4 OF BYTE;
		VAR le, be: a;	(* little endian, big endian *)
	BEGIN
		IF Kernel.littleEndian THEN
			le := SYSTEM.VAL(a, x)
		ELSE
			be := SYSTEM.VAL(a, x);
			le[0] := be[3]; le[1] := be[2]; le[2] := be[1]; le[3] := be[0]
		END;
		rider.WriteBytes(le, 0, 4)
	END WriteSReal;

	PROCEDURE WriteXReal* (rider: Files.Writer; x: REAL);
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
		rider.WriteBytes(le, 0, 4)
	END WriteXReal;

	PROCEDURE WriteReal* (rider: Files.Writer; x: REAL);
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
		rider.WriteBytes(le, 0, 8)
	END WriteReal;

	PROCEDURE WriteSet* (rider: Files.Writer; x: SET);
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
		rider.WriteBytes(le, 0, 4)
	END WriteSet;

	PROCEDURE WriteSString* (rider: Files.Writer; IN x: ARRAY OF SHORTCHAR);
		VAR i: INTEGER; ch: SHORTCHAR;
	BEGIN
		i := 0; ch := x[0]; WHILE ch # 0X DO WriteSChar(rider, ch); INC(i); ch := x[i] END;
		WriteSChar(rider, 0X)
	END WriteSString;

	PROCEDURE WriteXString* (rider: Files.Writer; IN x: ARRAY OF CHAR);
		VAR i: INTEGER; ch: CHAR;
	BEGIN
		i := 0; ch := x[0]; WHILE ch # 0X DO WriteXChar(rider, ch); INC(i); ch := x[i] END;
		WriteSChar(rider, 0X)
	END WriteXString;

	PROCEDURE WriteString* (rider: Files.Writer; IN x: ARRAY OF CHAR);
		VAR i: INTEGER; ch: CHAR;
	BEGIN
		i := 0; ch := x[0]; WHILE ch # 0X DO WriteChar(rider, ch); INC(i); ch := x[i] END;
		WriteChar(rider, 0X)
	END WriteString;
	
END FwFilesReaderWriter.
