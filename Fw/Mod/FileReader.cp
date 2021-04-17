MODULE FwFileReader;

	IMPORT Files;

	TYPE
		Reader* = POINTER TO ABSTRACT RECORD
			eof*: BOOLEAN;
		END;

	PROCEDURE (r: Reader) Base* (): Files.File, NEW, ABSTRACT;
	PROCEDURE (r: Reader) Pos* (): INTEGER, NEW, ABSTRACT;
	PROCEDURE (r: Reader) SetPos* (pos: INTEGER), NEW, ABSTRACT;
	PROCEDURE (r: Reader) ReadByte* (OUT x: BYTE), NEW, ABSTRACT;
	PROCEDURE (r: Reader) ReadBytes* (VAR x: ARRAY OF BYTE; beg, len: INTEGER), NEW, ABSTRACT;

END FwFileReader.
