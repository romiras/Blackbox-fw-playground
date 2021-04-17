MODULE FwDoc;

    CONST
	docTag = 6F4F4443H; docVersion = 0;

    PROCEDURE ImportDocument* (f: Files.File; OUT s: Stores.Store);
        VAR r: Stores.Reader; tag, version: INTEGER;
    BEGIN
        ASSERT(f # NIL, 20);
        r.ConnectTo(f);
        r.ReadInt(tag);
        IF tag = docTag THEN
            r.ReadInt(version);
            ASSERT(version = docVersion, 100);
            r.ReadStore(s);
            IF s IS Document THEN s := s(Document).ThisView()
            ELSE s := NIL
            END
        END
    END ImportDocument;

END FwDoc.
