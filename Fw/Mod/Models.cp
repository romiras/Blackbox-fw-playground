MODULE FwModels;
(**
	project	= "BlackBox"
	organization	= "www.oberon.ch"
	contributors	= "Oberon microsystems"
	version	= "System/Rsrc/About"
	copyright	= "System/Rsrc/About"
	license	= "Docu/BB-License"
	changes	= ""
	issues	= ""

**)

	IMPORT OSA, Stores := FwStores;

	CONST
		minVersion = 0; maxVersion = 0;

	TYPE
		Model* = POINTER TO ABSTRACT RECORD (Stores.Store)
		END;
		
(*		Context* = POINTER TO ABSTRACT RECORD END;*)

	(** Model **)

	PROCEDURE (m: Model) Internalize- (VAR rd: Stores.Reader), EXTENSIBLE;
		VAR thisVersion: INTEGER;
	BEGIN
		m.Internalize^(rd);
		IF rd.cancelled THEN RETURN END;
		rd.ReadVersion(minVersion, maxVersion, thisVersion)
	END Internalize;

	PROCEDURE (m: Model) Externalize- (VAR wr: Stores.Writer), EXTENSIBLE;
	BEGIN
		m.Externalize^(wr);
		wr.WriteVersion(maxVersion)
	END Externalize;
	

	(** Context **)

(*
	PROCEDURE (c: Context) ThisModel* (): Model, NEW, ABSTRACT;
	PROCEDURE (c: Context) Normalize* (): BOOLEAN, NEW, ABSTRACT;
	PROCEDURE (c: Context) GetSize* (OUT w, h: INTEGER), NEW, ABSTRACT;
	PROCEDURE (c: Context) SetSize* (w, h: INTEGER), NEW, EMPTY;
	PROCEDURE (c: Context) MakeVisible* (l, t, r, b: INTEGER), NEW, EMPTY;
	PROCEDURE (c: Context) Consider* (VAR p: Proposal), NEW, EMPTY;
*)
END FwModels.
