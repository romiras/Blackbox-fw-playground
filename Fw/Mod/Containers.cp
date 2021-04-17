MODULE FwContainers;

	IMPORT Models := FwModels;
	TYPE
		Model* = POINTER TO ABSTRACT RECORD (Models.Model) END;

END FwContainers.
