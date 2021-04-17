MODULE FwViews;

	IMPORT Stores := FwStores;

	TYPE
		View* = POINTER TO ABSTRACT RECORD (Stores.Store) END;

END FwViews.
