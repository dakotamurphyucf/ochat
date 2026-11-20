val encode
  :  Catalog_state.t
  -> (Document_schema.Document.t, Document_schema.Error.t) result

val decode
  :  Document_schema.Document.t
  -> (Catalog_state.t, Document_schema.Error.t) result
