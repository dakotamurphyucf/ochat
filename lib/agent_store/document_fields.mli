(** Pure named-field helpers shared by concrete durable document owners.
    No current runtime readers or I/O participate in these projections. *)
open! Core

(** Shared durable-document admission profile: depth 256, one million object
    fields and two million JSON nodes. The complete envelope, including embedded
    documents, must fit these bounds and the caller's configured byte budget.
    Writers, stored metadata projections and recovery use this same profile. *)
val limits : max_bytes:int -> (Document_schema.Limits.t, Document_schema.Error.t) Result.t

val invalid : string -> string -> ('a, Document_schema.Error.t) Result.t

val required
  :  Jsonaf.t
  -> string
  -> (Jsonaf.t -> ('a, Document_schema.Error.t) Result.t)
  -> ('a, Document_schema.Error.t) Result.t

val optional
  :  Jsonaf.t
  -> string
  -> (Jsonaf.t -> ('a, Document_schema.Error.t) Result.t)
  -> ('a option, Document_schema.Error.t) Result.t

val string : Jsonaf.t -> (string, Document_schema.Error.t) Result.t
val boolean : Jsonaf.t -> (bool, Document_schema.Error.t) Result.t
val decimal : Jsonaf.t -> (int64, Document_schema.Error.t) Result.t
val array : Jsonaf.t -> (Jsonaf.t list, Document_schema.Error.t) Result.t

val document
  :  ?limits:Document_schema.Limits.t
  -> Jsonaf.t
  -> (Document_schema.Document.t, Document_schema.Error.t) Result.t

val digest : Jsonaf.t -> (string, Document_schema.Error.t) Result.t
val decimal_json : int64 -> Jsonaf.t
val option_json : 'a option -> f:('a -> Jsonaf.t) -> Jsonaf.t
val shape : (string * Document_schema.Shape.t) list -> Document_schema.Shape.t

val upgrade
  :  Document_schema.Document.t
  -> limits:Document_schema.Limits.t
  -> kind:string
  -> (Document_schema.Document.t, Document_schema.Error.t) Result.t

val protocol
  :  ('a, Agent_protocol.Error.t) Result.t
  -> ('a, Document_schema.Error.t) Result.t

val store : ('a, Document_schema.Error.t) Result.t -> ('a, Store_error.t) Result.t

val expect
  :  Document_schema.Document.t
  -> kind:string
  -> version:int
  -> (unit, Document_schema.Error.t) Result.t

(** Explicit supported family versions, checked on the stored original document
    before any conversion or current typed reader. *)
val expect_versions
  :  Document_schema.Document.t
  -> kind:string
  -> versions:int list
  -> (unit, Document_schema.Error.t) Result.t

val record_error : Document_record.Error.t -> Store_error.t

(** Feed decoded string values, including unknown members and escaped IDs.
    Caller validates/bounds the tree first and owns reference/deletion policy. *)
val iter_strings : Jsonaf.t -> f:(string -> unit) -> unit
