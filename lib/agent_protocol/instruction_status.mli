(** Body-, path-, and root-label-free durable selection summary. Counts describe
    stored selection only, not current source authority or provider activation. *)
type t = private
  { revision : int64
  ; project_selected : bool
  ; project_files : int
  ; skills : int
  ; retained_captures : int
  ; retained_receipts : int
  }
[@@deriving equal, sexp]

val create
  :  revision:int64
  -> project_selected:bool
  -> project_files:int
  -> skills:int
  -> retained_captures:int
  -> retained_receipts:int
  -> (t, Error.t) result

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
