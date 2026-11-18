(** Private wire rules shared only by native Goal Create and safe controls.
    Canonical decimal counters are nonnegative; objects are closed, bounded to
    32 KiB/depth 16 and reject duplicate fields through Json_codec.fields. *)
val counter : Jsonaf.t -> (int64, Protocol_error.t) result

val fields : Jsonaf.t -> names:string list -> (Json_codec.fields, Protocol_error.t) result
