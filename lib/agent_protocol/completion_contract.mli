open Core

(** Host-captured standalone completion contract. It does not authorize execution
    or delivery: the runtime must rebind the pinned publisher/tool ceiling and
    verify the owning invocation's actual Pending acknowledgement. *)
type t =
  { tool_name : string
  ; tool_fingerprint : string
  ; capability_pins : (string * string) list
  ; completion_schema : Jsonaf.t option
  ; max_output_bytes : int
  ; max_output_depth : int
  }
[@@deriving equal, sexp]

val validate : t -> (unit, Protocol_error.t) result
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Protocol_error.t) result
