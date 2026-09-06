(** Redacted, cursor-paged server and session audit records. *)

type level =
  | Info
  | Warning
  | Error
[@@deriving compare, equal, sexp]

type t =
  { sequence : int64
  ; timestamp : Timestamp.t
  ; level : level
  ; name : string
  ; session_id : Id.Session.t option
  ; principal_id : Id.Principal.t option
  ; payload : Jsonaf.t
  ; redacted : bool
  }
[@@deriving sexp]

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result

module Read_request : sig
  type t =
    { page : Page.Request.t
    ; session_id : Id.Session.t option
    ; principal_id : Id.Principal.t option
    ; minimum_level : level option
    ; name_prefix : string option
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end
