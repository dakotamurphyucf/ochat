(** Bounded cross-session query. The scan bound is checked against the authorized
    matching catalog before immutable state IO. Cursor remains a top-level field;
    reasons and the bound are part of its authenticated query binding. *)
type t = private
  { server_id : Id.Server.t
  ; catalog : Session.List_request.t
  ; reasons : Session_activity.Reason.t list
  ; scan_limit : int
  }
[@@deriving sexp]

val create
  :  server_id:Id.Server.t
  -> catalog:Session.List_request.t
  -> reasons:Session_activity.Reason.t list
  -> scan_limit:int
  -> (t, Error.t) result

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
