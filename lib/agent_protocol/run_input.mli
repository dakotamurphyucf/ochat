(** Shared immutable Run input DATA and its existing bounded codec. This module
    depends only on message content, IDs and low-level codec/limit modules;
    it does not depend on Run, Invocation, Job or native action intent.
    [Run_start.Input] is a public alias of this same type. *)
type t =
  | User_submission of Session.Message_content.t
  | Authored_start
[@@deriving sexp]

(** Existing direct content validation used by Run_start.create. This preserves
    its original behavior; it issues no Run/source/initialization authority. *)
val validate : t -> (unit, Error.t) result

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
