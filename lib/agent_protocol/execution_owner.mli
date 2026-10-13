(** Pure ownership coordinates for host-executed invocations. No constructor in
    this module authenticates a principal, admits a run, or grants an effect. *)

type conversation = private
  { session_id : Id.Session.t
  ; generation : int
  }
[@@deriving equal, sexp_of]

type developer = private
  { host_id : Id.Server.t
  ; run_id : Id.Developer_run.t
  ; epoch : int
  ; principal_id : Id.Principal.t
  }
[@@deriving equal, sexp_of]

type t =
  | Conversation of conversation
  | Developer of developer
[@@deriving equal, sexp_of]

(** Revalidate every typed ID and nonnegative revision coordinate at an
    admission boundary. S-expression display is intentionally one-way. *)
val validate : t -> (unit, Error.t) result

val conversation : session_id:Id.Session.t -> generation:int -> (t, Error.t) result

(** The host issues these coordinates only after run/capture admission. The
    developer actor must still compare them with its current authenticated owner,
    epoch, source capture, and durable host ID at every effectful boundary.
    A host ID does not prove a live process lease. *)
val developer
  :  host_id:Id.Server.t
  -> run_id:Id.Developer_run.t
  -> epoch:int
  -> principal_id:Id.Principal.t
  -> (t, Error.t) result
