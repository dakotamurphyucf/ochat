open! Core

(** Eio-owned Unix-domain peer identity. *)

type t =
  { uid : int
  ; gid : int
  ; pid : int option
  }
[@@deriving compare, equal, sexp]

(** [of_flow flow] reads credentials from an Eio Unix stream socket. It fails
    closed when the backend has no file descriptor or the platform does not
    expose peer credentials. *)
val of_flow : _ Eio.Resource.t -> (t, Agent_protocol.Error.t) result

(** [authenticate_same_user ~scopes flow] authenticates a peer only when its
    effective UID matches the daemon. The resulting principal ID is stable
    for that UID across connections and daemon restarts. *)
val authenticate_same_user
  :  scopes:Agent_protocol.Scope.Set.t
  -> _ Eio.Resource.t
  -> (Agent_protocol.Principal.t, Agent_protocol.Error.t) result

(** [effective_uid ()] returns the daemon process effective UID. *)
val effective_uid : unit -> int
