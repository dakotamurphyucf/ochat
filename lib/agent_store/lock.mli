(** Process-scoped advisory ownership locks with inspectable owner metadata. *)

type owner =
  { server_id : Agent_protocol.Id.Server.t
  ; process_id : int
  ; process_start_identity : string option
  ; hostname : string
  ; acquired_at : Agent_protocol.Timestamp.t
  ; nonce : string
  }
[@@deriving sexp]

type t

val owner : t -> owner
val path : t -> string

(** [acquire] obtains a nonblocking exclusive kernel lock. Stale metadata is
    overwritten only after the kernel grants ownership. *)
val acquire
  :  env:Eio_unix.Stdenv.base
  -> sw:Eio.Switch.t
  -> path:string
  -> server_id:Agent_protocol.Id.Server.t
  -> process_start_identity:string option
  -> nonce:string
  -> (t, Store_error.t) result

(** [read_owner] reads owner metadata without attempting to acquire the lock. *)
val read_owner
  :  env:Eio_unix.Stdenv.base
  -> path:string
  -> (owner option, Store_error.t) result

(** [release t] clears metadata and releases both kernel locks. It is idempotent. *)
val release : env:Eio_unix.Stdenv.base -> t -> (unit, Store_error.t) result
