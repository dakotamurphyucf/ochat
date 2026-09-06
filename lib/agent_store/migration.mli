(** Non-destructive store schema inspection and migration planning. *)

type mode =
  | Validate_only
  | Dry_run
  | Apply
[@@deriving compare, equal, sexp]

type status =
  | Current
  | Migration_required
  | Schema_too_new
[@@deriving compare, equal, sexp]

type plan =
  { source_version : int
  ; target_version : int
  ; session_count : int
  ; status : status
  ; mode : mode
  }
[@@deriving sexp]

(** [inspect] is read-only and never acquires or modifies store ownership. *)
val inspect
  :  env:Eio_unix.Stdenv.base
  -> root:string
  -> mode:mode
  -> (plan, Store_error.t) result

(** [run] inspects the schema and counts session directories while holding the
    daemon lock. It does not validate individual session journals or artifacts.
    Validation and dry-run modes return a plan for every schema status.
    Applying an unsupported older or newer schema fails without mutation. *)
val run
  :  env:Eio_unix.Stdenv.base
  -> sw:Eio.Switch.t
  -> root:string
  -> server_id:Agent_protocol.Id.Server.t
  -> process_start_identity:string option
  -> lock_nonce:string
  -> mode:mode
  -> (plan, Store_error.t) result
