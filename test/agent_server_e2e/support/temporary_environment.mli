open Core

(** Isolated Eio-owned filesystem roots for one E2E scenario. *)

type roots =
  { root : string
  ; home : string
  ; data : string
  ; cache : string
  ; config : string
  ; workspaces : string
  ; logs : string
  ; artifacts : string
  ; sockets : string
  ; temporary : string
  }
[@@deriving sexp]

type t

(** [with_ ~env f] creates private roots, including a short Unix-socket root,
    invokes [f], and removes owned paths after return, failure, or cancellation.
    Partial allocation is rolled back. Cleanup attempts both roots independently
    under cancellation protection, reporting removal failures rather than hiding
    them. Ordinary callback failures are re-raised as sanitized diagnostics even
    if retaining artifacts fails. Eio cancellation retains its original type. *)
val with_
  :  ?scenario:string
  -> ?failure_artifact_root:string
  -> env:Eio_unix.Stdenv.base
  -> (t -> 'a)
  -> 'a

(** [roots t] returns the native paths owned by [t]. *)
val roots : t -> roots

(** [child_environment t ~base] removes ambient provider credentials and
    overrides home, cache, config, temporary, and E2E roots for a child. *)
val child_environment : t -> base:string array -> string array

(** [path t native_path] resolves [native_path] through the Eio filesystem
    capability owned by [t]. *)
val path : t -> string -> Eio.Fs.dir_ty Eio.Path.t

(** [fs t] returns the filesystem capability owned by [t]. *)
val fs : t -> Eio.Fs.dir_ty Eio.Path.t

(** [register_secret t secret] redacts literal and Base64 forms from retained
    failure reports and ordinary outward failure/cleanup diagnostics. *)
val register_secret : t -> string -> unit

(** Fault injection for the harness's own transactional setup and cleanup tests.
    Callbacks must raise before their side effect on failure. *)
module For_testing : sig
  val with_operations
    :  mkdir:(string -> unit)
    -> rmtree:(string -> unit)
    -> ?scenario:string
    -> ?failure_artifact_root:string
    -> env:Eio_unix.Stdenv.base
    -> (t -> 'a)
    -> 'a
end
