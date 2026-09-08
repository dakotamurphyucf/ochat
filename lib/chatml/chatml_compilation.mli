open Core

(** Non-executing compilation in a separately executed, resource-limited worker.
    The worker path is trusted host configuration from the same installation,
    never model/user input. No PATH search or ambient environment inheritance.
    Successful compilation grants no tool or session authority. *)
type target =
  | One_off_v1
  | Tool_v1
  | Moderator_v1
  | Delegated_moderator_v1
[@@deriving sexp, equal]

type limits =
  { wall_seconds : float
  ; max_source_bytes : int
  }

val default_limits : limits

type error =
  { code : string
  ; message : string
  }

(** Versioned builtin/alias/entrypoint type identity, without implementations or
    credentials. Hosts include it in prepared/cache fingerprints. *)
val contract : target -> Sexp.t

(** Timeout/cancellation kills and reaps the worker before returning/propagating.
    Transport is bounded to 16 MiB and depth 512. Errors expose at most 16 KiB.
    OS CPU, file-size and descriptor limits fail closed if unsupported. There
    is no portable hard process-heap limit in this API. This is process/resource
    isolation, not a filesystem/network sandbox; the trusted compiler never
    evaluates source or invokes tools. *)
val compile
  :  ?limits:limits
  -> env:Eio_unix.Stdenv.base
  -> worker:string
  -> target:target
  -> source:string
  -> unit
  -> (Chatml_host_runtime.compiled_script, error) result

(** Private executable entry point. Applies OS limits before reading its bounded
    request, and returns a versioned pure artifact. Not an agent-facing API. *)
val worker_main : unit -> unit
