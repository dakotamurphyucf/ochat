open Core

(** Non-executing compilation in an Eio-managed domain. Successful compilation
    grants no tool or session authority. No subprocess or artifact transport. *)
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

type policy =
  | Unrestricted
  | Bounded of limits

type error =
  { code : string
  ; message : string
  ; diagnostic : Chatml_host_runtime.compilation_diagnostic option
    (** Parser/typechecker details, including the original source span. Absent
        for source/resource/host failures. Both message forms remain bounded. *)
  }

(** Positive, finite host policy, without universal source/time ceilings. *)
val validate_limits : limits -> (unit, error) result

(** Versioned builtin/alias/entrypoint type identity, without implementations or
    credentials. Hosts include it in prepared/cache fingerprints. *)
val contract : target -> Sexp.t

(** Calls the native compiler using [Eio.Domain_manager.run]. Source is bounded
    before starting a domain; diagnostics expose at most 16 KiB. Compilation never
    evaluates initializers or calls tools. Mutable compiler state is invocation-local.

    Cancellation and elapsed time are checked between compiler stages and within
    inference traversals. Work between checkpoints must finish before cancellation
    takes effect; the caller waits for domain cleanup. This is a cooperative time budget, not a hard deadline or
    process/heap sandbox. No domain is abandoned after cancellation. *)
val compile
  :  ?limits:limits
  -> env:Eio_unix.Stdenv.base
  -> target:target
  -> source:string
  -> unit
  -> (Chatml_host_runtime.compiled_script, error) result

(** Explicit host policy. [Unrestricted] removes source/time limits but keeps
    cooperative caller cancellation and joined domain cleanup. It grants no tool
    or session authority; agents cannot choose this policy through submitted code.
    [compile] is the convenience wrapper for bounded compilation. *)
val compile_with_policy
  :  policy:policy
  -> env:Eio_unix.Stdenv.base
  -> target:target
  -> source:string
  -> unit
  -> (Chatml_host_runtime.compiled_script, error) result
