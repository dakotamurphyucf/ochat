open Core

(** Non-executing compilation in an Eio-managed domain. Successful compilation
    grants no tool or session authority. No subprocess or artifact transport. *)
type target =
  | One_off_v1
  | Tool_v1
  | Developer_tool_v1
  | Moderator_v1
  | Delegated_moderator_v1
[@@deriving sexp, equal]

(** Static tool-host selection, independent of provider credentials and runtime
    authority. Existing callers retain [Standard] and its original fingerprint. *)
module Tool_contract : sig
  type t =
    | Standard
    | Developer_model_spawn
  [@@deriving sexp, equal, compare]

  val target : t -> target
end

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

(** Joined, non-executing compilation for an actual legacy custom host surface.
    Uses the same source/time admission and cooperative cleanup as [compile].
    Retains that surface in the existing legacy artifact; it is not a static
    Linked artifact or a grant. Original unexpected exceptions/cancellation and
    backtraces propagate, rather than being converted to compiler_failed. *)
val compile_with_surface
  :  ?limits:limits
  -> env:Eio_unix.Stdenv.base
  -> surface:Chatml.Chatml_builtin_surface.surface
  -> required_bindings:(string * Chatml.Chatml_builtin_spec.ty) list
  -> source:string
  -> unit
  -> (Chatml_host_runtime.compiled_script, error) Result.t

(** Compile one already validated immutable capture through the same bounded
    domain/policy adapter as [compile]. Check its exact byte length before
    starting a domain; do not reopen, rewrite or rehash its contents. Successful
    programs and parser/type failures retain its identity, revision and source.
    No initializer executes. The capture can cross the joined domain boundary;
    inference state, checkpoint callbacks and execution authority do not escape.
    Resource/host failures remain unlocated, and cancellation follows [compile]. *)
val compile_captured
  :  Chatml_source.t
  -> ?limits:limits
  -> env:Eio_unix.Stdenv.base
  -> target:target
  -> unit
  -> (Chatml_host_runtime.compiled_script, error) result

(** Explicit policy variant of [compile_captured], using the same cooperative
    cancellation, elapsed-time checks and joined cleanup as [compile_with_policy].
    This API selects no project, file, bundle or runtime revision on the caller's
    behalf; the capture owner has already chosen the exact immutable source. *)
val compile_captured_with_policy
  :  Chatml_source.t
  -> policy:policy
  -> env:Eio_unix.Stdenv.base
  -> target:target
  -> unit
  -> (Chatml_host_runtime.compiled_script, error) result

(** Compile an admitted immutable graph into a separate static-only artifact.
    Admit total source+metadata bytes before projection/domain creation. Actual
    selected contract is computed/verified; one budget covers static projection,
    hashing, all parse/check/companion/entry contract/link stages and diagnostics.
    No initializer executes, no native builtin implementations cross this graph
    compiler domain, and no mutable inference data escapes. Depth is explicit
    positive host policy. Only selected host descriptors are prepared for187;
    existing text/captured APIs and runtime surface getter remain unchanged. *)
val compile_prepared
  :  Chatml_prepared_program.t
  -> ?limits:limits
  -> max_dependency_depth:int
  -> env:Eio_unix.Stdenv.base
  -> target:target
  -> unit
  -> (Chatml_linked_program.t, error) Result.t

val compile_prepared_with_policy
  :  Chatml_prepared_program.t
  -> policy:policy
  -> max_dependency_depth:int
  -> env:Eio_unix.Stdenv.base
  -> target:target
  -> unit
  -> (Chatml_linked_program.t, error) Result.t

module Entry_expectation : sig
  (** Comparison constraints from the entry's existing source owner. This is not
      another source identity, capture or execution authority. *)
  type t =
    { contents : string
    ; digest : Chatml_source.Digest.t
    }
end

(** Native/custom graph compilation through the same joined worker. [contract]
    contains only authentic callback-free static metadata; no runtime installer
    crosses the domain. Source admission covers Prepared canonical input plus
    actual canonical contract bytes. Conversion/encoding also has a separate
    conservative allocation-debit allowance: 32 times the remaining contract
    input allowance, checked before allocation. This is not heap accounting; a
    tight canonical limit does not guarantee sufficient conversion allowance.
    An optional expected original body is
    checked by exact length, digest and chunked bytes under the SAME elapsed
    checkpoint before compilation. Additional expected contents count against
    input admission unless physically the same immutable retained entry string;
    equal separate strings are not silently deduplicated. Never rewrite/reopen
    source or initialize it.
    Compiler and cancellation/error semantics follow compile_prepared. *)
val compile_prepared_with_contract
  :  Chatml_prepared_program.t
  -> contract:Chatml.Chatml_compiler_contract.t
  -> ?expected_entry:Entry_expectation.t
  -> ?limits:limits
  -> max_dependency_depth:int
  -> env:Eio_unix.Stdenv.base
  -> unit
  -> (Chatml_linked_program.t, error) Result.t

val compile_prepared_with_contract_and_policy
  :  Chatml_prepared_program.t
  -> contract:Chatml.Chatml_compiler_contract.t
  -> ?expected_entry:Entry_expectation.t
  -> policy:policy
  -> max_dependency_depth:int
  -> env:Eio_unix.Stdenv.base
  -> unit
  -> (Chatml_linked_program.t, error) Result.t

module Analysis_error : sig
  type t =
    | Policy of error
    | Analysis of Chatml_semantics.Error.t
end

(** Analyze an admitted inventory through the same joined compiler worker.
    Source/metadata admission precedes domain creation. The fixed standard static
    contract projection runs under the elapsed policy; the explicit semantic work
    ledger starts inside [Chatml_semantics.analyze], before fingerprinting,
    checking, freezing and indexing. Invalid programs return [Ok (Failed _)] with
    same-run syntax only. No initializer executes. Unexpected compiler exceptions
    and cancellation escape unchanged after joined cleanup; resource admission
    failures are typed errors. Neither callbacks nor inference cells escape. *)
val analyze_prepared
  :  Chatml_prepared_program.t
  -> ?limits:limits
  -> semantic_limits:Chatml_semantics.Limits.t
  -> max_dependency_depth:int
  -> env:Eio_unix.Stdenv.base
  -> target:target
  -> unit
  -> (Chatml_semantics.t, Analysis_error.t) Result.t

val analyze_prepared_with_policy
  :  Chatml_prepared_program.t
  -> policy:policy
  -> semantic_limits:Chatml_semantics.Limits.t
  -> max_dependency_depth:int
  -> env:Eio_unix.Stdenv.base
  -> target:target
  -> unit
  -> (Chatml_semantics.t, Analysis_error.t) Result.t

(** Authoring-only joined analysis of one parser-confirmed implementation
    prefix in the exact admitted graph. The immutable result is explicitly
    Provisional or semantic Unavailable; it cannot be executed or passed as a
    complete checked snapshot. Full [analyze_prepared] diagnostics remain
    authoritative. Incomplete companions return Unavailable. One bounded
    semantic work ledger covers prefix discovery and publication. *)
val analyze_prepared_prefix_with_policy
  :  Chatml_prepared_program.t
  -> source:Chatml_source.Id.t
  -> cursor_byte:int
  -> policy:policy
  -> semantic_limits:Chatml_semantics.Limits.t
  -> max_dependency_depth:int
  -> env:Eio_unix.Stdenv.base
  -> target:target
  -> unit
  -> (Chatml_semantics.Prefix.t, Analysis_error.t) Result.t

val analyze_prepared_prefix
  :  Chatml_prepared_program.t
  -> source:Chatml_source.Id.t
  -> cursor_byte:int
  -> ?limits:limits
  -> semantic_limits:Chatml_semantics.Limits.t
  -> max_dependency_depth:int
  -> env:Eio_unix.Stdenv.base
  -> target:target
  -> unit
  -> (Chatml_semantics.Prefix.t, Analysis_error.t) Result.t

(** Joined full and prefix semantic analysis under a host-selected authentic
    compiler contract. Canonical contract encoding and conservative conversion
    debits use the same admission as [compile_prepared_with_contract_and_policy]
    before the worker starts. These calls do not relabel the contract as a
    static target, execute initializers, or acquire sources. *)
val analyze_prepared_with_contract_and_policy
  :  Chatml_prepared_program.t
  -> contract:Chatml.Chatml_compiler_contract.t
  -> policy:policy
  -> semantic_limits:Chatml_semantics.Limits.t
  -> max_dependency_depth:int
  -> env:Eio_unix.Stdenv.base
  -> unit
  -> (Chatml_semantics.t, Analysis_error.t) Result.t

val analyze_prepared_prefix_with_contract_and_policy
  :  Chatml_prepared_program.t
  -> source:Chatml_source.Id.t
  -> cursor_byte:int
  -> contract:Chatml.Chatml_compiler_contract.t
  -> policy:policy
  -> semantic_limits:Chatml_semantics.Limits.t
  -> max_dependency_depth:int
  -> env:Eio_unix.Stdenv.base
  -> unit
  -> (Chatml_semantics.Prefix.t, Analysis_error.t) Result.t
