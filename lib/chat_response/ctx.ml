(** Shared execution context used by the {!Chat_response} sub-modules.

    The module provides a thin, immutable record bundling together:

    • [env]   – An Eio standard environment (usually
      {!Eio_unix.Stdenv.base}) or a restricted object that exposes at
      least the {e structural} methods we rely on (currently [#net]).

    • [dir]   – The root directory that should be considered “current”
      when resolving relative paths inside prompts or tool invocations.
      This is often {!Eio.Stdenv.fs}, but callers may provide a
      sandboxed subtree if they want to restrict file-system access.

    • [tool_dir] – Directory that acts as the current working directory
      when spawning external tools (defaults to [dir] but can be
      overridden by the caller so that helpers such as {!module-Chat_response.Tool}
      respect the user’s chosen CWD).

    • [cache] – A {!Cache.t} instance used to memoise network fetches
      and nested agent calls.

    The record is designed to travel through all helper functions so
    that they can call {[Ctx.net ctx]} or {[Ctx.dir ctx]} instead of
    threading long parameter lists.

    The polymorphic ['env] parameter is kept abstract in order to avoid
    a hard dependency on a concrete environment type.  Any object that
    fulfils the structural requirements will compile.  In practice this
    means the value is either the one supplied by [Eio_main.run] or a
    test double that provides compatible stubs.

    {1 API at a glance}

    {v
      (* Constructors *)
      let ctx = Ctx.create ~env ~dir ~cache in
      let ctx' = Ctx.of_env ~env ~cache (* dir = Eio.Stdenv.fs env *)

      (* Accessors *)
      let net   = Ctx.net   ctx in  (* env#net *)
      let dir   = Ctx.dir   ctx in  (* Eio.Path.t *)
      let cache = Ctx.cache ctx in
    v}

    No function performs blocking IO; accessing a field is O(1).
    Thread-safety follows the rules of the underlying structures:
    the context itself is immutable but the [cache] it carries is
    mutable and not synchronised.  Each fibre should therefore use its
    own context or ensure external synchronisation when sharing a cache.
*)

type 'env t =
  { inference_relation : Transcript.Scope.relation
  ; inference_context : Inference_runtime.Context.t
  ; inference_identity : Neutral_turn.Identity.t
  ; on_inference_attempt : Inference_runtime.Attempt.t -> unit
  ; on_inference_completion : Inference_client.Completion.t -> unit
  ; on_inference_observation : Inference.Observation.t -> unit
  ; env : 'env
  ; dir : Eio.Fs.dir_ty Eio.Path.t
    (** Root directory used by {!Fetch} helpers for reading local files. *)
  ; tool_dir : Eio.Fs.dir_ty Eio.Path.t
    (** Directory where tools are executed.  Defaults to [dir] but may be
        overridden by the caller to ensure tools run relative to the user’s
        current working directory. *)
  ; cache : Cache.t
    (** Shared TTL-LRU store for memoising agent answers and HTTP fetches. *)
  }

(** [create ~env ~dir ~cache] builds a fresh context from its parts. *)
let create
      ~inference_context
      ~inference_identity
      ~on_inference_attempt
      ~on_inference_completion
      ?(inference_relation = Transcript.Scope.Root)
      ?(on_inference_observation = fun _ -> ())
      ~env
      ~dir
      ~tool_dir
      ~cache
      ()
  =
  { inference_relation
  ; inference_context
  ; inference_identity
  ; on_inference_attempt
  ; on_inference_completion
  ; on_inference_observation
  ; env
  ; dir
  ; tool_dir
  ; cache
  }
;;

let of_env
      ~inference_context
      ~inference_identity
      ~on_inference_attempt
      ~on_inference_completion
      ?inference_relation
      ?on_inference_observation
      ~env
      ~cache
      ()
  =
  create
    ~inference_context
    ~inference_identity
    ~on_inference_attempt
    ~on_inference_completion
    ?on_inference_observation
    ?inference_relation
    ~env
    ~dir:(Eio.Stdenv.fs env)
    ~tool_dir:(Eio.Stdenv.cwd env)
    ~cache
    ()
;;

exception Inference_admission_rejected

let with_inference t ~inference_context = { t with inference_context }

let with_inference_attempt_guard t ~before_attempt =
  { t with
    on_inference_attempt =
      (fun attempt ->
        before_attempt attempt;
        t.on_inference_attempt attempt)
  }
;;

let with_inference_parent t ~parent =
  { t with inference_relation = Transcript.Scope.Nested parent }
;;

let inference_execution t =
  Inference_client.Execution.create
    ~context:t.inference_context
    ~identity:t.inference_identity
    ~relation:t.inference_relation
    ~before_dispatch:(fun _ -> ())
    ~on_attempt:t.on_inference_attempt
    ~on_observation:t.on_inference_observation
    ~on_completion:t.on_inference_completion
;;

(** [net t] exposes the network namespace ([env#net]). *)
let net t = t.env#net

(** Raw access to the encapsulated environment.  Use with care – prefer
    the specialised helpers provided by other modules when possible. *)
let env t = t.env

(** Filesystem root for local IO operations. *)
let dir t = t.dir

(** Shared cache instance carried by the context. *)
let cache t = t.cache

(** Directory where tools are executed.  Defaults to [dir] but may be
    overridden by the caller to ensure tools run relative to the user’s
    current working directory. *)
let tool_dir t = t.tool_dir
