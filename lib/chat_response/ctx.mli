(** Shared, immutable execution context passed around by the
    {!module:Chat_response} helper sub-modules.

    The record bundles together the *capabilities* needed by most
    functions – notably networking, filesystem access, and a shared
    cache – so that callers only pass a single value instead of a long
    parameter list.  A typical value is created once at program start
    and then threaded through the call-graph.

    {1 Why an ['env] type parameter?}

    Rather than taking a concrete [Eio_unix.Stdenv.base] we keep the
    environment abstract and only rely on {e structural} typing: the
    object must implement at least [net].  This brings two benefits:

    • The module remains agnostic of the chosen Eio backend (Unix,
      Mimic, tests…).
    • Unit tests can inject cheap stubs that satisfy the interface
      without spinning up real network resources.

    {1 Thread-safety}

    The context value itself is immutable, however the [cache] field is
    an internal {!module:Cache} instance that performs in-place updates.
    If you plan to share a cache between concurrent fibres, make sure
    the chosen implementation offers the required synchronisation or
    wrap it behind a mutex.
*)

type 'env t = private
  { inference_relation : Transcript.Scope.relation
  ; inference_context : Inference_runtime.Context.t
  ; inference_identity : Neutral_turn.Identity.t
  ; on_inference_attempt : Inference_runtime.Attempt.t -> unit
  ; on_inference_completion : Inference_client.Completion.t -> unit
  ; on_inference_observation : Inference.Observation.t -> unit
  ; env : 'env (** Underlying Eio standard environment. *)
  ; dir : Eio.Fs.dir_ty Eio.Path.t
    (** Base directory used when a function needs to access the
            file-system – e.g. reading a local include. *)
  ; tool_dir : Eio.Fs.dir_ty Eio.Path.t
    (** Current working directory applied when spawning a tool.  It
            defaults to [dir] but callers may pick a different value to
            respect the user’s CWD. *)
  ; cache : Cache.t (** Shared TTL-LRU instance for memoising results. *)
  }

(** [create ~env ~dir ~tool_dir ~cache] creates a fresh context from its
    constituents.  No validation is performed beyond structural typing
    of [env]; the caller is responsible for ensuring that the provided
    directories remain valid for the desired lifetime. *)
val create
  :  inference_context:Inference_runtime.Context.t
  -> inference_identity:Neutral_turn.Identity.t
  -> on_inference_attempt:(Inference_runtime.Attempt.t -> unit)
  -> on_inference_completion:(Inference_client.Completion.t -> unit)
  -> ?inference_relation:Transcript.Scope.relation
  -> ?on_inference_observation:(Inference.Observation.t -> unit)
  -> env:'env
  -> dir:Eio.Fs.dir_ty Eio.Path.t
  -> tool_dir:Eio.Fs.dir_ty Eio.Path.t
  -> cache:Cache.t
  -> unit
  -> 'env t

(** [of_env ~env ~cache] is a convenience constructor that takes the
    standard values returned by [Eio.Stdenv.*] helpers:

    {v
      dir      = Eio.Stdenv.fs  env
      tool_dir = Eio.Stdenv.cwd env
    v} *)
val of_env
  :  inference_context:Inference_runtime.Context.t
  -> inference_identity:Neutral_turn.Identity.t
  -> on_inference_attempt:(Inference_runtime.Attempt.t -> unit)
  -> on_inference_completion:(Inference_client.Completion.t -> unit)
  -> ?inference_relation:Transcript.Scope.relation
  -> ?on_inference_observation:(Inference.Observation.t -> unit)
  -> env:(< fs : Eio.Fs.dir_ty Eio.Path.t ; cwd : Eio.Fs.dir_ty Eio.Path.t ; .. > as 'env)
  -> cache:Cache.t
  -> unit
  -> 'env t

(** Expected host revocation before authentication or wire dispatch. Contains no
    private error or request payload. A successful guard acknowledgement is the
    admission point: a later Stop can race an already-admitted dispatch. *)
exception Inference_admission_rejected

(** Add a host admission guard before the existing strict attempt observer.
    Inference derivation and child contexts preserve both callbacks. Unexpected
    failures and cancellation propagate; no observer or backend is replaced. *)
val with_inference_attempt_guard
  :  'env t
  -> before_attempt:(Inference_runtime.Attempt.t -> unit)
  -> 'env t

(** Rebind only the explicitly selected inference context, retaining host identity
    and observers. Call Context.derive or the host resolver/policy first; this
    operation neither authorizes a target change nor recaptures stored selection. *)
val with_inference : 'env t -> inference_context:Inference_runtime.Context.t -> 'env t

(** Rebind actual tool-owner correlation on an immutable child context. The
    parent must come from Invocation.inference_parent or another actual owner;
    absent correlation must never be fabricated from a call alias. *)
val with_inference_parent : 'env t -> parent:Transcript.Scope.parent -> 'env t

(** Selected inference ports for auxiliary requests owned by this invocation.
    Attempt admission remains the supplied host identity and acknowledgement;
    no actor lookup, ambient backend resolution or additional callback is installed. *)
val inference_execution : 'env t -> Inference_client.Execution.t

(** [net t] exposes the network namespace obtained from [t.env#net]. *)
val net : < net : 'a ; .. > t -> 'a

(** [env t] returns the underlying environment object.  Prefer the
    specialised helpers provided by other modules when possible. *)
val env : 'env t -> 'env

(** [dir t] returns the root directory used for local IO operations. *)
val dir : _ t -> Eio.Fs.dir_ty Eio.Path.t

(** [cache t] retrieves the shared TTL-LRU cache carried by the
    context. *)
val cache : _ t -> Cache.t

(** [tool_dir t] returns the directory that acts as the current working
    directory when spawning tools.  When the caller did not override
    the value it is equal to {!dir}. *)
val tool_dir : _ t -> Eio.Fs.dir_ty Eio.Path.t
