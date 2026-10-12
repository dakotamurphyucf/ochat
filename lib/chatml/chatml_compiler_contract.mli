open Core
module Static = Chatml_static_surface

(** Pure authoritative metadata extracted from current target/contract helper;
    no second registry, runtime installer or Eio dependency. *)
module Target : sig
  type t =
    | One_off_v1
    | Tool_v1
    | Moderator_v1
    | Delegated_moderator_v1
  [@@deriving equal, sexp]
end

(** Abstract finite target plus static surface projection/host requirements.
    No runtime Surface/builtin.impl or claimed fingerprint can enter this value.
    Constructor selects existing target definitions and immediately projects. *)
type t

module Kind : sig
  type t =
    | Standard of Target.t
    | Native
end

(** Prepared Eio adapter calls this BEFORE domain creation; its domain closure
    receives only static data/required schemes, never original native callbacks. *)
val for_target : Target.t -> checkpoint:(unit -> unit) -> t

val kind : t -> Kind.t

(** Authoritative compatibility names for project requirements and receipts.
    Standard targets use their public lower-case target names; native metadata
    uses the distinct native compiler family and ["native"] target. These names
    describe this already selected contract and never choose or grant a target. *)
val compiler_family : t -> string

val target_name : t -> string
val static_surface : t -> Static.t
val required_bindings : t -> (string * Chatml_builtin_spec.ty) list

(** Pure native metadata context for compile-only/custom graph consumers.
    Compute identity from this supplied static surface AND exact selected
    required bindings, under a distinct native compiler identity. No supplied
    digest or fake Standard target pairing. Empty requirements yield an inert
    compile-only artifact. Public registered-target adapter uses for_target.
    Actual187 execution still needs caller-authorized runtime implementations. *)
val for_static_surface
  :  Static.t
  -> required_bindings:(string * Chatml_builtin_spec.ty) list
  -> t

(** Preserve existing compiler.v1 identity exactly: version/target, actual
    projected globals/module exports/aliases and grouped entrypoint contracts. *)
val identity : t -> checkpoint:(unit -> unit) -> Sexp.t

(** Return the actual canonical encoded identity byte length. [allocate] debits
    conservative conversion/encoding allocation estimates BEFORE construction;
    callers may reject admission by raising from it. This is not retained-byte
    accounting or a heap sandbox. Uses the existing identity codec and polls
    actual work; retains no encoded identity or callback. *)
val input_bytes : t -> checkpoint:(unit -> unit) -> allocate:(int -> unit) -> int

(** Compute SHA256 from this ACTUAL identity during compilation. Linked.compile
    checks it against Prepared.compiler_contract before parsing. No independent
    digest claim is trusted. Compilation.target/contract remain adapters here. *)
val fingerprint : t -> checkpoint:(unit -> unit) -> Chatml_source.Digest.t

(** Project actual installer metadata and compare its computed canonical SHA256
    identity using this authentic contract's existing kind/requirements as the
    comparison context. Includes every global/export scheme and alias body.
    Retains no installer; no caller digest claim or native implementation equality.
    Poll projection/encoding/hash; charge identity containers/bytes to [allocate]
    when supplied by a host execution scope. Metadata equality does not grant
    authority or prove native implementations honor their declared schemes. *)
val matches_runtime_surface
  :  t
  -> Chatml_builtin_surface.surface
  -> checkpoint:(unit -> unit)
  -> ?allocate:(int -> unit)
  -> unit
  -> bool

module Invocation : sig
  type contract = t

  (** Validated host invocation roles against authentic selected metadata, not
      runtime types. No inference cells, callback, installer or captured inventory. *)
  type t

  val entrypoints : t -> string list

  (** Standard one-off/tool targets allow their fixed main/run names; moderator
      targets require session invocation. Native requires an explicit outer TFun
      with a task result and exactly [argument_count] parameters. Generic selected
      value contracts remain usable by the non-invoking Runtime.instantiate API.
      Wrong kind/name/declared shape rejects before initialization. *)
  val standalone
    :  contract
    -> entrypoint:string
    -> argument_count:int
    -> checkpoint:(unit -> unit)
    -> (t, Chatml_diagnostic.t) Result.t

  (** Standard moderator/delegated targets require canonical initial_state and
      on_event roles. Native allows custom names, requiring explicit outer
      TFun([ctx;state;event], TCon("task",[state])) for on_event. Its state input
      and result must have the same canonical static type encoding as the selected
      initial_state requirement, including shared variable names. No runtime
      inference, alias solving or claim about native implementation correctness.
      Poll actual conversion/encoding/hash and charge allocations when supplied.
      Legacy caller-selected session APIs keep their existing behavior. *)
  val session
    :  contract
    -> initial_state:string
    -> on_event:string
    -> checkpoint:(unit -> unit)
    -> ?allocate:(int -> unit)
    -> unit
    -> (t, Chatml_diagnostic.t) Result.t

  (** Native event/state hook session. Require an explicit outer two-argument
      task function and identical canonical state requirement/input/result types.
      Standard contracts reject before initialization. Reuses session admission
      encoding and allocation checkpoints, without runtime type inference. *)
  val event_state_session
    :  contract
    -> initial_state:string
    -> on_event:string
    -> checkpoint:(unit -> unit)
    -> ?allocate:(int -> unit)
    -> unit
    -> (t, Chatml_diagnostic.t) Result.t
end
