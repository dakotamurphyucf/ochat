open Core
module Lookup = Chatml_configured_lookup
module Reference = Lookup.Reference
module Parse = Chatml.Chatml_parse

(** Real configured-module checker adapter, compiler-domain only. It factors existing unit
    inference/export collection, not a second checker, scanner or host compiler.
    Companions and selected host contracts reuse the same invocation engine. *)
type t

(** One shared inference identity/weak-cell owner for one Session invocation and authentic supplied surface.
    Discard this engine and all its compiler-owned exports on failure/cancellation.
    No source/inventory/current-origin globals or initializer evaluation. *)
val create : Chatml.Chatml_builtin_surface.surface -> checkpoint:(unit -> unit) -> t

module Public_module : sig
  (** Completed compiler-owned public schemes/types. Weak cells deliberately
      shared; not a domain-transfer value or OCH127 frozen semantics. *)
  type t = Chatml_typechecker.Configured_resolution.Public_module.t

  val value_scheme : t -> Chatml_typechecker.typ
  val types : t -> Chatml_typechecker.Type_alias.t
end

module Checked_unit : sig
  type t

  val annotations : t -> Chatml_typechecker.checked_program
  val exports : t -> Public_module.t
  val visibility : t -> Chatml.Chatml_module_visibility.t
end

(** Run the EXISTING infer_stmt/export paths with this source's fresh annotation
    maps and t's shared machine. Each independent file starts at inference level0,
    restoring the importer level on every exit; imported weak cells cannot gain
    polymorphism through importer generalization. Value/open/qualified syntax calls [require] for genuine configured
    fallback or an actual use of a binding previously imported by configured open. Capture immutable defining type fallbacks;
    use ordinary raw schemes/value restriction. This compatibility helper checks
    an implementation alone; check_node adds its companion.
    Both source context and parser unit must have identical source ID/digest.
    Callback exceptions escape unchanged via existing checkpoint protection.
    Result/exports remain private to the invocation and are discarded on failure. *)
val check_unit
  :  t
  -> source:Lookup.t
  -> require:
       (source:Lookup.t -> Reference.t -> (Public_module.t, Chatml_diagnostic.t) Result.t)
  -> Parse.Unit.t
  -> (Checked_unit.t, Chatml_diagnostic.t) Result.t

module Repl_statement : sig
  (** One actual node selected by index from an admitted complete parsed unit.
      All scopes contain candidate-owned inference cells; none is published by
      this checker adapter. BEFORE retains old lexical names with any compatible
      constraints acquired during checking. AFTER adds only successfully typed
      new bindings/aliases. Alias templates preserve their existing closure
      rules; the checked annotations belong to the exact captured snippet. *)
  type t

  val statement : t -> Chatml.Chatml_lang.stmt_node
  val checked : t -> Chatml_typechecker.checked_program
  val before_values : t -> Chatml_typechecker.Value_scope.t
  val before_types : t -> Chatml_typechecker.Type_alias.t
  val after_values : t -> Chatml_typechecker.Value_scope.t
  val after_types : t -> Chatml_typechecker.Type_alias.t
  val introduced_values : t -> string list
end

(** Check exactly one source-owned top-level statement against the candidate's
    copied lexical/alias namespaces using THIS SAME [t] inference machine as
    [check_node] for newly selected canonical dependencies. Reuse the existing
    [with_state] source/guarded-callback boundary and
    [infer_stmt_with_exports] rules; do not infer a fresh unit, replay project
    main or create a second checker. Validate [parsed]'s exact source ID/digest
    against [source], its implementation phase, and [statement_index] before
    inference; select the node from that parsed unit, never from caller-supplied
    offsets. The caller validates Repl_scope currentness before and after this
    yielding work. Per-statement annotation tables are fresh but share the
    machine's candidate inference identities. Diagnostics retain this source's
    real statement span. Callback/checkpoint exceptions and raw backtraces
    propagate unchanged under the existing with_state protection. *)
val check_repl_statement
  :  t
  -> source:Lookup.t
  -> require:
       (source:Lookup.t -> Reference.t -> (Public_module.t, Chatml_diagnostic.t) Result.t)
  -> parsed:Parse.Unit.t
  -> statement_index:int
  -> before_values:Chatml_typechecker.Value_scope.t
  -> before_types:Chatml_typechecker.Type_alias.t
  -> (Repl_statement.t, Chatml_diagnostic.t) Result.t

(** Static-only engine; runtime wrapper above immediately projects. *)
val create_static
  :  ?recorder:Chatml_typechecker.Semantic.t
  -> Chatml.Chatml_static_surface.t
  -> checkpoint:(unit -> unit)
  -> t

(** Fresh REPL inference machine seeded with the names reserved across the
    detached lexical and configured-public roots. The seed is prevalidated by
    the OCH-269 root-copy bridge; this does not import an old machine or copy
    any inference cell. Ordinary [create_static] remains unchanged. *)
val create_static_for_repl
  :  Chatml.Chatml_static_surface.t
  -> name_seed:Chatml_typechecker.Repl_type_roots.Name_seed.t
  -> checkpoint:(unit -> unit)
  -> t

(** Recording is compiler-private and runs beneath Recorder.with_recording in
    the common checked-graph driver. Enabled parser units must carry owned sites;
    each unit retains its own immutable context while inference identities and
    imported weak cells still belong to this ONE engine. No facts are certified
    until companions/importers/selected host constraints have all succeeded. *)

(** Validate source IDs/digests/phases for both units before inference. Each unit
    owns its selected lookup scope, sharing identity/weak cells/root level only.
    Companion scope starts with authentic host types plus its own declarations,
    never implementation-private/importer types. Existing atomic signature probe
    returns completed restricted exports/visibility; no provisional cycle schemes.
    Checkpoint/require exceptions escape with identity/backtrace unchanged. *)
val check_node
  :  t
  -> Chatml_dependency_session.Unit.t
  -> require:
       (source:Lookup.t -> Reference.t -> (Public_module.t, Chatml_diagnostic.t) Result.t)
  -> implementation:Parse.Unit.t
  -> interface:Parse.Interface.t option
  -> (Checked_unit.t, Chatml_diagnostic.t) Result.t

module Failure : sig
  type phase =
    | Implementation
    | Companion
    | Admission

  type t

  val phase : t -> phase
  val diagnostic : t -> Chatml_diagnostic.t
end

(** Same check_node computation with the actual failing phase retained. Source
    pairing rejection is Admission; propagated dependency errors remain owned by
    the common driver's require boundary. No exception/callback policy changes. *)
val check_node_detailed
  :  t
  -> Chatml_dependency_session.Unit.t
  -> require:
       (source:Lookup.t -> Reference.t -> (Public_module.t, Chatml_diagnostic.t) Result.t)
  -> implementation:Parse.Unit.t
  -> interface:Parse.Interface.t option
  -> (Checked_unit.t, Failure.t) Result.t

module Entrypoint : sig
  type t

  val name : t -> string
  val target : t -> Chatml.Chatml_module_reference.Binding_target.t
end

(** Check grouped host contracts against the final private lexical scope, not
    the selected public exports. Imported bindings observe Value_reference at
    their original implementation introduction span, validated with input/source/
    phase affinity; shadowed locals clear origin. Invoked while entry is active.
    Checked_unit must belong to this same engine (private identity token checked
    before borrowing its raw cells). Returned targets retain no callbacks/types.
    Failure discards the invocation. *)
val check_entrypoints
  :  t
  -> Checked_unit.t
  -> source:Lookup.t
  -> require:
       (source:Lookup.t -> Reference.t -> (Public_module.t, Chatml_diagnostic.t) Result.t)
  -> required_bindings:(string * Chatml.Chatml_builtin_spec.ty) list
  -> (Entrypoint.t list, Chatml_diagnostic.t) Result.t
