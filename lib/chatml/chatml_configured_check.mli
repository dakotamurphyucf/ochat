open Core
module Lookup = Chatml_configured_lookup
module Reference = Lookup.Reference
module Parse = Chatml.Chatml_parse

(** OCH185's real-checker adapter, compiler-domain only. It factors existing unit
    inference/export collection, not a second checker, scanner or host compiler.
    OCH186 reuses this engine and refines interfaces with companion matching. *)
type t

(** One shared inference identity/weak-cell owner for one Session invocation and authentic supplied surface.
    Discard this engine and all its compiler-owned exports on failure/cancellation.
    No source/inventory/current-origin globals or initializer evaluation. *)
val create : Chatml.Chatml_builtin_surface.surface -> checkpoint:(unit -> unit) -> t

module Public_module : sig
  (** Completed compiler-owned public schemes/types. Weak cells deliberately
      shared; not a domain-transfer value or OCH127 frozen semantics. *)
  type t

  val value_scheme : t -> Chatml_typechecker.typ
  val types : t -> Chatml_typechecker.Type_alias.t
end

module Checked_unit : sig
  type t

  val annotations : t -> Chatml_typechecker.checked_program
  val exports : t -> Public_module.t
end

(** Run the EXISTING infer_stmt/export paths with this source's fresh annotation
    maps and t's shared machine. Each independent file starts at inference level0,
    restoring the importer level on every exit; imported weak cells cannot gain
    polymorphism through importer generalization. Value/open/qualified syntax calls [require] for genuine configured
    fallback or an actual use of a binding previously imported by configured open. Capture immutable defining type fallbacks;
    use ordinary raw schemes/value restriction. No companion matching, target
    entrypoint checks, resolved linkage artifact or initialization policy here.
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
