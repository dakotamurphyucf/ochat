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

(** Compute SHA256 from this ACTUAL identity during compilation. Linked.compile
    checks it against Prepared.compiler_contract before parsing. No independent
    digest claim is trusted. Compilation.target/contract remain adapters here. *)
val fingerprint : t -> checkpoint:(unit -> unit) -> Chatml_source.Digest.t
