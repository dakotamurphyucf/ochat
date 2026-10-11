open Core
module Spec = Chatml_builtin_spec

(** Static projection of the existing runtime surface. NEVER a runtime surface:
    Spec.builtin.impl is deliberately absent, including module exports. *)
module Binding : sig
  type t =
    { name : string
    ; scheme : Spec.ty
    }
end

module Module : sig
  type t =
    { name : string
    ; exports : Binding.t list
    }

  val scheme : ?checkpoint:(unit -> unit) -> t -> Spec.ty
end

(** Immutable names/type descriptions/aliases only. No runtime value, function,
    installer, credential, registry or execution control. *)
type t

(** Project only names/schemes/module export descriptions/type-alias bodies,
    keeping current ordering/collision semantics. Poll walks; retain neither
    callback nor original Surface. No dummy builtin implementations. Prepared
    domain adapter projects BEFORE entering a compiler domain. *)
val of_runtime_surface : Chatml_builtin_surface.surface -> checkpoint:(unit -> unit) -> t

val globals : t -> Binding.t list
val modules : t -> Module.t list
val type_aliases : t -> Chatml_builtin_surface.builtin_type_alias list

(** Existing checker gets static initializer helpers; runtime-surface adapters
    project and delegate to those same helpers. Configured_check.create_static
    retains only this projection and its invocation-owned inference machine. *)
