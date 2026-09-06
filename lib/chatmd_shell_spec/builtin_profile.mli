open! Core

(** Versioned built-in shell runtime expansions. *)

type profile_ref =
  { requested : string
  ; runtime_id : Shell_spec.Runtime_id.t
  ; source : Source_ref.t
  }

(** [expand ~platform profile] expands [profile] into an ordinary runtime
    specification and records the concrete built-in version. *)
val expand
  :  platform:Shell_spec.platform
  -> profile_ref
  -> (Shell_spec.t, Diagnostic.t) result

(** [resolve_alias name] resolves an unversioned built-in name to its concrete
    compatible version. *)
val resolve_alias : string -> string option
