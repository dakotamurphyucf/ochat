open! Core

(** Field-specific shell runtime inheritance. *)

(** [runtime ~base child] overlays [child] on the fully expanded [base].
    Named collection collisions require [override=true]. *)
val runtime
  :  base:Shell_spec.t
  -> Shell_spec.t
  -> (Shell_spec.t, Diagnostic.t list) result
