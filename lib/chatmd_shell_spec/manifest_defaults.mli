open! Core

(** Fully explicit conservative defaults used before manifest authorization. *)

(** [runtime ~id ~source] creates a deny-capability, ask-policy runtime with
    finite I/O limits and platform sandbox candidates. *)
val runtime : id:Shell_spec.Runtime_id.t -> source:Source_ref.t -> Shell_spec.t

(** [legacy_runtime ~source] creates the visibly unsafe reviewed runtime used
    to desugar legacy custom command declarations. *)
val legacy_runtime : source:Source_ref.t -> Shell_spec.t
