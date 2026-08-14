open! Core

(** Stable identifiers for manifest behavior that requires runtime support. *)

type t = string [@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

module Set = String.Set

val fixed_tool : t
val structured_tool : t
val chain_tool : t
val raw_tool : t
val script_tool : t
val seatbelt_backend : t
val bubblewrap_backend : t
val direct_backend : t
val resource_limits : t
val literal_secrets : t
val chatml_matcher : t
val chatml_reviewer : t
val chatml_before_interceptor : t
val chatml_after_interceptor : t
val chatml_effect_analyzer : t
val chatml_audit_filter : t
val model_reviewer : t
val executable_hooks : t
val external_backend : t

(** [phase1] contains behavior implemented by the Phase 1 runtime. *)
val phase1 : Set.t

(** [phase2] contains all Phase 1 behavior plus chain, raw-shell, and
    script-file tools. *)
val phase2 : Set.t

(** [phase3] contains all Phase 2 behavior plus ChatML shell extensions and
    model reviewers. *)
val phase3 : Set.t
val phase4 : Set.t
