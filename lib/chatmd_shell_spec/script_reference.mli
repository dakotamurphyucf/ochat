open Core

(** An actual shell use of a script, shared by manifest validation and prepared
    program planning. Custom matcher entrypoints are retained without interpreting
    source code. Other roles select their kind's normal entrypoint. *)
type t =
  { path : string list
  ; script : string
  ; kind : Chatmd_script_spec.kind
  ; entrypoint : string option
  }

(** Walk policy and before-interceptor matchers, reviewers, interceptors, effect
    analyzers and audit filters. Includes matcher predicates on executable
    interceptors. After-interceptor predicates are unsupported and rejected by
    manifest validation, matching the authoring parser. Input is an effective
    runtime after defaults and inheritance;
    this function acquires no authority and executes no code. *)
val of_runtime : Shell_spec.t -> t list
