open! Core

(** Lowers effective manifest specifications into Shell_access values. *)

type error =
  { code : string
  ; message : string
  }
[@@deriving sexp, compare, equal]

val capabilities
  :  Host.t
  -> source:Chatmd_shell_spec.Source_ref.t
  -> Chatmd_shell_spec.Shell_spec.capabilities
  -> (Shell_access.Capabilities.t, error) result

val resolver
  :  Host.t
  -> source:Chatmd_shell_spec.Source_ref.t
  -> Chatmd_shell_spec.Shell_spec.resolver
  -> (Shell_access.Resolver.t, error) result

val limits
  :  Chatmd_shell_spec.Shell_spec.limits
  -> (Shell_access.Limits.t * int, error) result

(** Construct the same declared matcher with stack-safe node traversal. The
    optional borrowed checkpoint runs before retaining traversal/output nodes.
    It supplies the enclosing operation's admission; no independent ledger is
    created or retained by the executable matcher. Original callback exceptions
    propagate, including through [policy]'s expected lowering-error boundary. *)
val matcher
  :  ?checkpoint:(unit -> unit)
  -> Host.t
  -> Chatmd_shell_spec.Source_ref.t
  -> chatml_matcher:
       (action:Chatmd_shell_spec.Shell_spec.policy_action
        -> Chatmd_shell_spec.Shell_spec.chatml_matcher
        -> Shell_access.Matcher.t)
  -> action:Chatmd_shell_spec.Shell_spec.policy_action
  -> Chatmd_shell_spec.Shell_spec.matcher
  -> Shell_access.Matcher.t

val policy
  :  ?checkpoint:(unit -> unit)
  -> Host.t
  -> source:Chatmd_shell_spec.Source_ref.t
  -> chatml_matcher:
       (action:Chatmd_shell_spec.Shell_spec.policy_action
        -> Chatmd_shell_spec.Shell_spec.chatml_matcher
        -> Shell_access.Matcher.t)
  -> Chatmd_shell_spec.Shell_spec.policy
  -> (Shell_access.Policy.t, error) result

val backends
  :  Host.t
  -> source:Chatmd_shell_spec.Source_ref.t
  -> platform:Chatmd_shell_spec.Shell_spec.platform
  -> external_backend:(Chatmd_shell_spec.Shell_spec.backend -> Shell_access.Backend.t)
  -> Chatmd_shell_spec.Shell_spec.backends
  -> (Shell_access.Backend.t list, error) result

val audit_failure
  :  Chatmd_shell_spec.Shell_spec.audit_failure
  -> Shell_access.Audit.failure_policy
