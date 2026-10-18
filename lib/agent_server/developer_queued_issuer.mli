open! Core

(** Private Host composition for a selected child's queued issuing source.
    Serialized [Issuer_source] is comparison data; this service pairs the
    existing Live package and physical Root. It owns no scheduler, Store writer,
    capacity ledger or independent native runtime. Claim restoration is a
    separate dependent implementation boundary. *)
type t

(** Issued only from the Host's one paired Live/Root resource package.
    [runtime_policy] is the exact immutable selected moderator policy that the
    same Host gives its ChildRunner, not a caller-supplied policy grant. *)
val create
  :  live:Developer_live_resources.live
  -> root:Developer_root_owner.run
  -> runtime_policy:Chat_response.Runtime_semantics.policy
  -> (t, Agent_protocol.Error.t) result

val issued_by_root : t -> Developer_root_owner.run -> bool

(** Canonical v3 comparison digest for the selected child execution contract.
    Domain-separate the installed script/content runtime identity from the
    Host's actual moderator policy. The claimed executor recomputes this same
    function from its restored, authenticated Host package. *)
val runtime_policy_identity
  :  base:Chatml_source.Digest.t
  -> policy:Chat_response.Runtime_semantics.policy
  -> Chatml_source.Digest.t

(** Called only for the exact [Admitted_child] staging scope. The child keeps
    its physical Selected borrow and joined preparation live throughout this
    capture. Recheck it after any yield and before returning immutable data to
    the existing atomic stage. Root/Worker scopes cannot call this as a way to
    upgrade their selection. The v3 runtime comparison digest combines the
    child's actual installed script/content runtime identity with this retained
    Host moderator policy under a tagged version; the claimed executor must
    recompute it from the restored Host before Claim effects. *)
val capture_child
  :  t
  -> child:Developer_child_selection.t
  -> checkpoint:(unit -> unit)
  -> (Chat_response.Background_request.Issuer_source.t, Agent_protocol.Error.t) result
