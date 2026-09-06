open! Core

(** Read-only projection and revocation of generic and shell-runtime grants
    through the common agent-protocol security model. *)

type update =
  | Generic of Agent_protocol.Grant.t
  | Shell of Session.Shell_state.t * Agent_protocol.Grant.t

(** [list] returns generic invocation grants together with redacted shell
    approval and exact-manifest grants. Shell grant identifiers are stable
    opaque projections of their persisted runtime identifiers. *)
val list
  :  now:Agent_protocol.Timestamp.t
  -> session_id:Agent_protocol.Id.Session.t
  -> creating_principal:Agent_protocol.Id.Principal.t option
  -> generic:Agent_protocol.Grant.t list
  -> shell:Session.Shell_state.t
  -> Agent_protocol.Grant.t list

(** [project_manifest] returns the redacted common-protocol projection of one
    persisted exact shell-manifest grant. *)
val project_manifest
  :  now:Agent_protocol.Timestamp.t
  -> session_id:Agent_protocol.Id.Session.t
  -> creating_principal:Agent_protocol.Id.Principal.t option
  -> Session.Shell_state.Manifest_grant.persisted
  -> Agent_protocol.Grant.t

(** [revoke] resolves [grant_id] across generic and shell grants, rejects an
    inactive grant, and returns the exact durable replacement to commit. *)
val revoke
  :  now:Agent_protocol.Timestamp.t
  -> reason:string
  -> session_id:Agent_protocol.Id.Session.t
  -> creating_principal:Agent_protocol.Id.Principal.t option
  -> generic:Agent_protocol.Grant.t list
  -> shell:Session.Shell_state.t
  -> Agent_protocol.Id.Grant.t
  -> (update, Agent_protocol.Error.t) result
