open! Core

(** Shared request preparation for the existing Job.start_tool and
    Job.start_script grammar. This module creates reconstructible requests;
    neither a request nor a source callback grants stage/execution authority.
    The conversation and developer adapters supply their own authentic
    selected registry, policy and source-custody implementation. *)

(** Resolve [name] in [selected], validate its input and captured policy, and
    optionally attach an already installed host-verified code artifact. The
    source callback receives the exact resolved binding/request and may yield;
    [checkpoint] runs before and after it. The caller must recheck its live
    actor/selection after this operation and before physical staging. *)
val tool
  :  selected:Chat_response.Tool_capability.t
  -> policy:Chat_response.One_off_request.policy
  -> prepare_source:
       (binding:Chat_response.Tool_capability.binding
        -> request:Chat_response.Background_request.t
        -> checkpoint:(unit -> unit)
        -> ( Chat_response.Background_request.Agent_artifact.t option
             , Agent_protocol.Error.t )
             result)
  -> name:string
  -> input:Jsonaf.t
  -> checkpoint:(unit -> unit)
  -> (Chat_response.Background_request.t, Agent_protocol.Error.t) result

(** Use the single One_off_request decoder and One_off_script compiler path,
    with the exact selected registry and admitted policy. This keeps the
    conversation adapter's established compile/checkpoint sequence; a physical
    developer caller performs its own checkpoint and post-compile authority
    check before staging. Diagnostics retain their existing source owner. *)
val script
  :  env:Eio_unix.Stdenv.base
  -> selected:Chat_response.Tool_capability.t
  -> policy:Chat_response.One_off_request.policy
  -> Jsonaf.t
  -> (Chat_response.Background_request.t, Agent_protocol.Error.t) result
