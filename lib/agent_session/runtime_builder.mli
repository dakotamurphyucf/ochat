open! Core

(** Constructs the live ChatMD runtime shared by daemon and embedded session
    hosts. All filesystem coordinates are explicit runtime paths. *)

type moderator_drain =
  { moderator_snapshot : Jsonaf.t option
  ; runtime_requests : Chat_response.Moderation.Runtime_request.t list
  ; notifications : string list
  ; remaining_events : bool
  }

type model_job_outcome =
  | Model_succeeded of Jsonaf.t
  | Model_failed of string

type model_post_stream = Chat_response.In_memory_stream.post_stream

(** Host preparation for external queue ingress. Compare [before] and save
    [snapshot] atomically with the delivery receipt under actor ownership. An
    error leaves the live queue unchanged; success must mean durable acceptance. *)
type prepare_enqueue =
  before:Session.Moderator_state.Identity_snapshot.t
  -> snapshot:Session.Moderator_state.Identity_snapshot.t
  -> (unit, Agent_protocol.Error.t) result

type extension_services =
  { script_tools : Chat_response.Agent_runtime.t -> Script_tool_calls.t
    (** Bind shared native policy/disclosure to the exact constructed runtime.
        This service owns generic native tool approval; delegated shell tools
        still use the authorized shell runtime's policy and approval broker. *)
  ; claim_lifecycle : event:Chat_response.Moderation.Event.t -> Moderator_event.claim
    (** Actual running-idle actor ownership. Never manufacture an operation. *)
  ; lifecycle_started : Agent_protocol.Invocation.observer -> bool
    (** Whether the current source/generation has a completed lifecycle receipt.
        An initial prepared checkpoint alone does not mean startup executed. *)
  ; history : unit -> History_entry.t list
    (** Current canonical history, read after an actor claim without entering the manager. *)
  }

type moderator_activation =
  { pending : unit -> bool
  ; run : unit -> (bool, Agent_protocol.Error.t) result
  }

type t =
  { worker : Operation_worker.t
  ; parse_user_content :
      id:History_entry.Id.t
      -> Agent_protocol.Session.Message_content.t
      -> (History_entry.t, Agent_protocol.Error.t) result
  ; initial_history : History_entry.t list
  ; initial_prompt_entry_count : int
  ; reserved_history_through : int
  ; mutable moderator_snapshot : Jsonaf.t option
  ; moderator_manager : Chat_response.Moderator_manager.t option
  ; moderator_tools : Openai.Responses.Request.Tool.t list
  ; moderator_script_tools : Script_tool_calls.t option
    (** Host policy/disclosure services for v1 moderator native calls. Normal
        construction leaves this absent until v1 admission is installed. *)
  ; moderator_activation : moderator_activation option
    (** Deferred owned activation after installing the initial checkpoint. *)
  ; start_moderator : unit -> (Jsonaf.t option, Agent_protocol.Error.t) result
  ; enqueue_internal_event :
      ?prepare:prepare_enqueue
      -> Jsonaf.t
      -> (Jsonaf.t option, Agent_protocol.Error.t) result
  ; drain_internal_events :
      History_entry.t list -> (moderator_drain, Agent_protocol.Error.t) result
  ; execute_model_job :
      recipe:string
      -> payload:Jsonaf.t
      -> (model_job_outcome, Agent_protocol.Error.t) result
  ; enqueue_model_job_completion :
      ?prepare:prepare_enqueue
      -> Agent_protocol.Job.t
      -> (Jsonaf.t option, Agent_protocol.Error.t) result
  ; close : unit -> unit
  }

type schedule_services =
  { after_ms : delay_ms:int -> payload:Jsonaf.t -> (string, string) result
  ; cancel : id:string -> (unit, string) result
  }

type job_services =
  { spawn_model : recipe:string -> payload:Jsonaf.t -> (string, string) result
  ; call_model :
      recipe:string
      -> payload:Jsonaf.t
      -> execute:
           (unit
            -> (Chat_response.Moderation.Capabilities.model_call_result, string) result)
      -> (Chat_response.Moderation.Capabilities.model_call_result, string) result
  }

(** Encode an already prepared identity snapshot in the durable moderator
    envelope, without re-entering the live manager. *)
val encode_moderator_snapshot : Session.Moderator_state.Identity_snapshot.t -> Jsonaf.t

(** [moderator_snapshot_has_queued_events] inspects a durable identity snapshot
    without constructing a live runtime. *)
val moderator_snapshot_has_queued_events
  :  Jsonaf.t option
  -> (bool, Agent_protocol.Error.t) result

(** Inspect committed moderator termination without entering the live manager.
    No snapshot means false; malformed snapshots fail closed. *)
val moderator_snapshot_is_halted
  :  Jsonaf.t option
  -> (bool, Agent_protocol.Error.t) result

(** Read the installed source identity without entering the live manager. *)
val moderator_snapshot_observer
  :  Jsonaf.t option
  -> (Agent_protocol.Invocation.observer option, Agent_protocol.Error.t) result

(** [build ... ~paths ~storage_paths ...] constructs a runtime with public path
    substitutions from [paths] and private cache/response IO from [storage_paths].
    Supply the same paths outside detached administrative preparation. Initializer
    effects on external tools/files are not rolled back by session transactions. *)
val build
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> paths:Runtime_paths.t
  -> storage_paths:Runtime_paths.t
  -> revision:Prompt_revision.t
  -> session_id:Agent_protocol.Id.Session.t
  -> history_namespace:string
  -> next_history_sequence:int
  -> existing_history:History_entry.t list option
  -> existing_moderator_snapshot:Jsonaf.t option
  -> moderator_reservation_size:int
  -> manifest_authorizer:Shell_runtime.Manifest_authorizer.t
  -> approval_provider:Shell_runtime.Approval_broker.provider
  -> approval_store:Shell_access.Approval.store
  -> permission_profile:Permission_policy.t
  -> model_post_stream:model_post_stream option
  -> review_permission:
       (Permission_policy.invocation
        -> (Permission_reviewer.Decision.t, Permission_reviewer.Error.t) result)
  -> schedule_services:schedule_services
  -> job_services:job_services
  -> (t, Agent_protocol.Error.t) result

(** Construct an extensibility-aware runtime from the captured prompt revision
    and the actual authorized native resources. Installs owned foreground dispatch
    and event services. [start_moderator] returns the prepared initial checkpoint;
    startup/resume effects wait for [moderator_activation] or the first foreground
    operation, after actor installation. Standalone and background completion
    services remain unavailable until implemented. Public feature negotiation is
    unchanged; ordinary hosts continue using [build]. *)
val build_with_extensions
  :  services:extension_services
  -> sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> paths:Runtime_paths.t
  -> storage_paths:Runtime_paths.t
  -> revision:Prompt_revision.t
  -> session_id:Agent_protocol.Id.Session.t
  -> history_namespace:string
  -> next_history_sequence:int
  -> existing_history:History_entry.t list option
  -> existing_moderator_snapshot:Jsonaf.t option
  -> moderator_reservation_size:int
  -> manifest_authorizer:Shell_runtime.Manifest_authorizer.t
  -> approval_provider:Shell_runtime.Approval_broker.provider
  -> approval_store:Shell_access.Approval.store
  -> permission_profile:Permission_policy.t
  -> model_post_stream:model_post_stream option
  -> review_permission:
       (Permission_policy.invocation
        -> (Permission_reviewer.Decision.t, Permission_reviewer.Error.t) result)
  -> schedule_services:schedule_services
  -> job_services:job_services
  -> (t, Agent_protocol.Error.t) result
