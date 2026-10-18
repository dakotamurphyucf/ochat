(** Versioned, transport-neutral Developer request, view and capability data.
    All source selectors, fingerprints, IDs and owner coordinates are data, not
    a source read, execution, approval or cancellation grant. Every method must
    be reauthorized by the physical Host after decoding and after yields.
    Unknown/duplicate fields and explicit null for absent options reject. *)

module Features : sig
  val run : string
  val approvals : string
  val events : string
end

module Method : sig
  type t =
    | Capabilities
    | Submit
    | Observe
    | Await_terminal
    | Cancel_and_wait
    | Approval_list
    | Approval_respond
    | Approval_cancel
    | Watch
    | Unwatch
  [@@deriving compare, equal, sexp_of]

  val to_string : t -> string
  val of_string : string -> (t, Error.t) result
end

module Kind = Developer_receipt.Kind

module Target : sig
  type t =
    | One_off_v1
    | Tool_v1
    | Developer_tool_v1
    | Moderator_v1
    | Delegated_moderator_v1
  [@@deriving compare, equal, sexp_of]

  (** Closed pure counterpart of the existing compiler Target. The server
      converts only after its actual captured compiler ABI is selected. *)
  val to_string : t -> string

  val of_string : string -> (t, Error.t) result
end

module Route : sig
  type t = private
    | Local_project of
        { root_index : int
        ; dependency_root_indices : int list
        ; project : Source.Portable_path.t
        ; selection : Chatml_project.Selection.request
        }
    | Local_source of
        { root_index : int
        ; member : Source.Portable_path.t
        }
    | Authoring_project of { expected : Authoring.Context.Expected.t }
    | Document_script of
        { expected : Authoring.Context.Expected.t
        ; selector : Authoring.Analysis.Target.selector
        }

  (** Indices and Expected are stable requested selectors. A fresh Host
      operation must borrow the actual configured roots or current Authoring
      connection only after its authoritative historical key miss. *)
  val local_project
    :  limits:Document_schema.Limits.t
    -> root_index:int
    -> dependency_root_indices:int list
    -> project:Source.Portable_path.t
    -> selection:Chatml_project.Selection.request
    -> (t, Error.t) result

  val local_source
    :  limits:Document_schema.Limits.t
    -> root_index:int
    -> member:Source.Portable_path.t
    -> (t, Error.t) result

  val authoring_project
    :  limits:Document_schema.Limits.t
    -> expected:Authoring.Context.Expected.t
    -> (t, Error.t) result

  val document_script
    :  limits:Document_schema.Limits.t
    -> expected:Authoring.Context.Expected.t
    -> selector:Authoring.Analysis.Target.selector
    -> (t, Error.t) result

  val to_json : t -> Jsonaf.t
  val of_json : limits:Document_schema.Limits.t -> Jsonaf.t -> (t, Error.t) result
end

module Fixture : sig
  module Response : sig
    type t =
      | Return of Jsonaf.t
      | Fail of string
  end

  module Step : sig
    type t

    (** Pure finite JSON and scalar validation only. The Host later performs
        the existing selected Tool schema/ChatML conversion checks. *)
    val create
      :  limits:Document_schema.Limits.t
      -> tool_name:string
      -> input_schema:Jsonaf.t
      -> output_schema:Jsonaf.t
      -> expected_input:Jsonaf.t
      -> response:Response.t
      -> (t, Error.t) result

    val tool_name : t -> string
    val input_schema : t -> Jsonaf.t
    val output_schema : t -> Jsonaf.t
    val expected_input : t -> Jsonaf.t
    val response : t -> Response.t
  end

  type t

  (** [max_steps] and [max_bytes] are finite effective host limits; empty
      fixtures are valid. Aggregate admission includes every step and nested
      JSON. Decoding applies the same checks. *)
  val create
    :  limits:Document_schema.Limits.t
    -> max_steps:int
    -> max_bytes:int
    -> steps:Step.t list
    -> (t, Error.t) result

  val steps : t -> Step.t list
  val to_json : t -> Jsonaf.t

  val of_json
    :  limits:Document_schema.Limits.t
    -> max_steps:int
    -> max_bytes:int
    -> Jsonaf.t
    -> (t, Error.t) result
end

module Execution_limits : sig
  type t = private
    { fuel : int
    ; max_tasks : int
    ; wall_seconds : float
    ; max_value_bytes : int
    ; max_array_items : int
    ; max_depth : int
    ; allocation_bytes : int
    ; max_calls : int
    ; max_invocation_depth : int
    }

  (** Pure counterpart of Chatml_execution.valid_limits within the portable
      wire-integer domain; it does not grant execution or widen the Host's
      installed budget. *)
  val create
    :  fuel:int
    -> max_tasks:int
    -> wall_seconds:float
    -> max_value_bytes:int
    -> max_array_items:int
    -> max_depth:int
    -> allocation_bytes:int
    -> max_calls:int
    -> max_invocation_depth:int
    -> (t, Error.t) result

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Limits : sig
  type t

  (** Effective installed finite ceilings. [request] and [receipt] include
      complete nested JSON bytes/depth/fields/nodes; output is separately
      bounded by the actual Host and may be lower than [receipt]. [response]
      admits the complete method result/View framing around a receipt or an
      approval page. The installed Host chooses adequate headroom; a result
      exceeding it rejects before serialization, never truncates. *)
  val create
    :  request:Document_schema.Limits.t
    -> receipt:Document_schema.Limits.t
    -> response:Document_schema.Limits.t
    -> approval_page:Document_schema.Limits.t
    -> max_output_bytes:int
    -> max_fixture_steps:int
    -> max_fixture_bytes:int
    -> max_approval_display_bytes:int
    -> max_approval_decision_reason_bytes:int
    -> max_approval_page:int
    -> max_event_bytes:int
    -> (t, Error.t) result

  val request : t -> Document_schema.Limits.t
  val receipt : t -> Document_schema.Limits.t
  val response : t -> Document_schema.Limits.t
  val approval_page : t -> Document_schema.Limits.t
  val max_output_bytes : t -> int
  val max_fixture_steps : t -> int
  val max_fixture_bytes : t -> int
  val max_approval_display_bytes : t -> int
  val max_approval_decision_reason_bytes : t -> int
  val max_approval_page : t -> int
  val max_event_bytes : t -> int
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Capabilities : sig
  type t

  (** [format_version] is this DTO's supported schema version (currently 1),
      separate from Initialize.selected_version. Methods are the actual
      installed Host handlers for this authenticated connection, never the
      codec inventory. The server must additionally enforce selected Features.
      Construction is pure comparison data and does not install a handler.
      [create] and [of_json] share a fixed 64 KiB bootstrap Document_schema
      bound before reading advertised limits; a peer cannot choose its own
      decode budget. *)
  val max_json_bytes : int

  val create
    :  format_version:int
    -> methods:string list
    -> limits:Limits.t
    -> (t, Error.t) result

  val format_version : t -> int
  val supports : t -> Method.t -> bool
  val limits : t -> Limits.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Submit : sig
  type t

  (** All fields enter the original action/payload fingerprint; source bytes
      and a later capture fingerprint do not. Live requires Prompt and
      Workspace IDs; Check/Fixture require both absent. The Host separately
      validates the exact current source, compiler, schema and policies. *)
  val create
    :  limits:Limits.t
    -> run_id:Id.Developer_run.t
    -> key:Idempotency_key.t
    -> kind:Kind.t
    -> route:Route.t
    -> target:Target.t
    -> prompt:Id.Prompt_definition.t option
    -> workspace:Id.Workspace_definition.t option
    -> entrypoint:string
    -> input:Jsonaf.t
    -> fixtures:Fixture.t option
    -> requested_execution:Execution_limits.t option
    -> requested_max_output_bytes:int option
    -> (t, Error.t) result

  val run_id : t -> Id.Developer_run.t
  val key : t -> Idempotency_key.t
  val kind : t -> Kind.t
  val route : t -> Route.t
  val target : t -> Target.t
  val prompt : t -> Id.Prompt_definition.t option
  val workspace : t -> Id.Workspace_definition.t option
  val entrypoint : t -> string
  val input : t -> Jsonaf.t
  val fixtures : t -> Fixture.t option
  val requested_execution : t -> Execution_limits.t option
  val requested_max_output_bytes : t -> int option
  val to_json : t -> Jsonaf.t
  val of_json : limits:Limits.t -> Jsonaf.t -> (t, Error.t) result
end

module Lookup : sig
  type t = private
    { run_id : Id.Developer_run.t
    ; key : Idempotency_key.t
    }

  val create : run_id:Id.Developer_run.t -> key:Idempotency_key.t -> t
  val to_json : t -> Jsonaf.t
  val of_json : limits:Document_schema.Limits.t -> Jsonaf.t -> (t, Error.t) result
end

module Owner : sig
  type t = Execution_owner.developer

  (** Delegates to the existing Execution_owner.developer constructor and
      validator. These comparison coordinates do not prove a live process. *)
  val create
    :  host_id:Id.Server.t
    -> run_id:Id.Developer_run.t
    -> epoch:int
    -> principal_id:Id.Principal.t
    -> (t, Error.t) result

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module View : sig
  type phase =
    | Reserved
    | Waiting_permission
    | Execution_started
    | Terminal of
        { execution_started : bool
        ; receipt : Developer_receipt.t
        }

  type t

  (** A historical row proves its exact request and capture through distinct
      canonical fingerprints, not recoverable raw input/environment bytes.
      [Waiting_permission] is solely the pre-Start phase. A runtime shell
      approval leaves the command [Execution_started]. Kind, phase and
      receipt must agree as in the existing Store Row. *)
  val create
    :  limits:Limits.t
    -> command_id:Developer_command_id.t
    -> owner:Owner.t
    -> key:Idempotency_key.t
    -> kind:Kind.t
    -> phase:phase
    -> request_fingerprint:Chatml_source.Digest.t
    -> capture_fingerprint:Chatml_source.Digest.t
    -> (t, Error.t) result

  val command_id : t -> Developer_command_id.t
  val owner : t -> Owner.t
  val key : t -> Idempotency_key.t
  val kind : t -> Kind.t
  val phase : t -> phase
  val request_fingerprint : t -> Chatml_source.Digest.t
  val capture_fingerprint : t -> Chatml_source.Digest.t
  val to_json : t -> Jsonaf.t
  val of_json : limits:Limits.t -> Jsonaf.t -> (t, Error.t) result
end
