(** Closed, versioned comparison DTOs for one authenticated Developer REPL
    endpoint. No Instance/Epoch/Revision/Task physical owner, source grant or
    effect capability crosses this module. All constructors and decoders apply
    the installed finite policy; the server reauthorizes every decoded call. *)

module Features : sig
  val repl : string
  val events : string
end

module Limits : sig
  type t

  (** [request], [result], [page] and [response] bound complete JSON trees,
      including IDs and framing. [page] counts a cursor repeated in [items]
      and [next] twice on the wire; the in-process page charge is not a
      transport bound. [max_registry_entries] is a server installation cap,
      admitted against the exact Host Instance/key ledger before any work. *)
  val create
    :  request:Document_schema.Limits.t
    -> result:Document_schema.Limits.t
    -> page:Document_schema.Limits.t
    -> response:Document_schema.Limits.t
    -> max_source_bytes:int
    -> max_reference_bytes:int
    -> max_output_bytes:int
    -> max_page_items:int
    -> max_event_bytes:int
    -> max_registry_entries:int
    -> (t, Error.t) result

  val request : t -> Document_schema.Limits.t
  val result : t -> Document_schema.Limits.t
  val page : t -> Document_schema.Limits.t
  val response : t -> Document_schema.Limits.t
  val max_source_bytes : t -> int
  val max_reference_bytes : t -> int
  val max_output_bytes : t -> int
  val max_page_items : t -> int
  val max_event_bytes : t -> int
  val max_registry_entries : t -> int
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Wire_id : sig
  module Instance : sig
    type t

    val of_string : string -> (t, Error.t) result
    val to_string : t -> string
    val equal : t -> t -> bool
  end

  module Epoch : sig
    type t

    val of_string : string -> (t, Error.t) result
    val to_string : t -> string
    val equal : t -> t -> bool
  end

  module Revision : sig
    type t

    val of_string : string -> (t, Error.t) result
    val to_string : t -> string
    val equal : t -> t -> bool
  end
end

module Create : sig
  type t

  (** Route/target/Prompt/Workspace and requested ceilings are canonical
      action identity, not source authority. Only Standard REPL targets are
      valid. Explicit null for an absent option rejects. *)
  val create
    :  limits:Limits.t
    -> run_id:Id.Developer_run.t
    -> key:Idempotency_key.t
    -> route:Developer_common.Route.t
    -> target:Developer_common.Target.t
    -> prompt:Id.Prompt_definition.t
    -> workspace:Id.Workspace_definition.t
    -> requested_execution:Developer_common.Execution_limits.t option
    -> requested_max_output_bytes:int option
    -> (t, Error.t) result

  val run_id : t -> Id.Developer_run.t
  val key : t -> Idempotency_key.t
  val route : t -> Developer_common.Route.t
  val target : t -> Developer_common.Target.t
  val prompt : t -> Id.Prompt_definition.t
  val workspace : t -> Id.Workspace_definition.t
  val requested_execution : t -> Developer_common.Execution_limits.t option
  val requested_max_output_bytes : t -> int option
  val to_json : t -> Jsonaf.t
  val of_json : limits:Limits.t -> Jsonaf.t -> (t, Error.t) result
end

module Submit : sig
  type t

  val create
    :  limits:Limits.t
    -> instance:Wire_id.Instance.t
    -> epoch:Wire_id.Epoch.t
    -> expected_revision:Wire_id.Revision.t
    -> key:Idempotency_key.t
    -> source:string
    -> (t, Error.t) result

  val instance : t -> Wire_id.Instance.t
  val epoch : t -> Wire_id.Epoch.t
  val expected_revision : t -> Wire_id.Revision.t
  val key : t -> Idempotency_key.t
  val source : t -> string
  val to_json : t -> Jsonaf.t
  val of_json : limits:Limits.t -> Jsonaf.t -> (t, Error.t) result
end

module Lookup : sig
  type t

  val create : instance:Wire_id.Instance.t -> key:Idempotency_key.t -> t
  val instance : t -> Wire_id.Instance.t
  val key : t -> Idempotency_key.t
  val to_json : t -> Jsonaf.t
  val of_json : limits:Limits.t -> Jsonaf.t -> (t, Error.t) result
end

module Current_epoch : sig
  type t

  val create
    :  instance:Wire_id.Instance.t
    -> epoch:Wire_id.Epoch.t
    -> key:Idempotency_key.t
    -> t

  val instance : t -> Wire_id.Instance.t
  val epoch : t -> Wire_id.Epoch.t
  val key : t -> Idempotency_key.t
  val to_json : t -> Jsonaf.t
  val of_json : limits:Limits.t -> Jsonaf.t -> (t, Error.t) result
end

module Type_selector : sig
  (** [Binding_index] selects this statement's declared value bindings, zero-based
      in compiler declaration order, excluding builtins and earlier statements.
      Expressions, types, opens and failed runtime statements have none. *)
  type t =
    | Expression
    | Binding_index of int
  [@@deriving equal, sexp_of]
end

module Inspect : sig
  type t

  val create
    :  limits:Limits.t
    -> current:Current_epoch.t
    -> statement_index:int
    -> selector:Type_selector.t
    -> max_bytes:int
    -> (t, Error.t) result

  val current : t -> Current_epoch.t
  val statement_index : t -> int
  val selector : t -> Type_selector.t
  val max_bytes : t -> int
  val to_json : t -> Jsonaf.t
  val of_json : limits:Limits.t -> Jsonaf.t -> (t, Error.t) result
end

module Reference_page_request : sig
  type t

  val create
    :  limits:Limits.t
    -> current:Current_epoch.t
    -> after:string option
    -> limit:int
    -> (t, Error.t) result

  val current : t -> Current_epoch.t
  val after : t -> string option
  val limit : t -> int
  val to_json : t -> Jsonaf.t
  val of_json : limits:Limits.t -> Jsonaf.t -> (t, Error.t) result
end

module Task_submit : sig
  type t

  (** [fresh_epoch] is comparison data for a true new-key action. The Host
      checks durable run/key/original history first; an exact old-key retry
      never resolves these possibly stale process-local IDs. *)
  val create
    :  limits:Limits.t
    -> key:Idempotency_key.t
    -> reference:string
    -> requested_execution:Developer_common.Execution_limits.t option
    -> requested_max_output_bytes:int option
    -> fresh_epoch:(Wire_id.Instance.t * Wire_id.Epoch.t) option
    -> (t, Error.t) result

  val key : t -> Idempotency_key.t
  val reference : t -> string
  val requested_execution : t -> Developer_common.Execution_limits.t option
  val requested_max_output_bytes : t -> int option
  val fresh_epoch : t -> (Wire_id.Instance.t * Wire_id.Epoch.t) option
  val to_json : t -> Jsonaf.t
  val of_json : limits:Limits.t -> Jsonaf.t -> (t, Error.t) result
end

module View : sig
  type t

  val create
    :  limits:Limits.t
    -> instance:Wire_id.Instance.t
    -> epoch:Wire_id.Epoch.t
    -> revision:Wire_id.Revision.t
    -> run_id:Id.Developer_run.t
    -> capture_fingerprint:Chatml_source.Digest.t
    -> (t, Error.t) result

  val instance : t -> Wire_id.Instance.t
  val epoch : t -> Wire_id.Epoch.t
  val revision : t -> Wire_id.Revision.t
  val run_id : t -> Id.Developer_run.t
  val capture_fingerprint : t -> Chatml_source.Digest.t
  val to_json : t -> Jsonaf.t
  val of_json : limits:Limits.t -> Jsonaf.t -> (t, Error.t) result
end

module Submission : sig
  type t

  module Location : sig
    type t

    (** Byte coordinates and exact source revision for one already owned
        diagnostic. This comparison DTO does not authenticate source bytes or
        grant read access. The server projects a genuine Source.Location. *)
    val create
      :  limits:Limits.t
      -> source:Chatml_source.Id.t
      -> digest:Chatml_source.Digest.t
      -> start_byte:int
      -> end_byte:int
      -> (t, Error.t) result

    val source : t -> Chatml_source.Id.t
    val digest : t -> Chatml_source.Digest.t
    val start_byte : t -> int
    val end_byte : t -> int
  end

  module Rendered_type : sig
    type t

    type kind =
      | Type
      | Signature
    [@@deriving equal, sexp_of]

    type truncation =
      | Node_limit
      | Edge_limit
      | Depth_limit
      | Work_limit
      | Byte_limit
    [@@deriving equal, sexp_of]

    type completeness =
      | Complete
      | Truncated of truncation
    [@@deriving equal, sexp_of]

    val create
      :  limits:Limits.t
      -> kind:kind
      -> completeness:completeness
      -> text:string
      -> (t, Error.t) result

    val kind : t -> kind
    val completeness : t -> completeness
    val text : t -> string
  end

  module Diagnostic : sig
    type t

    val create
      :  limits:Limits.t
      -> message:string
      -> location:Location.t option
      -> (t, Error.t) result

    val message : t -> string
    val location : t -> Location.t option
  end

  module Summary : sig
    (** Bounded scalar outcome, preview, diagnostic, rendered type and Task
        retention. No frozen graph, value or physical Task handle. *)
    type t

    type failure_kind =
      | Initializer
      | Runtime
      | Budget
    [@@deriving equal, sexp_of]

    type outcome =
      | Succeeded
      | Failed of failure_kind
    [@@deriving equal, sexp_of]

    type task_retention =
      | Not_a_task
      | Retained
      | Limit_reached
    [@@deriving equal, sexp_of]

    val create
      :  limits:Limits.t
      -> outcome:outcome
      -> value_preview:string option
      -> diagnostic:Diagnostic.t option
      -> expression_type:Rendered_type.t option
      -> binding_count:int
      -> task_retention:task_retention
      -> text_limited:bool
      -> (t, Error.t) result

    val outcome : t -> outcome
    val value_preview : t -> string option
    val diagnostic : t -> Diagnostic.t option
    val expression_type : t -> Rendered_type.t option
    val binding_count : t -> int
    val task_retention : t -> task_retention
    val text_limited : t -> bool
  end

  type need_more_reason =
    | Parser_eof
    | Comment_eof
    | String_eof

  type status =
    | Need_more of need_more_reason * Location.t option
    | Rejected of Diagnostic.t
    | Finished
    | Stopped of Diagnostic.t option

  type details =
    | Available
    | Output_limited

  val create
    :  limits:Limits.t
    -> status:status
    -> details:details
    -> completed_statements:int
    -> statement_count:int
    -> epoch:Wire_id.Epoch.t
    -> revision:Wire_id.Revision.t
    -> summaries:Summary.t list
    -> task_reference_count:int
    -> task_references:string list
    -> (t, Error.t) result

  val epoch : t -> Wire_id.Epoch.t
  val revision : t -> Wire_id.Revision.t
  val status : t -> status
  val details : t -> details
  val completed_statements : t -> int
  val statement_count : t -> int
  val summaries : t -> Summary.t list
  val task_reference_count : t -> int
  val task_references : t -> string list
  val to_json : t -> Jsonaf.t
  val of_json : limits:Limits.t -> Jsonaf.t -> (t, Error.t) result
end

module Type_inspection : sig
  type t

  val create
    :  limits:Limits.t
    -> revision:Wire_id.Revision.t
    -> binding_name:string option
    -> rendered:Submission.Rendered_type.t
    -> (t, Error.t) result

  val revision : t -> Wire_id.Revision.t
  val binding_name : t -> string option
  val rendered : t -> Submission.Rendered_type.t
  val to_json : t -> Jsonaf.t
  val of_json : limits:Limits.t -> Jsonaf.t -> (t, Error.t) result
end

module Reference_page : sig
  (** Encoded [items] and [next] each count toward [Limits.page], including
      the repeated cursor string. A nonempty remaining page must advance. *)
  type t

  val create
    :  limits:Limits.t
    -> items:string list
    -> next:string option
    -> total:int
    -> (t, Error.t) result

  val items : t -> string list
  val next : t -> string option
  val total : t -> int
  val to_json : t -> Jsonaf.t
  val of_json : limits:Limits.t -> Jsonaf.t -> (t, Error.t) result
end

module Task_view : sig
  module Diagnostic : sig
    type t

    val create
      :  limits:Limits.t
      -> code:string
      -> message:string
      -> location:Submission.Location.t option
      -> (t, Error.t) result

    val code : t -> string
    val message : t -> string
    val location : t -> Submission.Location.t option
  end

  type outcome =
    | Succeeded of string option
    | Failed of
        { error : Error.t
        ; diagnostic : Diagnostic.t option
        }
    | Limited of
        { error : Error.t
        ; diagnostic : Diagnostic.t option
        }
    | Interrupted
    | Cancelled

  type phase =
    | Reserved
    | Waiting_permission
    | Execution_started
    | Terminal of
        { execution_started : bool
        ; outcome : outcome
        }

  (** This is only an authenticated Repl_task_run projection. The ordinary
      Developer_common.View decoder deliberately excludes Task rows. *)
  type t

  val create
    :  limits:Limits.t
    -> command_id:Developer_command_id.t
    -> owner:Developer_common.Owner.t
    -> key:Idempotency_key.t
    -> phase:phase
    -> request_fingerprint:Chatml_source.Digest.t
    -> capture_fingerprint:Chatml_source.Digest.t
    -> (t, Error.t) result

  val command_id : t -> Developer_command_id.t
  val owner : t -> Developer_common.Owner.t
  val key : t -> Idempotency_key.t
  val phase : t -> phase
  val request_fingerprint : t -> Chatml_source.Digest.t
  val capture_fingerprint : t -> Chatml_source.Digest.t
  val to_json : t -> Jsonaf.t
  val of_json : limits:Limits.t -> Jsonaf.t -> (t, Error.t) result
end

module Capabilities : sig
  type t

  val create : methods:string list -> limits:Limits.t -> (t, Error.t) result
  val supports : t -> string -> bool
  val limits : t -> Limits.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Watch_key : sig
  type t =
    | Repl of Lookup.t
    | Task of Developer_common.Lookup.t

  val to_json : t -> Jsonaf.t
  val of_json : limits:Limits.t -> Jsonaf.t -> (t, Error.t) result
end

module Request : sig
  type t =
    | Capabilities
    | Create of Create.t
    | Submit of Submit.t
    | Observe of Lookup.t
    | Cancel_submission of Lookup.t
    | Inspect_type of Inspect.t
    | Task_references_page of Reference_page_request.t
    | Task_submit of Task_submit.t
    | Task_observe of Developer_common.Lookup.t
    | Task_await_terminal of Developer_common.Lookup.t
    | Task_cancel_and_wait of Developer_common.Lookup.t
    | Reset_and_wait of Current_epoch.t
    | Close_and_wait of Lookup.t
    | Watch of Watch_key.t
    | Unwatch of Watch_key.t

  val method_name : t -> string
  val params : t -> Jsonaf.t

  val of_method_and_params
    :  limits:Limits.t
    -> method_:string
    -> params:Jsonaf.t
    -> (t, Error.t) result

  val supported_methods : string list
end

module Response : sig
  type t =
    | Capabilities of Capabilities.t
    | Create of View.t
    | Submit of Submission.t
    | Observe of Submission.t option
    | Cancel_submission of Submission.t
    | Inspect_type of Type_inspection.t
    | Task_references_page of Reference_page.t
    | Task_submit of Task_view.t
    | Task_observe of Task_view.t option
    | Task_await_terminal of Task_view.t
    | Task_cancel_and_wait of Task_view.t
    | Reset_and_wait of View.t
    | Close_and_wait
    | Watch
    | Unwatch

  val method_name : t -> string
  val to_json : t -> limits:Limits.t -> (Jsonaf.t, Error.t) result

  val of_method_and_json
    :  limits:Limits.t
    -> method_:string
    -> Jsonaf.t
    -> (t, Error.t) result
end

module Change : sig
  type kind =
    | Changed
    | Resync_required
  [@@deriving equal, sexp_of]

  type t

  val create : limits:Limits.t -> key:Watch_key.t -> kind:kind -> (t, Error.t) result
  val key : t -> Watch_key.t
  val kind : t -> kind
  val method_name : string
  val to_json : t -> Jsonaf.t
  val of_json : limits:Limits.t -> Jsonaf.t -> (t, Error.t) result
end
