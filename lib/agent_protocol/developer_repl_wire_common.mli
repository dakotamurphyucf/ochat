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

(** Shared strict field decoding for the private REPL codecs. *)
val get
  :  Json_codec.fields
  -> string
  -> (Jsonaf.t -> ('a, Error.t) result)
  -> ('a, Error.t) result

val optional
  :  Json_codec.fields
  -> string
  -> (Jsonaf.t -> ('a, Error.t) result)
  -> ('a option, Error.t) result

val supported_method_names : string list
