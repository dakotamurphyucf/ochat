open! Core
module Lang = Chatml.Chatml_lang
module Runtime = Chatml_moderator_runtime
module Res = Openai.Responses

(** Shared host-side moderation types and adapters.

    This module sits between driver history/tool data and the generic
    {!module:Chatml_moderator_runtime}.  It defines:

    - the v1 lifecycle phase vocabulary,
    - projection helpers from {!Openai.Responses.Item.t} to ChatML
      moderator [context] records,
    - a durable overlay snapshot shape,
    - structured interpretation of committed moderator local effects, and
    - host capability adapters for tool/model/scheduler callbacks.

    Embedders should keep local transactional effects transactional:
    instead of mutating driver state from runtime callbacks, read
    {!Runtime.committed_local_effects}, decode them with
    {!Runtime.decode_local_effects}, and interpret the resulting
    structured outcomes after {!Runtime.handle_event} succeeds. *)

module Phase : sig
  type t =
    | Session_start
    | Session_resume
    | Turn_start
    | Message_appended
    | Pre_tool_call
    | Post_tool_response
    | Turn_end
    | Internal_event
  [@@deriving sexp, compare]

  val to_string : t -> string
  val of_string : string -> (t, string) result
end

module Item : sig
  type t =
    { id : string
    ; value : Jsonaf.t
    }
  [@@deriving sexp]

  val create : id:string -> value:Jsonaf.t -> t
  val of_value : Lang.value -> (t, string) result
  val to_value : t -> Lang.value
  val of_response_item : id:string -> Res.Item.t -> t
  val to_response_item : t -> (Res.Item.t, string) result
  val text_input_message : id:string -> role:Res.Input_message.role -> text:string -> t
end

module Tool_desc : sig
  type t =
    { name : string
    ; description : string
    ; input_schema : Jsonaf.t
    }
  [@@deriving sexp]

  val of_request_tool : Res.Request.Tool.t -> t
  val to_value : t -> Lang.value
end

module Tool_call : sig
  type kind =
    | Function
    | Custom
  [@@deriving sexp, compare]

  type t =
    { id : string
    ; name : string
    ; args : Jsonaf.t
    ; kind : kind
    ; payload_text : string
    ; meta : Jsonaf.t
    }
  [@@deriving sexp]

  val of_response_item : Res.Item.t -> t option
  val to_value : t -> Lang.value
end

module Tool_result : sig
  type t =
    { call_id : string
    ; name : string
    ; result : Jsonaf.t
    ; kind : Tool_call.kind
    ; raw_output : string option
    ; meta : Jsonaf.t
    }
  [@@deriving sexp]

  val of_output_item : name:string -> kind:Tool_call.kind -> Res.Item.t -> t option
  val to_value : t -> Lang.value
end

module Context : sig
  type t =
    { session_id : string
    ; now_ms : int
    ; phase : Phase.t
    ; items : Item.t list
    ; available_tools : Tool_desc.t list
    ; session_meta : Jsonaf.t
    }
  [@@deriving sexp]

  val to_value : t -> Lang.value
end

module Event : sig
  type t =
    | Session_start
    | Session_resume
    | Turn_start
    | Item_appended of Item.t
    | Pre_tool_call of Tool_call.t
    | Post_tool_response of Tool_result.t
    | Turn_end
    | Internal_event of Lang.value

  val phase : t -> Phase.t
  val to_value : t -> Lang.value
end

module Projection : sig
  type t =
    { item_ids : string list
    ; next_generated_id : int
    }
  [@@deriving sexp, compare]

  val empty : t
  val project_item : t -> Res.Item.t -> t * Item.t
  val project_history : t -> Res.Item.t list -> t * Item.t list

  val project_context
    :  projection:t
    -> session_id:string
    -> now_ms:int
    -> phase:Phase.t
    -> history:Res.Item.t list
    -> available_tools:Res.Request.Tool.t list
    -> session_meta:Jsonaf.t
    -> t * Context.t
end

module Overlay : sig
  type replacement =
    { target_id : string
    ; item : Item.t
    }
  [@@deriving sexp]

  type op =
    | Prepend_system of string
    | Append_item of Item.t
    | Replace_item of replacement
    | Delete_item of string
    | Halt of string
  [@@deriving sexp]

  type t =
    { prepended_system_items : Item.t list
    ; appended_items : Item.t list
    ; replacements : replacement list
    ; deleted_item_ids : string list
    ; halted_reason : string option [@jsonaf.option]
    }
  [@@deriving sexp]

  val empty : t
  val of_runtime_turn_effect : Runtime.turn_effect -> (op, string) result
  val apply : t -> Item.t list -> Item.t list
end

module Tool_moderation : sig
  type t =
    | Approve
    | Reject of string
    | Rewrite_args of Jsonaf.t
    | Redirect of string * Jsonaf.t
  [@@deriving sexp]

  val of_runtime : Runtime.tool_moderation -> (t, string) result
end

module Runtime_request : sig
  type t =
    | Request_compaction
    | Request_turn
    | End_session of string
  [@@deriving sexp, compare]
end

module Outcome : sig
  type t =
    { overlay_ops : Overlay.op list
    ; tool_moderation : Tool_moderation.t option
    ; runtime_requests : Runtime_request.t list
    ; emitted_events : Lang.value list
    }

  val empty : t
  val of_runtime_effects : Runtime.local_effect list -> (t, string) result
end

module Capabilities : sig
  type tool_call_result =
    | Tool_ok of Jsonaf.t
    | Tool_error of string
  [@@deriving sexp]

  type model_call_result =
    | Model_ok of Jsonaf.t
    | Model_refused of string
    | Model_error of string
  [@@deriving sexp]

  type model_recipe =
    { call : payload:Jsonaf.t -> (model_call_result, string) result
    ; spawn : payload:Jsonaf.t -> (string, string) result
    }

  type t =
    { on_log : level:Runtime.log_level -> message:string -> (unit, string) result
    ; on_tool_call : name:string -> args:Jsonaf.t -> (tool_call_result, string) result
    ; on_tool_spawn : name:string -> args:Jsonaf.t -> (string, string) result
    ; model_recipes : model_recipe String.Map.t
    ; on_schedule_after_ms : delay_ms:int -> payload:Lang.value -> (string, string) result
    ; on_schedule_cancel : id:string -> (unit, string) result
    }

  val default : t

  (** [runtime_handlers t] builds the external/diagnostic callback bundle
      passed to {!Runtime.default_runtime_config}.

      Local transactional effects intentionally keep their default no-op
      handlers; callers should instead inspect committed effects after a
      successful {!Runtime.handle_event}. *)
  val runtime_handlers : t -> Runtime.default_handlers
end
