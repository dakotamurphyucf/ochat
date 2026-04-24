open! Core

module Lang = Chatml.Chatml_lang
module Runtime = Chatml_runtime
module Res = Openai.Responses

module Phase : sig
  type t = Moderation.Phase.t =
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
  type t = Moderation.Item.t =
    { id : string
    ; value : Jsonaf.t
    }
  [@@deriving sexp]

  val create : id:string -> value:Jsonaf.t -> t
  val of_value : Lang.value -> (t, string) result
  val to_value : t -> Lang.value
  val of_response_item : id:string -> Res.Item.t -> t
  val to_response_item : t -> (Res.Item.t, string) result
  val kind : t -> string option
  val role : t -> string option
  val text_parts : t -> string list
  val text : t -> string option
  val input_text_message : id:string -> role:string -> text:string -> t
  val output_text_message : id:string -> text:string -> t
  val user_text : id:string -> string -> t
  val assistant_text : id:string -> string -> t
  val system_text : id:string -> string -> t
  val notice : id:string -> text:string -> t
  val is_user : t -> bool
  val is_assistant : t -> bool
  val is_system : t -> bool
  val is_tool_call : t -> bool
  val is_tool_result : t -> bool
end

module Tool_desc : sig
  type t = Moderation.Tool_desc.t =
    { name : string
    ; description : string
    ; input_schema : Jsonaf.t
    }
  [@@deriving sexp]

  val of_request_tool : Res.Request.Tool.t -> t
  val to_value : t -> Lang.value
end

module Tool_call : sig
  type kind = Moderation.Tool_call.kind =
    | Function
    | Custom
  [@@deriving sexp, compare]

  type t = Moderation.Tool_call.t =
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
  val arg : t -> string -> Jsonaf.t option
  val arg_string : t -> string -> string option
  val arg_bool : t -> string -> bool option
  val arg_array : t -> string -> Jsonaf.t list option
  val is_named : t -> string -> bool
  val is_one_of : t -> string list -> bool
end

module Tool_result : sig
  type t = Moderation.Tool_result.t =
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
  type t = Moderation.Context.t =
    { session_id : string
    ; now_ms : int
    ; phase : Phase.t
    ; items : Item.t list
    ; available_tools : Tool_desc.t list
    ; session_meta : Jsonaf.t
    }
  [@@deriving sexp]

  val to_value : t -> Lang.value
  val last_item : t -> Item.t option
  val last_user_item : t -> Item.t option
  val last_assistant_item : t -> Item.t option
  val last_system_item : t -> Item.t option
  val last_tool_call : t -> Item.t option
  val last_tool_result : t -> Item.t option
  val find_item : t -> id:string -> Item.t option
  val items_since_last_user_turn : t -> Item.t list
  val items_since_last_assistant_turn : t -> Item.t list
  val items_by_role : t -> role:string -> Item.t list
  val find_tool : t -> name:string -> Tool_desc.t option
  val has_tool : t -> name:string -> bool
end

module Event : sig
  type t = Moderation.Event.t =
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
  type t = Moderation.Projection.t =
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
  type replacement = Moderation.Overlay.replacement =
    { target_id : string
    ; item : Item.t
    }
  [@@deriving sexp]

  type op = Moderation.Overlay.op =
    | Prepend_system of string
    | Append_item of Item.t
    | Replace_item of replacement
    | Delete_item of string
    | Halt of string
  [@@deriving sexp]

  type t = Moderation.Overlay.t =
    { prepended_system_items : Item.t list
    ; appended_items : Item.t list
    ; replacements : replacement list
    ; deleted_item_ids : string list
    ; halted_reason : string option [@jsonaf.option]
    }
  [@@deriving sexp]

  val empty : t
  val of_turn_effect : Runtime.turn_effect -> (op, string) result
  val apply : t -> Item.t list -> Item.t list
end

module Tool_moderation : sig
  type t = Moderation.Tool_moderation.t =
    | Approve
    | Reject of string
    | Rewrite_args of Jsonaf.t
    | Redirect of string * Jsonaf.t
  [@@deriving sexp]

  val of_runtime : Runtime.tool_moderation -> (t, string) result
end

module Runtime_request : sig
  type t = Moderation.Runtime_request.t =
    | Request_compaction
    | Request_turn
    | End_session of string
  [@@deriving sexp, compare]
end

module Outcome : sig
  type t = Moderation.Outcome.t =
    { overlay_ops : Overlay.op list
    ; tool_moderation : Tool_moderation.t option
    ; ui_notifications : string list
    ; runtime_requests : Runtime_request.t list
    ; emitted_events : Lang.value list
    }

  val empty : t
  val of_runtime_effects : Runtime.local_effect list -> (t, string) result
end

module Capabilities : sig
  type tool_call_result = Moderation.Capabilities.tool_call_result =
    | Tool_ok of Jsonaf.t
    | Tool_error of string
  [@@deriving sexp]

  type model_call_result = Moderation.Capabilities.model_call_result =
    | Model_ok of Jsonaf.t
    | Model_refused of string
    | Model_error of string
  [@@deriving sexp]

  type model_recipe = Moderation.Capabilities.model_recipe =
    { call : payload:Jsonaf.t -> (model_call_result, string) result
    ; spawn : payload:Jsonaf.t -> (string, string) result
    }

  type t = Moderation.Capabilities.t =
    { on_log : level:Runtime.log_level -> message:string -> (unit, string) result
    ; on_ui_notify : message:string -> (unit, string) result
    ; on_tool_call : name:string -> args:Jsonaf.t -> (tool_call_result, string) result
    ; on_tool_spawn : name:string -> args:Jsonaf.t -> (string, string) result
    ; model_recipes : model_recipe String.Map.t
    ; on_schedule_after_ms : delay_ms:int -> payload:Lang.value -> (string, string) result
    ; on_schedule_cancel : id:string -> (unit, string) result
    }

  val default : t
  val runtime_handlers : t -> Runtime.default_handlers
end
