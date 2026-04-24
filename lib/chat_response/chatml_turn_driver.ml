open! Core

module Moderation = Chatml_moderation
module Moderator = Chatml_moderator
module Res = Openai.Responses

module Safe_point_input = struct
  type t = In_memory_stream.Safe_point_input.t = { consume : unit -> string option }
end

type moderator =
  { manager : Moderator.t
  ; session_id : string
  ; session_meta : Jsonaf.t
  ; runtime_policy : Runtime_semantics.policy
  }

type pending_ui_request = Moderator.pending_ui_request =
  | Ask_text of { prompt : string }
  | Ask_choice of { prompt : string; choices : string array }

type moderated_tool_call =
  { call_item : Res.Item.t
  ; kind : Tool_call.Kind.t
  ; name : string
  ; payload : string
  ; synthetic_result : Res.Tool_output.Output.t option
  ; runtime_requests : Moderation.Runtime_request.t list
  }

let to_in_memory_moderator ({ manager; session_id; session_meta; runtime_policy } : moderator)
  : In_memory_stream.moderator
  =
  { manager; session_id; session_meta; runtime_policy }
;;

let map_moderator moderator = Option.map moderator ~f:to_in_memory_moderator

let of_in_memory_moderated_tool_call
      ({ call_item; kind; name; payload; synthetic_result; runtime_requests }
        : In_memory_stream.moderated_tool_call)
  : moderated_tool_call
  =
  { call_item; kind; name; payload; synthetic_result; runtime_requests }
;;

let pending_ui_request (moderator : moderator) =
  In_memory_stream.pending_ui_request (to_in_memory_moderator moderator)
;;

let resume_ui_request (moderator : moderator) ~response =
  In_memory_stream.resume_ui_request (to_in_memory_moderator moderator) ~response
;;

let prepare_turn_inputs ~moderator ?safe_point_input ~available_tools ~now_ms ~history ()
  =
  In_memory_stream.prepare_turn_inputs
    ~moderator:(map_moderator moderator)
    ?safe_point_input
    ~available_tools
    ~now_ms
    ~history
    ()
;;

let finish_turn ~moderator ~available_tools ~now_ms ~history =
  In_memory_stream.finish_turn
    ~moderator:(map_moderator moderator)
    ~available_tools
    ~now_ms
    ~history
;;

let moderate_tool_call
      ~moderator
      ~available_tools
      ~now_ms
      ~history
      ~kind
      ~name
      ~payload
      ~call_id
      ~item_id
  =
  Result.map
    (In_memory_stream.moderate_tool_call
       ~moderator:(map_moderator moderator)
       ~available_tools
       ~now_ms
       ~history
       ~kind
       ~name
       ~payload
       ~call_id
       ~item_id)
    ~f:of_in_memory_moderated_tool_call
;;

let handle_tool_result ~moderator ~available_tools ~now_ms ~history ~name ~kind ~item =
  In_memory_stream.handle_tool_result
    ~moderator:(map_moderator moderator)
    ~available_tools
    ~now_ms
    ~history
    ~name
    ~kind
    ~item
;;
