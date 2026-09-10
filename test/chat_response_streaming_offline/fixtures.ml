open Core
module CM = Prompt.Chat_markdown
module Manager = Chat_response.Moderator_manager
module Res = Openai.Responses
module Stream = Chat_response.In_memory_stream

let ok_or_fail = Result.ok_or_failwith
let input_text text = Res.Input_message.Text { text; _type = "input_text" }

let output_text text =
  { Res.Output_message.annotations = []; text; _type = "output_text" }
;;

let stream_function_call ~output_index ~item_id ~call_id ~arguments =
  ( Res.Response_stream.Output_item_added
      { item =
          Function_call
            { name = "echo"
            ; arguments = ""
            ; call_id
            ; _type = "function_call"
            ; id = Some item_id
            ; status = Some "in_progress"
            }
      ; output_index
      ; type_ = "response.output_item.added"
      }
  , Res.Response_stream.Function_call_arguments_done
      { arguments
      ; item_id
      ; output_index
      ; type_ = "response.function_call_arguments.done"
      } )
;;

let input_entry allocator =
  History_entry.create
    ~allocator
    (Res.Item.Input_message
       { role = User; content = [ input_text "hello" ]; _type = "message" })
  |> Result.ok_or_failwith
;;

let entry_kind entry =
  match History_entry.item entry with
  | Res.Item.Input_message _ -> "input"
  | Function_call _ -> "function-call"
  | Custom_tool_call _ -> "custom-call"
  | Function_call_output _ -> "function-output"
  | Custom_tool_call_output _ -> "custom-output"
  | Output_message _ -> "message"
  | _ -> "other"
;;

let moderator_of_source
      ?(surface = Chatml.Chatml_builtin_surface.moderator_surface)
      ?(runtime_policy = Chat_response.Runtime_semantics.default_policy)
      source
  =
  let script =
    CM.{ id = "main"; language = "chatml"; kind = "moderator"; source = Inline source }
  in
  let artifact =
    ok_or_fail (Manager.Registry.compile_script ~surface Manager.Registry.empty script)
    |> snd
  in
  let capabilities = Chat_response.Moderation.Capabilities.default in
  let manager = ok_or_fail (Manager.create ~artifact ~capabilities ()) in
  Stream.
    { manager
    ; session_id = "session-1"
    ; session_meta = `Null
    ; runtime_policy
    ; event_handlers = None
    }
;;
