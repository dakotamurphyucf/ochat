type t =
  { entry_id : History_entry.Id.t option
  ; invocation_id : string option
  ; parent_call_id : string option
  ; event : Openai.Responses.Response_stream.t
  }

let outer ?entry_id event =
  { entry_id; invocation_id = None; parent_call_id = None; event }
;;

let fork ?entry_id ~invocation_id ~parent_call_id event =
  { entry_id
  ; invocation_id = Some invocation_id
  ; parent_call_id = Some parent_call_id
  ; event
  }
;;
