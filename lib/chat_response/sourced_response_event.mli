(** A streamed response event with transient source attribution.

    [parent_call_id = None] identifies the outer response. [Some call_id]
    identifies activity produced by the fork tool call [call_id].
    [invocation_id] scopes one nested execution independently of [call_id].
    Source attribution is not canonical response history. *)
type t =
  { entry_id : History_entry.Id.t option
  ; invocation_id : string option
  ; parent_call_id : string option
  ; event : Openai.Responses.Response_stream.t
  }

val outer : ?entry_id:History_entry.Id.t -> Openai.Responses.Response_stream.t -> t

val fork
  :  ?entry_id:History_entry.Id.t
  -> invocation_id:string
  -> parent_call_id:string
  -> Openai.Responses.Response_stream.t
  -> t
