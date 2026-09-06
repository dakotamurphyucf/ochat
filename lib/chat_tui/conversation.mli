open Types
module Res_item = Openai.Responses.Item

(** Convert response items to display text and identity-bearing projections
    without I/O. Tool-output tuples retain at most 10,000 sanitized bytes plus
    a truncation marker; that byte cut is not grapheme-aware. This display
    policy does not bound canonical history, specialized renderers or exports,
    and is not secret redaction. *)

(** [pair_of_item item] converts a supported response item into a display
    message. Input text and call arguments use {!Util.sanitize} [~strip:true];
    assistant, reasoning and tool output use [~strip:false].

    @return [Some (role, content)] for items that carry textual content
            and [None] for artefacts that cannot be shown in the chat
            transcript (e.g. progress markers).

    {2 Example}
    Mapping a complete user message into the UI format:
    {[
      let open Openai.Responses in
      let item =
        Item.Input_message
          { Input_message.role = Input_message.User
          ; content = [ Input_message.Text { text = "Hi"; _type = "input_text" } ]
          ; _type = "message"
          }
      in
      Chat_tui.Conversation.pair_of_item item
    ]} *)
val pair_of_item : Res_item.t -> message option

(** [of_history items] maps {!pair_of_item} over [items], discarding
    elements that cannot be rendered. Relative order is preserved but indices
    need not match input indices. Use projected IDs for identity-sensitive
    selection, editing and history deletion. *)
val of_history : Res_item.t list -> message list

type projection

val project_entries : History_entry.t list -> projection

val project_effective_entries
  :  Chat_response.Moderation.Effective_entry.t list
  -> projection

val rows : projection -> Projected_message.t list
val messages : projection -> message list
val index_of_id : projection -> Projected_message.Id.t -> int option

val append_pending_approval
  :  projection
  -> local_id:string
  -> text:string
  -> (projection, string) result

val append_placeholder
  :  projection
  -> local_id:string
  -> kind:string
  -> message
  -> (projection, string) result
