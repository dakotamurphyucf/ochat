open Types

(** Neutral presentation only. Sanitization and the 10,000-byte tool-output
    display cut do not change canonical content or constitute security redaction. *)
module Rendered : sig
  type t

  val of_payload : History_entry.Payload.t -> t
  val of_visible : Agent_protocol.Public.History.Visible.t -> t
  val of_redaction : Agent_protocol.Public.History.Redaction.t -> t
  val of_draft : Transcript.Draft.item_view -> t
  val message : t -> message

  (** Exact complete known plain-message text, before display transformations.
      Does not grant editing authority; partial/unknown/multimodal content refuses. *)
  val copy_text : t -> string option
end

val output_text : History_entry.Payload.Output.t -> string
val of_history : History_entry.t list -> message list

type projection

val project_entries : History_entry.t list -> projection
val project_public_entries : Agent_protocol.Public.History.t list -> projection

val project_effective_entries
  :  Chat_response.Moderation.Effective_entry.t list
  -> projection

val draft_row : Transcript.Draft.item_view -> Projected_message.t
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
