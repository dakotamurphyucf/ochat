(** Conversation summariser used by {!module:Context_compaction}.

    This module renders an ordered list of {!module:Openai.Responses.Item}
    values and asks the Responses API for a textual summary. Multipart
    message content is preserved in order.

    Two execution modes are supported:

    • {e Online} – triggered when an [OPENAI_API_KEY] is available and
      a capability-based *Eio* environment is provided.  In this mode
      {!Openai.Responses.post_response} is invoked with the developer
      prompt in [summarizer.ml] and a generous [max_output_tokens] limit.
      Parsing failures receive three total attempts. If the whole-history
      request exhausts those attempts, the compactable history is split once
      into two ordered halves. The first result is rolling context for the
      second request.

    • {e Offline stub} – activated whenever the API key or the
      environment is missing. The first 2,000 bytes of the rendered
      transcript are returned for deterministic tests.

    Input and tool-output images are preserved using a lightweight HTML
    placeholder of the form [<image src="..."/>]. *)

open! Core

(** [summarise ~relevant_items ~env] synthesises a concise summary of
    [relevant_items].

    Parameters
    • [relevant_items] – ordered sub-sequence of the conversation that
      must survive compaction.  Items may originate from the user, the
      assistant, or be function/tool-call artefacts.

    • [env] – optional {!Eio_unix.Stdenv.base}. Passing [None]
      unequivocally selects the offline stub.

    Online failure is returned explicitly.

    @raise Eio.Cancel.Cancelled if the operation is cancelled. *)
val summarise
  :  relevant_items:Openai.Responses.Item.t list
  -> env:Eio_unix.Stdenv.base option
  -> (string, exn) result

module For_testing : sig
  val render_transcript : Openai.Responses.Item.t list -> string

  val summarise_with
    :  sleep:(float -> unit)
    -> request:(Openai.Responses.Item.t list -> string)
    -> relevant_items:Openai.Responses.Item.t list
    -> (string, exn) result
end
