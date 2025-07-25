open Types
module Res_item = Openai.Responses.Item

(** Transform OpenAI response items into plain chat messages that the
    rendering layer can print.

    The module is deliberately tiny: it only concerns itself with
    *pure* data munging – no I/O, no state – so it can be reused by the
    model, the renderer and the persistence layer without introducing
    additional dependencies.

    {b Safety measures}

    • Every returned string passes through {!Chat_tui.Util.sanitize} to
      eliminate control characters that could break the terminal.

    • Tool output longer than {b 2 000} bytes is truncated and marked
      with an explicit ellipsis.  This prevents the UI from freezing on
      huge JSON payloads.

    {b Relation to {!Types}}

    The result type {!Types.message} matches the expectations of
    {!Chat_tui.Model.messages} and ultimately the renderer, so the
    conversion can be fed directly into the UI without further
    processing. *)

(** [pair_of_item item] converts an OpenAI chat [item] into a renderable
    [message].  Control characters are replaced with spaces and leading
    / trailing whitespace removed.

    @return [Some (role, content)] for items that carry textual content
            and [None] for artefacts that cannot be shown in the chat
            transcript (e.g. progress markers).

    {2 Example}
    Mapping a single assistant response into the UI format:
    {[
      let it = Openai.Responses.Item.Output_message
                 { role = Assistant
                 ; content = [ { text = "Hi" } ]
                 } in
      assert (Option.value_exn (Chat_tui.Conversation.pair_of_item it)
              = ("assistant", "Hi"))
    ]} *)
val pair_of_item : Res_item.t -> message option

(** [of_history items] maps {!pair_of_item} over [items], discarding
    elements that cannot be rendered.  The original order is preserved
    so the resulting list aligns with the OpenAI response indices. *)
val of_history : Res_item.t list -> message list
