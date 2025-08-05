(** Conversation summariser used by {!module:Context_compaction}.

    A compaction cycle starts with {!module:Relevance_judge}
    selecting a subset of the chat history that must survive.  This
    module converts that list of {!module:Openai.Responses.Item}
    values into a **single textual summary** that subsequently
    replaces the original messages.  The aim is to reduce token usage
    without losing critical context for the next model call.

    Two execution modes are supported:

    • {e Online} – triggered when an [OPENAI_API_KEY] is available and
      a capability-based *Eio* environment is provided.  In this mode
      {!Openai.Responses.post_response} is invoked with a handcrafted
      system prompt (see the [prompt] constant in
      {!file:summarizer.ml}).  The call uses model [`Gpt4_1`], a
      temperature of 0.3 and a generous [max_output_tokens] limit of
      100_000 to avoid premature truncation.

    • {e Offline stub} – activated whenever the API key or the
      environment is missing, or if the online call throws.  We shove
      the conversation transcript through a 2 000-character truncate
      and return the result verbatim.  This deterministic path is
      just for unit tests.

    All errors are handled internally; the public API is synchronous
    and exception-free. *)

open! Core

(** [summarise ~relevant_items ~env] synthesises a concise summary of
    [relevant_items].

    Parameters
    • [relevant_items] – ordered sub-sequence of the conversation that
      must survive compaction.  Items may originate from the user, the
      assistant, or be function/tool-call artefacts.

    • [env] – optional {!Eio_unix.Stdenv.base}.  Passing [None]
      unequivocally selects the offline stub.  When [Some], the online
      path is attempted (and silently downgraded to the stub on any
      failure).

    Returns an UTF-8 encoded string safe for direct injection into a
    system or developer message.

    @raise Never – all internal failures are converted into the stub
           output. *)
val summarise
  :  relevant_items:Openai.Responses.Item.t list
  -> env:Eio_unix.Stdenv.base option
  -> string
