(** Conversation–history compactor.

    [Compactor] glues together {!module:Context_compaction.Config},
    {!module:Context_compaction.Relevance_judge}, and
    {!module:Context_compaction.Summarizer}.  Its sole purpose is to trim a
    potentially long chat transcript down to a size that comfortably fits
    within the LLM’s context window while keeping the essence of the
    conversation intact.

    The default pipeline is deliberately lightweight – no tokeniser is
    needed and the function always finishes in {e O(n)} time where [n] is the
    number of messages:

    {ol
    {- Load user overrides via {!Config.load}.}
    {- Convert each {!Openai.Responses.Item} to plain text and retain only
      those whose importance score, as judged by
      {!Relevance_judge.is_relevant}, meets or exceeds
      {!Config.relevance_threshold}.}
    {- Pass the retained sub-sequence to {!Summarizer.summarise} and truncate
      the resulting summary to {!Config.context_limit} characters – the
      rule-of-thumb {e 1 char ≈ 1 token} has proven robust enough in
      practice.}
    {- Return a new history consisting of the original first item (to keep
      the system prompt) plus **at most one** additional [`system`] message
      containing the summary.}}

    The entire pipeline is exception-safe: any internal failure causes the
    function to fall back to the identity transformation and return the
    original [history].

    {1 Typical usage}

    {[
      let compacted =
        Context_compaction.Compactor.compact_history
          ~env:(Some stdenv)   (* pass Eio capabilities when available *)
          ~history
      in
      send_to_llm (compacted @ new_user_messages)
    ]}
    *)

open! Core

(** [compact_history ~env ~history] returns a condensed replacement for
    [history].

    The result is guaranteed to:
    {ul
    {- start with the original first item;}
    {- contain {e at most} one additional [`system`] message with a summary;}
    {- respect {!Config.context_limit} (character budget).}}

    Parameters
    {ul
    {- [env] – optional {!Eio_unix.Stdenv.base}.  When [Some], the pipeline
       invokes the OpenAI API; when [None] it falls back to deterministic
       offline stubs.}
    {- [history] – full conversation transcript to be compacted.}}

    Never raises – on error the original [history] is returned verbatim. *)
val compact_history
  :  env:Eio_unix.Stdenv.base option
  -> history:Openai.Responses.Item.t list
  -> Openai.Responses.Item.t list
