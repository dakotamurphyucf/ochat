open! Core

(** Source-dependent values for the existing ChatMarkdown converter. The
    Converter keeps its role, inline-string and history identity rules. Its
    selected projection admits message and reasoning content, but not tool-call
    history. An owned conversion supplies these three reads
    instead of invoking Fetch, Ctx.dir, cache, Base64.file_to_data_uri or a
    recursive Driver. This is a projection callback bundle, not filesystem,
    network, model or Agent execution authority. Trusted callers bind it to
    their selected source and execution scopes. *)
type t

(** [image] returns a fully admitted data URI for an image item; Converter
    retains its original inline-string spelling when that rendering mode is
    used. [document] returns final rendered text after the item's existing
    cleanup_html-before-markdown precedence. [agent] returns the final answer
    of an actual selected content-Agent execution, or a typed denial until
    such a runner is installed. All callbacks must enforce aggregate bounds,
    source provenance and currentness; Converter does not grant them. *)
val create
  :  image:
       (Prompt.Chat_markdown.basic_content_item
        -> (string, Agent_protocol.Error.t) result)
  -> document:
       (Prompt.Chat_markdown.basic_content_item
        -> (string, Agent_protocol.Error.t) result)
  -> agent:(Prompt.Chat_markdown.agent_content -> (string, Agent_protocol.Error.t) result)
  -> t

val image
  :  t
  -> Prompt.Chat_markdown.basic_content_item
  -> (string, Agent_protocol.Error.t) result

val document
  :  t
  -> Prompt.Chat_markdown.basic_content_item
  -> (string, Agent_protocol.Error.t) result

val agent
  :  t
  -> Prompt.Chat_markdown.agent_content
  -> (string, Agent_protocol.Error.t) result
