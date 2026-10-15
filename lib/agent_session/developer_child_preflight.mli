open! Core

(** Bounded visitor over the genuine captured ChatMD AST. It only rejects
    unsupported executable surfaces; it never authorizes a tool or supplies a
    source, filesystem route or runtime implementation. The caller must prove
    the revision and parsed view belong to its physically selected saved child
    before and after this pure scan. A parser/view identity mismatch rejects.
    The fixed scan allowance is 16,384 nodes and 8 MiB of examined text,
    besides the caller's cancellation/deadline checkpoint. *)

val admit_literal
  :  revision:Prompt_revision.t
  -> parsed:Prompt.Chat_markdown.Parsed.t
  -> checkpoint:(unit -> unit)
  -> (unit, Agent_protocol.Error.t) result

(** Selected child preparation may admit inert text/config/reasoning and
    Builtin (except the driver-owned subagent), Read_file, Custom, Shell, MCP
    and Extension Tool declarations, plus non-moderator Shell_script,
    Tool_script Extension_script and Shell_runtime metadata. Generic Script is
    the converted moderator surface and rejects here.
    Actual child resources remain unavailable until the owning host proves
    and installs every selected implementation. Nested Agent, Persistent and
    Inherited Tool declarations, authored call/response, image/document/Agent
    content, moderation and authoring effects reject before host activation. *)
val admit_selected
  :  revision:Prompt_revision.t
  -> parsed:Prompt.Chat_markdown.Parsed.t
  -> input:Prompt.Chat_markdown.content_item list
  -> checkpoint:(unit -> unit)
  -> (unit, Agent_protocol.Error.t) result
