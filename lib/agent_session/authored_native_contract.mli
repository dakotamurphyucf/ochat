open Core

(** Native declarations whose authority is not owned by the shell Registry or
    Managed_tool_registry. Compare actual parser-emitted tools in order, retaining
    builtin/inherited names, MCP endpoints/credentials/interfaces and typed
    read-file roots. Only diagnostic Source_ref rebasing is ignored. Agent tool
    associations are admitted separately by Agent_source_code_view; shell-native
    declarations are admitted by Registry.rebind_code. No resources are opened.
    Complete element traversal and input-sized comparisons poll the caller's
    whole-helper checkpoint, with a finite 16384-element admission before
    retaining lists. Callback failures/cancellation propagate unchanged. *)
val validate
  :  original:Prompt.Chat_markdown.top_level_elements list
  -> selected:Prompt.Chat_markdown.top_level_elements list
  -> checkpoint:(unit -> unit)
  -> (unit, Agent_protocol.Error.t) Result.t
