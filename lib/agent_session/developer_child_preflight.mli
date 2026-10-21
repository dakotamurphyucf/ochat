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
    Tool_script Extension_script, Shell_runtime and Moderator_runtime metadata.
    Moderator metadata is only admitted structurally here; the selected child's
    prepared Agent runtime and Shell manifest authorize its execution. The parser-owned
    legacy Script constructor is admitted only for a selected child. Its
    ID, language, kind, path and source text consume the finite scan allowance;
    retained script compilation and moderator installation validate and run it
    later.
    Actual child resources remain unavailable until the owning host proves
    and installs every selected implementation. Nested Agent, Persistent and
    Inherited Tool declarations, authored call/response, unsupported script roles
    and authoring effects reject
    before host activation. Image/document Basic items pass only a finite structural
    preflight; the selected source projector must subsequently authenticate
    each parser-owned occurrence and admit any read before Driver entry. This
    preflight grants no local or remote source access. Authored Agent content
    passes only its bounded structural scan; the selected source runner must
    prove each parser occurrence and mint a distinct descendant Content target
    before any nested child begins. *)
val admit_selected
  :  revision:Prompt_revision.t
  -> parsed:Prompt.Chat_markdown.Parsed.t
  -> input:Prompt.Chat_markdown.content_item list
  -> checkpoint:(unit -> unit)
  -> (unit, Agent_protocol.Error.t) result

type ordinary_agent_tools =
  | Denied
  | Registered

(** The same bounded selected preflight, returning whether any genuine parsed
    image/document Basic requires dynamic source admission. A false result is
    structural captured/literal input, not a fallback after a failed read.
    [Registered] only changes the bounded structural classifier. It grants no
    native implementation or source access. The host may select it only after
    preparing exact parser-owned Agent registrations under its original bound
    Source_binding and trusted Tool runner. Missing/failed registration uses
    [Denied], never a literal or Fetch fallback. *)
val classify_selected
  :  ordinary_agent_tools:ordinary_agent_tools
  -> revision:Prompt_revision.t
  -> parsed:Prompt.Chat_markdown.Parsed.t
  -> input:Prompt.Chat_markdown.content_item list
  -> checkpoint:(unit -> unit)
  -> (bool, Agent_protocol.Error.t) result
