open Core
module Spec = Chatmd_shell_spec.Extension_spec

type t

(** Parsed generated source DATA shared with immutable artifact selection.
    Uses the same generated-only syntax, plain initial messages and no implicit
    agent rules as [prepare]. No authoring resolution, tool binding, compilation
    or executable admission is implied. Reads only the supplied source bundle
    rooted in its caller-verified immutable materialized tree. *)
module Source : sig
  type t

  val parse
    :  dir:Eio.Fs.dir_ty Eio.Path.t
    -> Chatmd_source_bundle.t
    -> (t, Chatmd_shell_spec.Diagnostic.t list) result

  val moderator : t -> Spec.script option
end

(** Parse supplied bytes, bind explicit inherited references, resolve authoring
    policy and compile lifecycle moderators without evaluating or constructing
    tools. [ceiling] must already reflect host/parent delegability decisions.
    [requested_names] narrows that ceiling before file declarations narrow it
    again. No declarations can replace an inherited implementation or resources.
    Normal native/shell/MCP/agent tool construction and implicit message-resource
    loading are rejected; use an explicit inherited capability instead.

    Generated lifecycle scripts use extensibility-v1 without direct Model or
    Process modules. Compilation in Eio-managed domains shares one aggregate cooperative time budget;
    cancellation waits for the current compiler stage to finish.
    Generation configuration checks include the installed reasoning-effort codec;
    unsupported values reject before initialization. Remote model availability is
    not inferred from successful static validation.
    This does not create a child, materialize an artifact, authorize model use,
    enforce ongoing revocation, or provide parent-moderator mediation. Those
    remain responsibilities of the owning delegation service. *)
val prepare
  :  ?limits:Chatml_compilation.limits
  -> ?catalog:Authoring_policy.catalog
  -> env:Eio_unix.Stdenv.base
  -> dir:Eio.Fs.dir_ty Eio.Path.t
  -> ceiling:Tool_capability.t
  -> requested_names:string list
  -> Chatmd_source_bundle.t
  -> (t, Chatmd_shell_spec.Diagnostic.t list) result

val elements : t -> Prompt.Chat_markdown.top_level_elements list
val capabilities : t -> Tool_capability.t
val authoring : t -> Authoring_policy.t
val moderators : t -> (Spec.script * Extension_compiler.Program.t) list
val definition : t -> Extension_compiler.definition
val source_fingerprint : t -> string
val fingerprint : t -> string
