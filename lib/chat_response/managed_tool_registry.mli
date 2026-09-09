open Core

(** Captured managed definitions and real base capabilities, compiled without
    initialization, resource construction or effects. Native and managed targets
    remain distinct; no no-op native runners are installed. *)
type t

(** Validates source/schema digests, handler kinds, exact uses dependencies and
    definition cycles before binding and compilation. The managed permission
    identity includes all captured scripts/tools/help and base permission identities;
    this conservatively invalidates grants when the definition's authority changes.
    Native bindings retain their original live identities and metadata; help
    declarations cannot retag existing capabilities. [owner] is supplied by
    the owning host; reference JSON cannot register or authorize targets.

    Uses existing definition limits and Eio-managed compilation. This preparation
    does not expose tools, initialize a moderator or install invocation dispatch.
    The host must use the matching prepared definitions under actor ownership and
    current permission/disclosure policy, including revalidation after waits. *)
val prepare
  :  ?limits:Chatml_compilation.limits
  -> env:Eio_unix.Stdenv.base
  -> owner:string
  -> capabilities:Tool_capability.t
  -> Prompt.Chat_markdown.top_level_elements list
  -> (t, Chatmd_shell_spec.Diagnostic.t list) result

val capabilities : t -> Tool_capability.t
val definition : t -> Extension_compiler.definition
val authority_fingerprint : t -> string

(** Resolve the exact live binding, never a same-name target. *)
val resolve
  :  t
  -> Tool_capability.binding
  -> (Extension_compiler.t, Tool_capability.error) result

(** Revalidate this definition's captured registry; extra current bindings cannot
    widen it, and removed/re-registered bindings invalidate it. *)
val revalidate : t -> current:Tool_capability.t -> (unit, Tool_capability.error) result
