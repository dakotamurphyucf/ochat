# chatmd_script_declaration

Typed ChatML declaration parsing, registry validation and serialization with loader-provided source provenance. Compiling declarations is distinct from running their effects.

See [agent-core embedding](../../agent-server/embedding.md),
[session/history behavior](../../agent-server/sessions-and-workspaces.md),
and [protocol synchronization](../../agent-server/protocol.md).

## Public contract

[Interface](../../../lib/chatmd/chatmd_script_declaration.mli) · [implementation](../../../lib/chatmd/chatmd_script_declaration.ml)

The following excerpt is the current callable contract. Eio switches own active
resources; preserve typed errors/cancellation and the host's authorization
boundary rather than bypassing the actor from a presentation adapter.

```ocaml
open! Core

(** Parses and serializes typed ChatML script declarations outside
    [Prompt]. *)

val parse
  :  dir:Eio.Fs.dir_ty Eio.Path.t
  -> loader:Source_loader.t
  -> source_node:Source_loader.source
  -> source:Chatmd_shell_spec.Source_ref.t
  -> attributes:Chatmd_ast.attribute list
  -> inline_source:string
  -> (Chatmd_shell_spec.Chatmd_script_spec.t, Chatmd_shell_spec.Diagnostic.t list) result

(** [validate_registry scripts] rejects duplicate qualified script IDs. *)
val validate_registry
  :  Chatmd_shell_spec.Chatmd_script_spec.t list
  -> ( Chatmd_shell_spec.Chatmd_script_spec.t list
       , Chatmd_shell_spec.Diagnostic.t list )
       result

(** [validate_prompt_registry ~moderator_ids scripts] validates aggregate
    compatibility variants without placing registry logic in [Prompt]. *)
val validate_prompt_registry
  :  moderator_ids:string list
  -> Chatmd_shell_spec.Chatmd_script_spec.t list
  -> (unit, Chatmd_shell_spec.Diagnostic.t list) result

(** [serialize script] renders one canonical ChatMD script declaration. *)
val serialize : Chatmd_shell_spec.Chatmd_script_spec.t -> string
```
