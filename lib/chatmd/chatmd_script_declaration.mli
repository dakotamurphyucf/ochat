open! Core

(** Parses and serializes typed ChatML script declarations outside
    [Prompt]. *)

val parse
  :  dir:Eio.Fs.dir_ty Eio.Path.t
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
