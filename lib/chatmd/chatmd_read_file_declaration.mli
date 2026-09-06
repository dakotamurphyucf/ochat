open! Core

(** [parse ~source node] validates one built-in [read_file] declaration and
    lowers its nested [read] roots. Relative root paths use [${tool_dir}]. *)
val parse
  :  source:Chatmd_shell_spec.Source_ref.t
  -> Chatmd_ast.node
  -> (Chatmd_read_file_spec.t, Chatmd_shell_spec.Diagnostic.t list) result

(** [serialize specification] emits one complete [read_file] tool element. *)
val serialize : Chatmd_read_file_spec.t -> string
