open! Core

(** Converts shell declarations already parsed by the ChatMD parser into pure
    serializable specifications. *)

(** [parse_runtime ~source node] validates and lowers one parsed
    [shell_access] element. *)
val parse_runtime
  :  source:Chatmd_shell_spec.Source_ref.t
  -> Chatmd_ast.node
  -> (Chatmd_shell_spec.Shell_spec.t, Chatmd_shell_spec.Diagnostic.t list) result

(** [parse_tool ~source node] validates and lowers one parsed shell [tool]
    element. The ChatMD parser must have recognized all nested children. *)
val parse_tool
  :  source:Chatmd_shell_spec.Source_ref.t
  -> Chatmd_ast.node
  -> (Chatmd_shell_spec.Shell_tool_spec.t, Chatmd_shell_spec.Diagnostic.t list) result
