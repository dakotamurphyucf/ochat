open! Core

(** Strict conversion for top-level moderator shell runtime declarations. *)

(** [parse ~source node] converts one empty [<moderator_runtime
    shell_runtime="..."/>] element. Unknown attributes and non-whitespace
    children fail closed. *)
val parse
  :  source:Chatmd_shell_spec.Source_ref.t
  -> Chatmd_ast.node
  -> ( Chatmd_shell_spec.Manifest_compiler.moderator_runtime
       , Chatmd_shell_spec.Diagnostic.t list )
       result
