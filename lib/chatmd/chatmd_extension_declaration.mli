open Core
module Spec = Chatmd_shell_spec.Extension_spec

val tool
  :  loader:Source_loader.t
  -> source_node:Source_loader.source
  -> source:Chatmd_shell_spec.Source_ref.t
  -> Chatmd_ast.node
  -> (Spec.tool, Chatmd_shell_spec.Diagnostic.t list) result

val script
  :  dir:Eio.Fs.dir_ty Eio.Path.t
  -> loader:Source_loader.t
  -> source_node:Source_loader.source
  -> source:Chatmd_shell_spec.Source_ref.t
  -> attributes:Chatmd_ast.attribute list
  -> inline_source:string
  -> (Spec.script, Chatmd_shell_spec.Diagnostic.t list) result

val authoring_context
  :  source:Chatmd_shell_spec.Source_ref.t
  -> Chatmd_ast.node
  -> (Spec.authoring_context, Chatmd_shell_spec.Diagnostic.t list) result

val serialize_tool : Spec.tool -> string
val serialize_script : Spec.script -> string
val serialize_authoring : Spec.authoring_context -> string

val authoring_help
  :  source:Chatmd_shell_spec.Source_ref.t
  -> Chatmd_ast.node
  -> (Spec.authoring_help, Chatmd_shell_spec.Diagnostic.t list) result

val serialize_help : Spec.authoring_help -> string
