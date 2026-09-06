open! Core

(** Canonical ChatMD serialization for parsed shell declarations. *)

(** [runtime spec] serializes [spec] as a complete [shell_access] element. *)
val runtime : Chatmd_shell_spec.Shell_spec.t -> string

(** [tool spec] serializes [spec] as a complete shell [tool] element. *)
val tool : Chatmd_shell_spec.Shell_tool_spec.t -> string
