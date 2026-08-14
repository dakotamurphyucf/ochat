open! Core

(** Strict ordered attribute access for parsed ChatMD declarations. *)

type t

(** [create ~source ~path ~allowed attributes] rejects duplicate and unknown
    attributes while preserving source order. *)
val create
  :  source:Chatmd_shell_spec.Source_ref.t
  -> path:string list
  -> allowed:string list
  -> Chatmd_ast.attribute list
  -> (t, Chatmd_shell_spec.Diagnostic.t) result

(** [optional t name] returns the decoded value for [name]. Bare attributes
    are rejected because shell declarations require explicit values. *)
val optional : t -> string -> (string option, Chatmd_shell_spec.Diagnostic.t) result

(** [required t name] returns the non-empty decoded value for [name]. *)
val required : t -> string -> (string, Chatmd_shell_spec.Diagnostic.t) result

(** [to_values t] returns attributes as decoded string pairs. Bare attributes
    are represented by an empty value so the destination schema can reject
    them with its field-specific diagnostic. *)
val to_values : t -> (string * string) list
