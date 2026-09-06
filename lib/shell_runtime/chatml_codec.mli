open! Core

(** Strict data-only ChatML boundary validation. *)

type limits =
  { max_value_bytes : int
  ; max_string_bytes : int
  ; max_array_items : int
  ; max_depth : int
  }

val of_script_limits : Chatmd_shell_spec.Chatmd_script_spec.limits -> limits

(** [validate limits value] rejects runtime values and values exceeding any
    configured structural bound. *)
val validate
  :  limits
  -> Chatml.Chatml_lang.value
  -> (unit, Chatmd_shell_spec.Diagnostic.t) result

(** [record ~path ~allowed ~required value] validates a closed versioned
    record and returns its fields. *)
val record
  :  path:string list
  -> allowed:string list
  -> required:string list
  -> Chatml.Chatml_lang.value
  -> (Chatml.Chatml_lang.value String.Map.t, Chatmd_shell_spec.Diagnostic.t) result

val field
  :  path:string list
  -> Chatml.Chatml_lang.value String.Map.t
  -> string
  -> (Chatml.Chatml_lang.value, Chatmd_shell_spec.Diagnostic.t) result

val string
  :  path:string list
  -> Chatml.Chatml_lang.value
  -> (string, Chatmd_shell_spec.Diagnostic.t) result

val bool
  :  path:string list
  -> Chatml.Chatml_lang.value
  -> (bool, Chatmd_shell_spec.Diagnostic.t) result

val int
  :  path:string list
  -> Chatml.Chatml_lang.value
  -> (int, Chatmd_shell_spec.Diagnostic.t) result

val strings
  :  path:string list
  -> Chatml.Chatml_lang.value
  -> (string list, Chatmd_shell_spec.Diagnostic.t) result

val option
  :  path:string list
  -> (path:string list -> Chatml.Chatml_lang.value -> ('a, Chatmd_shell_spec.Diagnostic.t) result)
  -> Chatml.Chatml_lang.value
  -> ('a option, Chatmd_shell_spec.Diagnostic.t) result

val encode_record : (string * Chatml.Chatml_lang.value) list -> Chatml.Chatml_lang.value
val encode_strings : string list -> Chatml.Chatml_lang.value
val encode_option : ('a -> Chatml.Chatml_lang.value) -> 'a option -> Chatml.Chatml_lang.value
