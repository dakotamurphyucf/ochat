open! Core

type t =
  { max_commands : int
  ; max_bytes : int
  ; max_terminal_bytes : int
  }

let max_commands t = t.max_commands
let max_bytes t = t.max_bytes
let max_terminal_bytes t = t.max_terminal_bytes

let create ~max_commands ~max_bytes ~max_terminal_bytes =
  if
    max_commands <= 0
    || max_commands > 16_384
    || max_bytes <= 0
    || max_bytes > 16 * 1024 * 1024
    || max_terminal_bytes <= 0
    || max_terminal_bytes > max_bytes
  then Or_error.error_string "invalid developer command limits"
  else Ok { max_commands; max_bytes; max_terminal_bytes }
;;

let default ~document =
  let max_bytes =
    Int.min (Document_schema.Limits.max_bytes document) (16 * 1024 * 1024)
  in
  { max_commands = 1024; max_bytes; max_terminal_bytes = Int.min max_bytes (64 * 1024) }
;;
