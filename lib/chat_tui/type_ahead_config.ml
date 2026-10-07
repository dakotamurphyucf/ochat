open! Core

type mode =
  | Off
  | Manual
  | Auto
[@@deriving sexp, equal]

type t =
  { mode : mode
  ; model : string
  ; history_messages : int
  ; debounce_ms : int
  ; max_output_tokens : int
  }

let default =
  { mode = Off
  ; model = "gpt-5.6-luna"
  ; history_messages = 0
  ; debounce_ms = 200
  ; max_output_tokens = 200
  }
;;

let parse_mode = function
  | "off" -> Ok Off
  | "manual" -> Ok Manual
  | "auto" -> Ok Auto
  | _ -> Or_error.error_string "--typeahead must be off, manual, or auto"
;;

let check_range flag value low high =
  if value >= low && value <= high
  then Ok ()
  else Or_error.errorf "%s must be %d–%d" flag low high
;;

let create ~mode ~model ~history_messages ~debounce_ms ~max_output_tokens =
  let open Or_error.Let_syntax in
  let%bind mode = parse_mode mode in
  let%bind () = check_range "--typeahead-history-messages" history_messages 0 3 in
  let%bind () = check_range "--typeahead-debounce-ms" debounce_ms 100 5000 in
  let%bind () = check_range "--typeahead-max-output-tokens" max_output_tokens 1 512 in
  if String.is_empty (String.strip model)
  then Or_error.error_string "--typeahead-model must not be empty"
  else if not (String.Utf8.is_valid model)
  then Or_error.error_string "--typeahead-model must be valid UTF-8"
  else Ok { mode; model; history_messages; debounce_ms; max_output_tokens }
;;
