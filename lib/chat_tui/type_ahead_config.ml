open! Core

type mode =
  | Off
  | Manual
  | Auto
[@@deriving sexp, equal]

type t =
  { mode : mode
  ; model : Openai.Responses.Request.model
  ; history_messages : int
  ; debounce_ms : int
  ; max_output_tokens : int
  }

let default =
  { mode = Off
  ; model = Openai.Responses.Request.model_of_str_exn "gpt-5.6-luna"
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
  else (
    let%map model =
      Or_error.try_with (fun () -> Openai.Responses.Request.model_of_str_exn model)
    in
    { mode; model; history_messages; debounce_ms; max_output_tokens })
;;

let validate_credentials t ~api_key =
  if
    equal_mode t.mode Off
    || Option.exists api_key ~f:(fun key -> not (String.is_empty (String.strip key)))
  then Ok ()
  else Or_error.error_string "Typeahead requires a nonempty local OPENAI_API_KEY"
;;
