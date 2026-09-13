open Core
module V = Chat_response.Authoring_validation

type t =
  { default_tokens : int option
  ; max_tokens : int option
  ; preload_tokens : int option
  }

let param =
  let open Command.Let_syntax in
  let%map_open default_tokens =
    flag
      "--authoring-default-tokens"
      (optional int)
      ~doc:"N Local documentation query default, estimated tokens (default 12000)."
  and max_tokens =
    flag
      "--authoring-max-tokens"
      (optional int)
      ~doc:"N Local documentation query ceiling, estimated tokens (default 32000)."
  and preload_tokens =
    flag
      "--authoring-preload-tokens"
      (optional int)
      ~doc:
        "N Local automatic authoring context ceiling, estimated tokens (default 32000)."
  in
  { default_tokens; max_tokens; preload_tokens }
;;

let is_configured t =
  Option.is_some t.default_tokens
  || Option.is_some t.max_tokens
  || Option.is_some t.preload_tokens
;;

let resolve t =
  match is_configured t with
  | false -> Ok None
  | true ->
    let defaults = V.default_context_budget in
    V.context_budget
      ~default_tokens:(Option.value t.default_tokens ~default:defaults.default_tokens)
      ~max_tokens:(Option.value t.max_tokens ~default:defaults.max_tokens)
      ~preload_tokens:(Option.value t.preload_tokens ~default:defaults.preload_tokens)
    |> Result.map ~f:Option.some
    |> Result.map_error ~f:(fun message ->
      Error.of_string ("Invalid authoring budget: " ^ message))
;;
