open! Core
module J = Json_codec

type t =
  | User_submission of Session.Message_content.t
  | Authored_start

let to_json = function
  | Authored_start -> `Object [ "kind", `String "authored_start" ]
  | User_submission content ->
    `Object
      [ "kind", `String "user_submission"
      ; "content", Session.Message_content.to_json content
      ]
;;

let validate = function
  | Authored_start -> Ok ()
  | User_submission content ->
    let open Result.Let_syntax in
    let%bind () = Run_limits.check_count (List.length content.attachments) in
    if String.length content.text > Run_limits.max_document_bytes
    then Error (Protocol_error.invalid_request "run submission exceeds byte bound")
    else if String.is_empty content.text && List.is_empty content.attachments
    then Error (Protocol_error.invalid_request "user run submission requires input")
    else (
      let json = Session.Message_content.to_json content in
      let%bind () =
        Extension_codec.validate_json
          ~max_bytes:Run_limits.max_document_bytes
          ~max_depth:Run_limits.max_depth
          json
      in
      Result.map (Session.Message_content.of_json json) ~f:(fun _ -> ()))
;;

let bounded_content json =
  let open Result.Let_syntax in
  let%bind fields = J.fields json in
  let%bind () =
    J.required_as fields "attachments" (function
      | `Array items -> Run_limits.check_count (List.length items)
      | _ -> Error (Protocol_error.invalid_request "run attachments must be an array"))
  in
  Session.Message_content.of_json json
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind () =
    Extension_codec.validate_json
      ~max_bytes:Run_limits.max_document_bytes
      ~max_depth:Run_limits.max_depth
      json
  in
  let%bind fields = J.fields json in
  let%bind kind = J.required_as fields "kind" J.string in
  let%bind value =
    match kind with
    | "authored_start" -> Ok Authored_start
    | "user_submission" ->
      Result.map (J.required_as fields "content" bounded_content) ~f:(fun content ->
        User_submission content)
    | _ -> Error (Protocol_error.invalid_request "unsupported run start input")
  in
  Result.map (validate value) ~f:(fun () -> value)
;;

let sexp_of_t t = Jsonaf.sexp_of_t (to_json t)

let t_of_sexp sexp =
  match of_json (Jsonaf.t_of_sexp sexp) with
  | Ok value -> value
  | Error error -> Sexplib.Conv.of_sexp_error error.message sexp
;;
