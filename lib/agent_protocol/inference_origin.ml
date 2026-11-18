open! Core

type t =
  | Unbound
  | Run of Id.Run.t
[@@deriving equal, sexp]

let validate = function
  | Unbound -> Ok ()
  | Run id -> Id.Run.of_string (Id.Run.to_string id) |> Result.map ~f:ignore
;;

let generated_t_of_sexp = t_of_sexp

let t_of_sexp sexp =
  let value = generated_t_of_sexp sexp in
  match validate value with
  | Ok () -> value
  | Error error -> Sexplib.Conv.of_sexp_error error.Protocol_error.message sexp
;;

let to_json = function
  | Unbound -> `Object [ "schema_version", `Number "1"; "type", `String "unbound" ]
  | Run id ->
    `Object
      [ "schema_version", `Number "1"
      ; "type", `String "run"
      ; "run_id", Id.Run.to_json id
      ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind _ =
    Json_codec.required_as fields "schema_version" (Json_codec.bounded_int ~min:1 ~max:1)
  in
  let%bind kind = Json_codec.required_as fields "type" Json_codec.string in
  match kind with
  | "unbound" ->
    let%map () = Extension_codec.closed fields [ "schema_version"; "type" ] in
    Unbound
  | "run" ->
    let%bind () = Extension_codec.closed fields [ "schema_version"; "type"; "run_id" ] in
    let%map id = Json_codec.required_as fields "run_id" Id.Run.of_json in
    Run id
  | _ -> Error (Protocol_error.invalid_request "unknown inference origin")
;;
