open! Core
module J = Json_codec

type t =
  | Continue
  | Wait of Run_wake.t
  | Finish of
      { terminal : Run.Terminal.t
      ; relinquish : Run_work.t list
      }
[@@deriving equal]

let to_json = function
  | Continue -> `Object [ "kind", `String "continue" ]
  | Wait wake -> `Object [ "kind", `String "wait"; "wake", Run_wake.to_json wake ]
  | Finish { terminal; relinquish } ->
    `Object
      [ "kind", `String "finish"
      ; "terminal", Run.Terminal.to_json terminal
      ; "relinquish", `Array (List.map relinquish ~f:Run_work.to_json)
      ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind () =
    Extension_codec.validate_json
      ~max_bytes:Run_limits.max_document_bytes
      ~max_depth:Run_limits.max_depth
      json
  in
  let%bind f = J.fields json in
  let%bind kind = J.required_as f "kind" J.string in
  match kind with
  | "continue" -> Ok Continue
  | "wait" ->
    let%map wake = J.required_as f "wake" Run_wake.of_json in
    Wait wake
  | "finish" ->
    let%bind terminal = J.required_as f "terminal" Run.Terminal.of_json in
    let%bind relinquish =
      J.required_as f "relinquish" (Run_limits.list Run_work.of_json)
    in
    if
      List.length relinquish > Run_limits.max_occurrences
      || List.contains_dup relinquish ~compare:Run_work.compare
    then Error (Protocol_error.invalid_request "invalid run relinquishment set")
    else Ok (Finish { terminal; relinquish })
  | _ -> Error (Protocol_error.invalid_request "unsupported run action")
;;

let validate action =
  let open Result.Let_syntax in
  let%bind () =
    match action with
    | Continue -> Ok ()
    | Wait wake -> Run_wake.validate wake
    | Finish { terminal; relinquish } ->
      let%bind () = Run_limits.check_count (List.length relinquish) in
      let%bind () =
        List.fold_result relinquish ~init:() ~f:(fun () work -> Run_work.validate work)
      in
      let%bind () = Run.Terminal.validate terminal in
      if List.contains_dup relinquish ~compare:Run_work.compare
      then Error (Protocol_error.invalid_request "duplicate relinquishment occurrence")
      else Ok ()
  in
  Extension_codec.validate_json
    ~max_bytes:Run_limits.max_document_bytes
    ~max_depth:Run_limits.max_depth
    (to_json action)
;;

let combine left right =
  let open Result.Let_syntax in
  let%bind () =
    match left with
    | None -> Ok ()
    | Some action -> validate action
  in
  let%bind () =
    match right with
    | None -> Ok ()
    | Some action -> validate action
  in
  match left, right with
  | None, value | value, None -> Ok value
  | Some left, Some right ->
    if equal left right
    then Ok (Some left)
    else Error (Protocol_error.invalid_request "incompatible transactional run actions")
;;

let sexp_of_t t = Jsonaf.sexp_of_t (to_json t)

let t_of_sexp sexp =
  match of_json (Jsonaf.t_of_sexp sexp) with
  | Ok t -> t
  | Error e -> Sexplib.Conv.of_sexp_error e.message sexp
;;
