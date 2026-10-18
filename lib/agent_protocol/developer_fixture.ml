open! Core
module C = Developer_codec

let ( let* ) result f = Result.bind result ~f
let get fields name decode = C.required fields name |> Result.bind ~f:decode

module Fixture = struct
  module Response = struct
    type t =
      | Return of Jsonaf.t
      | Fail of string
  end

  module Step = struct
    type t =
      { tool_name : string
      ; input_schema : Jsonaf.t
      ; output_schema : Jsonaf.t
      ; expected_input : Jsonaf.t
      ; response : Response.t
      }

    let to_json t =
      `Object
        [ "tool_name", `String t.tool_name
        ; "input_schema", t.input_schema
        ; "output_schema", t.output_schema
        ; "expected_input", t.expected_input
        ; ( "response"
          , match t.response with
            | Response.Return x -> `Object [ "kind", `String "return"; "value", x ]
            | Fail x -> `Object [ "kind", `String "fail"; "message", `String x ] )
        ]
    ;;

    let create ~limits ~tool_name ~input_schema ~output_schema ~expected_input ~response =
      if
        String.is_empty tool_name
        || String.length tool_name > 1024
        ||
        match response with
        | Response.Fail message -> String.length message > 16_384
        | Return _ -> false
      then C.invalid "invalid fixture tool name"
      else (
        let t = { tool_name; input_schema; output_schema; expected_input; response } in
        let* () = C.validate ~limits (to_json t) in
        Ok t)
    ;;

    let tool_name t = t.tool_name
    let input_schema t = t.input_schema
    let output_schema t = t.output_schema
    let expected_input t = t.expected_input
    let response t = t.response

    let of_json ~limits json =
      let* fields =
        C.fields
          ~allowed:
            [ "tool_name"; "input_schema"; "output_schema"; "expected_input"; "response" ]
          json
      in
      let* tool_name = get fields "tool_name" (C.string ~max_bytes:1024) in
      let* input_schema = C.required fields "input_schema" in
      let* output_schema = C.required fields "output_schema" in
      let* expected_input = C.required fields "expected_input" in
      let* response =
        get fields "response" (fun json ->
          let* fields = C.fields ~allowed:[ "kind"; "value"; "message" ] json in
          let* kind = get fields "kind" (C.string ~max_bytes:16) in
          match kind with
          | "return" ->
            let* () =
              C.fields ~allowed:[ "kind"; "value" ] json |> Result.map ~f:ignore
            in
            let* value = C.required fields "value" in
            Ok (Response.Return value)
          | "fail" ->
            let* () =
              C.fields ~allowed:[ "kind"; "message" ] json |> Result.map ~f:ignore
            in
            let* message = get fields "message" (C.string ~max_bytes:16384) in
            Ok (Response.Fail message)
          | _ -> C.invalid "unknown fixture response")
      in
      create ~limits ~tool_name ~input_schema ~output_schema ~expected_input ~response
    ;;
  end

  type t = { steps : Step.t list }

  let to_json t = `Object [ "steps", `Array (List.map t.steps ~f:Step.to_json) ]
  let steps t = t.steps

  let create ~limits ~max_steps ~max_bytes ~steps =
    if max_steps < 0 || max_bytes <= 0 || List.length steps > max_steps
    then C.limit "fixture exceeds installed step limit"
    else (
      let t = { steps } in
      let json = to_json t in
      let* bytes = C.measure ~limits json in
      if bytes > max_bytes then C.limit "fixture exceeds installed byte limit" else Ok t)
  ;;

  let of_json ~limits ~max_steps ~max_bytes json =
    let* () = C.validate ~limits json in
    let* fields = C.fields ~allowed:[ "steps" ] json in
    let* steps =
      get fields "steps" (C.array ~max_items:max_steps (Step.of_json ~limits))
    in
    create ~limits ~max_steps ~max_bytes ~steps
  ;;
end
