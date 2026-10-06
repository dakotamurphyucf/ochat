open Core
open Jsonaf.Export
module Sse = Openai.Responses_sse

let show = function
  | Ok None -> ()
  | Ok (Some Sse.Done) -> print_endline "done-marker"
  | Ok (Some (Sse.Payload json)) -> print_endline (Jsonaf.to_string json)
  | Error _ -> print_endline "error"
;;

let%expect_test "SSE supports CRLF, BOM, comments and multiline JSON" =
  let parser = Sse.create () |> Or_error.ok_exn in
  List.iter
    [ "\239\187\191: comment\r"
    ; "event: response.future\r"
    ; "id: 4\r"
    ; "data: {\r"
    ; "data: \"type\": \"response.future\",\r"
    ; "data: \"nested\": [1, null]}\r"
    ; "\r"
    ; "data: [DONE]"
    ; ""
    ]
    ~f:(fun line -> show (Sse.feed_line parser line));
  printf "pending:%b\n" (Sse.finish parser);
  [%expect
    {|
    {"type":"response.future","nested":[1,null]}
    done-marker
    pending:false
    |}]
;;

let%expect_test "SSE malformed data and event mismatch poison the parser" =
  List.iter
    [ [ "data: not-json"; ""; "data: {}" ]
    ; [ "event: response.completed"; "data: {\"type\":\"response.failed\"}"; ""; "" ]
    ]
    ~f:(fun lines ->
      let parser = Sse.create () |> Or_error.ok_exn in
      List.iter lines ~f:(fun line -> show (Sse.feed_line parser line)));
  [%expect
    {|
    error
    error
    error
    error
    |}]
;;

let%expect_test "SSE does not dispatch an incomplete frame on EOF" =
  let parser = Sse.create () |> Or_error.ok_exn in
  show (Sse.feed_line parser "data: {\"type\":\"response.completed\"}");
  printf "pending:%b\n" (Sse.finish parser);
  show (Sse.feed_line parser "");
  [%expect
    {|
    pending:true
    error
    |}]
;;

let%expect_test "SSE bounds ignored fields and rejects invalid limits" =
  printf "zero-limit:%b\n" (Result.is_error (Sse.create ~max_frame_bytes:0 ()));
  let parser = Sse.create ~max_frame_bytes:8 () |> Or_error.ok_exn in
  show (Sse.feed_line parser ":1234567");
  [%expect
    {|
    zero-limit:true
    error
    |}]
;;

module Optional_probe = struct
  type t = { value : string option [@jsonaf.option] } [@@deriving jsonaf]
end

module Nullable_probe = struct
  type t = { value : string option } [@@deriving jsonaf]
end

let%expect_test "actual PPX optional vs nullable attributes are distinct" =
  List.iter [ "{}"; "{\"value\":null}"; "{\"value\":\"x\"}" ] ~f:(fun input ->
    let json = Jsonaf.of_string input in
    let decode f =
      match Or_error.try_with (fun () -> f json) with
      | Error _ -> "error"
      | Ok None -> "none"
      | Ok (Some text) -> text
    in
    printf
      "%s optional=%s nullable=%s\n"
      input
      (decode (fun json -> (Optional_probe.t_of_jsonaf json).value))
      (decode (fun json -> (Nullable_probe.t_of_jsonaf json).value)));
  print_endline (Jsonaf.to_string (Optional_probe.jsonaf_of_t { value = None }));
  print_endline (Jsonaf.to_string (Nullable_probe.jsonaf_of_t { value = None }));
  [%expect
    {|
    {} optional=none nullable=error
    {"value":null} optional=error nullable=none
    {"value":"x"} optional=x nullable=x
    {}
    {"value":null}
    |}]
;;
