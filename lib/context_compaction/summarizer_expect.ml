open! Core

(***********************************************************************
 *  Helpers                                                           *
 ***********************************************************************)

let make_input_msg role texts : Openai.Responses.Item.t =
  let open Openai.Responses in
  let open Input_message in
  let item : Input_message.t =
    { role
    ; content = List.map texts ~f:(fun text -> Text { text; _type = "input_text" })
    ; _type = "message"
    }
  in
  Item.Input_message item
;;

let make_user_msg text = make_input_msg Openai.Responses.Input_message.User [ text ]

let%expect_test "summariser – offline stub" =
  let relevant_items =
    List.init 5 ~f:(fun i -> make_user_msg (Printf.sprintf "Line %d" i))
  in
  let summary = Context_compaction.Summarizer.summarise ~relevant_items ~env:None in
  print_endline (Result.ok_exn summary);
  [%expect
    {|user: Line 0
user: Line 1
user: Line 2
user: Line 3
user: Line 4|}]
;;

let%expect_test "multipart input and output content is complete" =
  let open Openai.Responses in
  let developer =
    make_input_msg Input_message.Developer [ "before"; "import"; "after" ]
  in
  let assistant =
    Item.Output_message
      { role = Assistant
      ; id = "message"
      ; content =
          [ { annotations = []; text = "first"; _type = "output_text" }
          ; { annotations = []; text = "second"; _type = "output_text" }
          ]
      ; status = "completed"
      ; phase = None
      ; _type = "message"
      }
  in
  Context_compaction.Summarizer.For_testing.render_transcript [ developer; assistant ]
  |> print_endline;
  [%expect
    {|developer: before
import
after
Assistant: first
second|}]
;;

let parsing_error message =
  Openai.Responses.Response_parsing_error (`Object [], Failure message)
;;

let stream_parsing_error message =
  Openai.Responses.Response_stream_parsing_error (`Object [], Failure message)
;;

let text_of_item item =
  let open Openai.Responses in
  match item with
  | Item.Input_message { content = Text { text; _ } :: _; _ } -> text
  | _ -> ""
;;

let%expect_test "three attempts precede linear chunking" =
  let calls = ref 0 in
  let delays = ref [] in
  let chunk_requests = ref [] in
  let request items =
    incr calls;
    if !calls <= 3
    then raise (parsing_error "whole")
    else (
      chunk_requests
      := (items |> List.map ~f:text_of_item |> String.concat ~sep:"|") :: !chunk_requests;
      sprintf "result-%d" (!calls - 3))
  in
  let relevant_items =
    [ make_input_msg Openai.Responses.Input_message.Developer [ "shared" ]
    ; make_user_msg (String.make 40 'a')
    ; make_user_msg (String.make 40 'b')
    ; make_user_msg (String.make 40 'c')
    ]
  in
  let result =
    Context_compaction.Summarizer.For_testing.summarise_with
      ~sleep:(fun delay -> delays := delay :: !delays)
      ~request
      ~relevant_items
  in
  printf
    "calls=%d delays=%s\n"
    !calls
    (List.rev !delays |> [%sexp_of: float list] |> Sexp.to_string);
  List.rev !chunk_requests
  |> List.iteri ~f:(fun index request -> printf "request-%d=%s\n" (index + 1) request);
  print_endline (Result.ok_exn result);
  [%expect
    {|
    calls=5 delays=(1 2)
    request-1=shared|aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa|bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
    request-2=shared|<previous-compaction-result>
    result-1
    </previous-compaction-result>|cccccccccccccccccccccccccccccccccccccccc
    <compaction-part index="1">
    result-1
    </compaction-part>
    <compaction-part index="2">
    result-2
    </compaction-part>|}]
;;

let%expect_test "both parsing error variants are retried" =
  let calls = ref 0 in
  let request _ =
    incr calls;
    match !calls with
    | 1 -> raise (stream_parsing_error "stream")
    | 2 -> raise (parsing_error "response")
    | _ -> "ok"
  in
  let result =
    Context_compaction.Summarizer.For_testing.summarise_with
      ~sleep:ignore
      ~request
      ~relevant_items:[ make_user_msg "message" ]
  in
  printf "calls=%d result=%s\n" !calls (Result.ok_exn result);
  [%expect {|calls=3 result=ok|}]
;;

let%expect_test "ordinary failures are not retried" =
  let calls = ref 0 in
  let result =
    Context_compaction.Summarizer.For_testing.summarise_with
      ~sleep:ignore
      ~request:(fun _ ->
        incr calls;
        failwith "permanent")
      ~relevant_items:[ make_user_msg "message" ]
  in
  let error =
    match result with
    | Ok _ -> "unexpected success"
    | Error exn -> Exn.to_string_mach exn
  in
  printf "calls=%d error=%s\n" !calls error;
  [%expect {|calls=1 error=(Failure permanent)|}]
;;

let%expect_test "failed chunk exposes no partial summary" =
  let calls = ref 0 in
  let delays = ref [] in
  let result =
    Context_compaction.Summarizer.For_testing.summarise_with
      ~sleep:(fun delay -> delays := delay :: !delays)
      ~request:(fun _ ->
        incr calls;
        raise (parsing_error "unavailable"))
      ~relevant_items:[ make_user_msg "first"; make_user_msg "second" ]
  in
  let outcome =
    match result with
    | Ok summary -> "unexpected summary: " ^ summary
    | Error exn -> Exn.to_string_mach exn
  in
  printf
    "calls=%d delays=%s outcome=%s\n"
    !calls
    (List.rev !delays |> [%sexp_of: float list] |> Sexp.to_string)
    outcome;
  [%expect {|calls=6 delays=(1 2 1 2) outcome=(Failure unavailable)|}]
;;
