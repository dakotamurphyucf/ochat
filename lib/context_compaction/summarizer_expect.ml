open! Core

(***********************************************************************
 *  Helpers                                                           *
 ***********************************************************************)

let allocator =
  History_entry.Allocator.create ~namespace:"summary-tests" ~next_sequence:0
  |> Result.ok_or_failwith
;;

let entry item = Openai.Responses_history.create ~allocator item |> Result.ok_or_failwith

let make_input_msg role texts =
  let open Openai.Responses in
  let open Input_message in
  let item : Input_message.t =
    { role
    ; content = List.map texts ~f:(fun text -> Text { text; _type = "input_text" })
    ; _type = "message"
    }
  in
  entry (Item.Input_message item)
;;

let make_user_msg text = make_input_msg Openai.Responses.Input_message.User [ text ]

let%expect_test "summariser uses an explicitly injected offline request" =
  let relevant_items =
    List.init 5 ~f:(fun i -> make_user_msg (Printf.sprintf "Line %d" i))
  in
  let summary =
    Context_compaction.Summarizer.For_testing.summarise_with
      ~relevant_items
      ~sleep:ignore
      ~request:(fun entries ->
        Ok (Context_compaction.Summarizer.render_transcript entries))
  in
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
  Context_compaction.Summarizer.For_testing.render_transcript
    [ developer; entry assistant ]
  |> print_endline;
  [%expect
    {|developer: before
import
after
Assistant: first
second|}]
;;

let parsing_error =
  Inference_client.Execution.Completion_error.Outcome (Failed (Transport Protocol))
;;

let text_of_item item =
  match
    History_entry.Payload.Semantic.view
      (History_entry.Payload.semantic (History_entry.payload item))
  with
  | Message { content = Text { text; _ } :: _; _ } -> text
  | Message _ | Call _ | Result _ | Reasoning _ | Unknown _ -> ""
;;

let%expect_test "three attempts precede linear chunking" =
  let calls = ref 0 in
  let delays = ref [] in
  let chunk_requests = ref [] in
  let request items =
    incr calls;
    if !calls <= 3
    then Error parsing_error
    else (
      chunk_requests
      := (items |> List.map ~f:text_of_item |> String.concat ~sep:"|") :: !chunk_requests;
      Ok (sprintf "result-%d" (!calls - 3)))
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

let%expect_test "protocol failure retries stop on successful summary" =
  let calls = ref 0 in
  let request _ =
    incr calls;
    match !calls with
    | 1 | 2 -> Error parsing_error
    | _ -> Ok "ok"
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

let%expect_test "authentication failures are not retried" =
  let calls = ref 0 in
  let result =
    Context_compaction.Summarizer.For_testing.summarise_with
      ~sleep:ignore
      ~request:(fun _ ->
        incr calls;
        Error (Outcome (Failed (Authentication Missing))))
      ~relevant_items:[ make_user_msg "message" ]
  in
  let error =
    match result with
    | Ok _ -> "unexpected success"
    | Error
        (Context_compaction.Summarizer.Failed (Outcome (Failed (Authentication Missing))))
      -> "missing authentication"
    | Error exn -> Exn.to_string_mach exn
  in
  printf "calls=%d error=%s\n" !calls error;
  [%expect {|calls=1 error=missing authentication|}]
;;

let%expect_test "failed chunk exposes no partial summary" =
  let calls = ref 0 in
  let delays = ref [] in
  let result =
    Context_compaction.Summarizer.For_testing.summarise_with
      ~sleep:(fun delay -> delays := delay :: !delays)
      ~request:(fun _ ->
        incr calls;
        Error parsing_error)
      ~relevant_items:[ make_user_msg "first"; make_user_msg "second" ]
  in
  let outcome =
    match result with
    | Ok summary -> "unexpected summary: " ^ summary
    | Error (Context_compaction.Summarizer.Failed (Outcome (Failed (Transport Protocol))))
      -> "invalid response"
    | Error exn -> Exn.to_string_mach exn
  in
  printf
    "calls=%d delays=%s outcome=%s\n"
    !calls
    (List.rev !delays |> [%sexp_of: float list] |> Sexp.to_string)
    outcome;
  [%expect {|calls=6 delays=(1 2 1 2) outcome=invalid response|}]
;;

exception Observer_failed

let%expect_test "strict callback failures propagate without retry or partial summary" =
  let calls = ref 0 in
  let raised =
    try
      ignore
        (Context_compaction.Summarizer.For_testing.summarise_with
           ~sleep:ignore
           ~request:(fun _ ->
             incr calls;
             raise Observer_failed)
           ~relevant_items:[ make_user_msg "keep" ]
         : (string, exn) Result.t);
      false
    with
    | Observer_failed -> true
  in
  printf "propagated=%b calls=%d\n" raised !calls;
  [%expect {| propagated=true calls=1 |}]
;;

let%expect_test "bound parallel calls with reused provider alias stay together" =
  let module P = History_entry.Payload in
  let create view metadata =
    P.Semantic.create view ~metadata
    |> Result.ok_or_failwith
    |> P.authored
    |> History_entry.create ~allocator
    |> Result.ok_or_failwith
  in
  let call () =
    create
      (Call
         { kind = Function
         ; name = "read_file"
         ; namespace = Absent
         ; input_bytes = "{}"
         ; async = Absent
         })
      { P.Metadata.empty with call_id = Value "reused" }
  in
  let first = call () in
  let second = call () in
  let result call =
    create
      (Result
         { kind = Function
         ; relation = Bound (History_entry.id call)
         ; output = Text "result"
         })
      { P.Metadata.empty with call_id = Value "reused" }
  in
  Context_compaction.Summarizer.grouped_items
    [ first; second; result first; result second; make_user_msg "next" ]
  |> List.map ~f:List.length
  |> [%sexp_of: int list]
  |> print_s;
  [%expect {| (4 1) |}]
;;
