open! Core
module P = Agent_protocol
module T = P.Session_seed.Transcript
module D = Document_schema

let entry sequence text =
  let module N = History_entry.Payload in
  let id =
    History_entry.Id.create ~namespace:"retained/input" ~sequence |> Result.ok_or_failwith
  in
  let semantic =
    N.Semantic.create
      (Message
         { form = Input
         ; role = User
         ; content = [ Text { text; annotations = []; logprobs = Absent } ]
         ; phase = Absent
         })
      ~metadata:N.Metadata.empty
    |> Result.ok_or_failwith
  in
  let payload = N.authored semantic in
  P.History.
    { id
    ; content_revision = Content_revision.zero
    ; role = User
    ; kind = Message
    ; payload = N.to_json payload
    ; provenance = Canonical
    ; redacted = false
    }
;;

let create ?(checkpoint = Fn.ignore) ?(limits = D.Limits.default) entries =
  T.create
    entries
    ~next_history_sequence:0L
    ~limits
    ~max_entries:32
    ~max_work_steps:100000
    ~checkpoint
;;

let require = function
  | Ok value -> value
  | Error error -> failwith error.P.Error.message
;;

let%test_unit "complete neutral roles, call relations and opaque wire lexemes survive" =
  let module N = History_entry.Payload in
  let id sequence =
    History_entry.Id.create ~namespace:"foreign-seed" ~sequence |> Result.ok_or_failwith
  in
  let retained sequence role kind view metadata =
    let semantic = N.Semantic.create view ~metadata |> Result.ok_or_failwith in
    let payload =
      N.captured
        semantic
        ~origin:N.Origin.unavailable
        ~raw:
          (`Object
              [ "opaque-id", `String "007\\caller"
              ; "negative-zero", `Number "-0"
              ; "exponent", `Number "1.2300e+02"
              ; "absent-vs-null", `Null
              ])
      |> Result.ok_or_failwith
    in
    P.History.
      { id = id sequence
      ; content_revision = Content_revision.zero
      ; role
      ; kind
      ; payload = N.to_json payload
      ; provenance = Canonical
      ; redacted = false
      }
  in
  let message form role text =
    N.Semantic.Message
      { form
      ; role
      ; content = [ Text { text; annotations = []; logprobs = Absent } ]
      ; phase = Absent
      }
  in
  let call_metadata =
    { N.Metadata.empty with call_id = Value "opaque:01"; item_id = Value "provider/17" }
  in
  let entries =
    [ retained
        1
        System
        Message
        (message Input Developer "literal <tool/>")
        N.Metadata.empty
    ; retained
        2
        Assistant
        Message
        (message Output Assistant "assistant reply")
        N.Metadata.empty
    ; retained
        3
        Assistant
        Tool_call
        (N.Semantic.Call
           { kind = Function
           ; name = "unexecuted"
           ; namespace = Absent
           ; input_bytes = "{\"n\":1e0}"
           ; async = Absent
           })
        call_metadata
    ; retained
        4
        Tool
        Tool_output
        (N.Semantic.Result
           { relation = Bound (id 3); kind = Function; output = Text "unchanged result" })
        call_metadata
    ]
  in
  let original =
    List.map entries ~f:(fun entry -> Jsonaf.to_string entry.P.History.payload)
  in
  let admitted = create entries |> require in
  let restored = T.of_json (T.to_json admitted) |> require in
  assert (List.equal P.History.equal_entry entries (T.entries restored));
  assert (
    List.equal
      String.equal
      original
      (List.map (T.entries restored) ~f:(fun entry ->
         Jsonaf.to_string entry.P.History.payload)))
;;

let%expect_test
    "neutral seed keeps foreign IDs and literal text without evaluating ChatMD"
  =
  let entries = [ entry 5 "<tool name=never/>\nopaque\\id"; entry 8 "second" ] in
  let seed = require (create entries) in
  assert (List.equal P.History.equal_entry entries (T.entries seed));
  assert (Int64.equal (T.next_history_sequence seed) 0L);
  List.iter (T.entries seed) ~f:(fun e -> print_endline (P.History.Id.to_string e.id));
  print_endline "0 is the new conversation's high water; retained/input IDs stay foreign";
  [%expect
    {|
    14:retained/input:5
    14:retained/input:8
    0 is the new conversation's high water; retained/input IDs stay foreign |}]
;;

let%test_unit "independent compact JSON byte boundary and malformed wire presence" =
  let entries = [ entry 2 "escaped\"\\\n"; entry 4 "é" ] in
  let seed = require (create entries) in
  let wire = T.to_json seed in
  let exact_bytes = String.length (Jsonaf.to_string wire) in
  let limits bytes =
    D.Limits.create ~max_bytes:bytes ~max_depth:256 ~max_fields:10000 ~max_nodes:10000
    |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
    |> Result.ok_or_failwith
  in
  assert (Result.is_ok (create entries ~limits:(limits exact_bytes)));
  assert (Result.is_error (create entries ~limits:(limits (exact_bytes - 1))));
  List.iter
    [ {|{"entries":[],"next_history_sequence":null}|}
    ; {|{"entries":[]}|}
    ; {|{"entries":[],"next_history_sequence":"-1"}|}
    ; {|{"entries":[],"next_history_sequence":"00"}|}
    ; {|{"entries":[],"next_history_sequence":"0","extra":false}|}
    ; {|{"entries":[],"entries":[],"next_history_sequence":"0"}|}
    ]
    ~f:(fun bytes -> assert (Result.is_error (T.of_json (Jsonaf.of_string bytes))));
  assert (
    Result.is_ok
      (T.of_json (Jsonaf.of_string {|{"entries":[],"next_history_sequence":"0"}|})))
;;

let%test_unit "finite invalid entry transforms cannot enter through JSON or sexp" =
  let base = entry 3 "text" in
  List.iter
    [ [ base; base ]
    ; [ { base with redacted = true } ]
    ; [ { base with provenance = Moderator_inserted } ]
    ; [ { base with role = Assistant } ]
    ; [ { base with kind = Tool_call } ]
    ; [ { base with content_revision = P.History.Content_revision.of_int64 1L |> require }
      ]
    ]
    ~f:(fun entries ->
      assert (Result.is_error (create entries));
      let json =
        `Object
          [ "entries", `Array (List.map entries ~f:P.History.entry_to_json)
          ; "next_history_sequence", `String "0"
          ]
      in
      assert (Result.is_error (T.of_json json));
      let sexp =
        Sexp.List
          [ Sexp.List [ Sexp.Atom "entries"; [%sexp_of: P.History.entry list] entries ]
          ; Sexp.List [ Sexp.Atom "next_history_sequence"; Sexp.Atom "0" ]
          ]
      in
      assert (Result.is_error (Or_error.try_with (fun () -> T.t_of_sexp sexp))));
  let sexp =
    Sexp.List
      [ Sexp.List [ Sexp.Atom "entries"; Sexp.List [] ]
      ; Sexp.List [ Sexp.Atom "next_history_sequence"; Sexp.Atom "-1" ]
      ]
  in
  assert (Result.is_error (Or_error.try_with (fun () -> T.t_of_sexp sexp)))
;;

exception Injected of int

let%test_unit
    "every observed checkpoint preserves exception identity and reusable immutable input"
  =
  let entries = [ entry 0 "first"; entry 1 "second" ] in
  let observed = ref 0 in
  ignore (require (create entries ~checkpoint:(fun () -> incr observed)) : T.t);
  assert (!observed > 0);
  for ordinal = 1 to !observed do
    let calls = ref 0 in
    let injected = Injected ordinal in
    let raised =
      try
        ignore
          (create entries ~checkpoint:(fun () ->
             incr calls;
             if !calls = ordinal then raise injected)
           : (T.t, P.Error.t) Result.t);
        false
      with
      | exn -> phys_equal exn injected
    in
    assert raised;
    let reusable = require (create entries) in
    assert (List.equal P.History.equal_entry entries (T.entries reusable))
  done
;;
