open! Core
module P = Agent_protocol
module H = History_entry
module Payload = H.Payload

let ok = function
  | Ok value -> value
  | Error (error : P.Error.t) -> failwith error.message
;;

let text_ok = Result.ok_or_failwith
let generator = P.Id.Generator.create ~bytes:(fun n -> String.make n '\000')
let id = H.Id.create ~namespace:"public-tests" ~sequence:0 |> text_ok
let session_id = P.Id.Session.create_with generator
let timestamp = P.Timestamp.of_time_ns Time_ns.epoch

let principal scopes =
  P.Principal.create
    ~id:(P.Id.Principal.create_with generator)
    ~authentication_kind:"test"
    ~scopes:(P.Scope.Set.of_list scopes)
    ~attributes:[]
  |> ok
;;

let entry role =
  let semantic =
    Payload.Semantic.create
      (Message
         { form = Input
         ; role
         ; phase = Value "analysis"
         ; content =
             [ Text
                 { text = "visible text"
                 ; annotations = [ `String "secret annotation" ]
                 ; logprobs = Value (`String "secret logprobs")
                 }
             ; Unknown { kind = "future.media"; raw = `Object [ "secret", `True ] }
             ]
         })
      ~metadata:Payload.Metadata.empty
    |> text_ok
  in
  let payload =
    Payload.reconstructed
      semantic
      ~provider:"test"
      ~raw:(`Object [ "secret_raw", `String "opaque" ])
    |> text_ok
  in
  H.create_with_id ~id payload
;;

let internal entry =
  P.History.
    { id = H.id entry
    ; role = System
    ; kind = Message
    ; payload = Payload.to_json (H.payload entry)
    ; provenance = Canonical
    ; redacted = false
    }
;;

let event entry =
  P.Event.Durable.of_payload
    ~session_id
    ~sequence:1L
    ~revision:1L
    ~timestamp
    (History_appended [ internal entry ])
;;

let public principal entry =
  Agent_server.Principal_projection.durable principal (event entry) |> ok
;;

let only_entry event =
  match event.P.Public.Durable.body with
  | Full (History_appended [ entry ]) | Filtered (History_appended [ entry ]) -> entry
  | _ -> failwith "expected one public entry"
;;

let contains_secret json = String.is_substring (Jsonaf.to_string json) ~substring:"secret"

let%expect_test
    "public transcript grants separate full evidence from readable fields and hidden \
     ordering"
  =
  let original = entry Developer in
  let full =
    public (principal [ View_session_transcript; View_security_state ]) original
  in
  let visible = public (principal [ View_session_transcript ]) original in
  let hidden = public (principal [ View_security_state ]) original in
  print_s
    [%sexp (P.Public.History.header (only_entry visible) : Transcript.Header.t option)];
  print_s
    [%sexp
      (contains_secret (P.Public.Durable.to_json full) : bool)
    , (contains_secret (P.Public.Durable.to_json visible) : bool)];
  print_s
    [%sexp
      (hidden.sequence : int64)
    , ((match hidden.body with
        | Hidden -> true
        | Full _ | Filtered _ -> false)
       : bool)];
  let payload = Option.value_exn (P.Public.History.full_payload (only_entry full)) in
  print_s
    [%sexp
      (Jsonaf.exactly_equal
         (Payload.to_json payload)
         (Payload.to_json (H.payload original))
       : bool)];
  [%expect
    {|
    ((Message Developer))
    (true false)
    (1 true)
    true
    |}]
;;

let%expect_test
    "public history roundtrip cannot turn a readable view into canonical evidence"
  =
  let view =
    public (principal [ View_session_transcript ]) (entry Developer) |> only_entry
  in
  let encoded = P.Public.History.to_json view in
  let decoded = P.Public.History.of_json encoded |> ok in
  print_s
    [%sexp
      (P.Public.History.equal view decoded : bool)
    , (Option.is_none (P.Public.History.full_payload decoded) : bool)];
  print_s [%sexp (Result.is_error (P.History.entry_of_json encoded) : bool)];
  [%expect
    {|
    (true true)
    true
    |}]
;;

let%expect_test
    "activity receipts retain unknown envelope and nested activity fields exactly"
  =
  let source = Transcript.Source_id.of_string "source" |> text_ok in
  let attempt = Transcript.Attempt_id.of_string "attempt" |> text_ok in
  let key =
    P.Activity.Key.create ~scope:{ source; attempt } ~call_alias:"call" ~parent:None |> ok
  in
  let progress =
    P.Activity.Tool.Progress
      { key; progress = { channel = Stdout; update = Append "hello" } }
  in
  let event =
    P.Event.Recoverable.create
      ~session_id
      ~operation_id:(P.Id.Operation.create_with generator)
      ~operation_sequence:1L
      ~anchor_sequence:0L
      ~timestamp
      ~invocation_id:None
      ~parent_invocation_id:None
      (Tool_activity progress)
    |> ok
  in
  let json =
    match P.Event.Recoverable.to_json event with
    | `Object fields ->
      `Object
        (("future", `Number "1e0")
         :: List.map fields ~f:(fun (name, value) ->
           if String.equal name "payload"
           then (
             match value with
             | `Object parts -> name, `Object (("future_activity", `Null) :: parts)
             | _ -> assert false)
           else name, value))
    | _ -> assert false
  in
  let decoded = P.Event.Recoverable.of_json json |> ok in
  print_s [%sexp (Jsonaf.exactly_equal json (P.Event.Recoverable.to_json decoded) : bool)];
  print_s
    [%sexp
      (Result.is_error
         (P.Event.Recoverable.create
            ~session_id
            ~operation_id:event.operation_id
            ~operation_sequence:0L
            ~anchor_sequence:0L
            ~timestamp
            ~invocation_id:None
            ~parent_invocation_id:None
            event.payload)
       : bool)];
  [%expect
    {|
    true
    true
    |}]
;;

let%expect_test "wire upgrade rejects protocol one while current is two" =
  print_s [%sexp (P.Version.current : P.Version.t)];
  print_s
    [%sexp
      (Result.is_error
         (P.Version.negotiate
            ~client_min:P.Version.initial
            ~client_max:P.Version.initial
            ~supported:[ P.Version.current ])
       : bool)];
  [%expect
    {|
    ((major 2) (minor 0))
    true
    |}]
;;

let%expect_test "nested activity binds the actual parent scope even when aliases repeat" =
  let source value = Transcript.Source_id.of_string value |> text_ok in
  let attempt = Transcript.Attempt_id.of_string "attempt" |> text_ok in
  let parent_scope = Transcript.Scope.Key.{ source = source "parent"; attempt } in
  let child_scope = Transcript.Scope.Key.{ source = source "child"; attempt } in
  let parent = P.Activity.Key.{ scope = parent_scope; call_alias = "same-alias" } in
  let key =
    P.Activity.Key.create
      ~scope:child_scope
      ~call_alias:"same-alias"
      ~parent:(Some parent)
    |> ok
  in
  let progress =
    P.Activity.Tool.Progress
      { key; progress = { channel = Activity; update = Append "child" } }
  in
  let restored =
    P.Activity.Tool.of_json (P.Activity.Tool.to_json progress)
    |> ok
    |> P.Activity.Tool.key
  in
  print_s [%sexp (P.Activity.Key.equal key restored : bool)];
  print_s
    [%sexp
      (Result.is_error
         (P.Activity.Key.create
            ~scope:parent_scope
            ~call_alias:"same-alias"
            ~parent:(Some parent))
       : bool)];
  [%expect
    {|
    true
    true
    |}]
;;

let%expect_test "native public shared events reject malformed domain children" =
  let operation =
    P.Operation.
      { id = P.Id.Operation.create_with generator
      ; generation = 0
      ; kind = Turn User_submit
      ; state = Running
      ; started_at = timestamp
      ; updated_at = timestamp
      }
  in
  let admit operation =
    P.Public.Durable.Shared_payload.of_internal (Operation_started operation)
  in
  print_s [%sexp (Result.is_ok (admit operation) : bool)];
  print_s [%sexp (Result.is_error (admit { operation with generation = -1 }) : bool)];
  [%expect
    {|
    true
    true
    |}]
;;

let%expect_test "native history admission preserves canonical and header invariants" =
  let raw =
    List.init 125 ~f:Fn.id |> List.fold ~init:`Null ~f:(fun nested _ -> `Array [ nested ])
  in
  let semantic =
    Payload.Semantic.create
      (Message
         { form = Input
         ; role = Developer
         ; content = [ Unknown { kind = "future.deep"; raw } ]
         ; phase = Absent
         })
      ~metadata:Payload.Metadata.empty
    |> text_ok
  in
  let payload = Payload.authored semantic in
  let invalid = H.create_with_id ~id payload in
  print_s [%sexp (Result.is_error (Payload.validate payload) : bool)];
  print_s
    [%sexp (Result.is_error (P.Public.History.full invalid ~provenance:Canonical) : bool)];
  let redacted header =
    P.Public.History.redacted
      id
      ~provenance:Canonical
      (P.Public.History.Redaction.create ~disclosed_header:(Some header))
  in
  print_s [%sexp (Result.is_error (redacted (Unknown "")) : bool)];
  print_s [%sexp (Result.is_ok (redacted (Unknown "future")) : bool)];
  [%expect
    {|
    true
    true
    true
    true
    |}]
;;

let%expect_test
    "public history windows reject duplicate identities and oversized envelopes"
  =
  let value = P.Public.History.full (entry Developer) ~provenance:Canonical |> ok in
  let window entries =
    P.Public.History.Window.
      { entries
      ; previous_cursor = None
      ; next_cursor = None
      ; reached_start = true
      ; reached_end = true
      ; structurally_complete = true
      }
  in
  let valid = window [ value ] in
  let duplicate = window [ value; value ] in
  let oversized =
    match P.Public.History.Window.to_json valid with
    | `Object fields ->
      `Object (("future", `String (String.make (16 * 1024 * 1024) 'x')) :: fields)
    | _ -> assert false
  in
  assert (Result.is_error (P.Public.History.Window.of_json oversized));
  print_s [%sexp (Result.is_ok (P.Public.History.Window.validate valid) : bool)];
  print_s [%sexp (Result.is_error (P.Public.History.Window.validate duplicate) : bool)];
  print_s
    [%sexp
      (Result.is_error
         (P.Public.History.Window.of_json (P.Public.History.Window.to_json duplicate))
       : bool)];
  let source = event (entry Developer) in
  print_s
    [%sexp
      (Result.is_error
         (P.Public.Durable.of_internal_envelope
            source
            ~body:(Full (History_appended [ value; value ]))
            ~extension_status:None
            ~replacement_snapshot:None)
       : bool)];
  [%expect
    {|
    true
    true
    true
    true
    |}]
;;
