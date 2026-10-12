open! Core
module P = Agent_protocol
module Seed = P.Session_seed
module Payload = History_entry.Payload

let ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp "seed conformance rejected", (error : P.Error.t)]
;;

(* HTTP admission sorts object fields. The protocol's canonical encoding keeps
   array order and every string/number spelling, including opaque captured raw. *)
let same_document expected actual =
  String.equal
    (P.Json_codec.canonical_string expected |> ok)
    (P.Json_codec.canonical_string actual |> ok)
;;

let transcript ~developer =
  let id sequence =
    History_entry.Id.create ~namespace:"conformance/opaque-transcript" ~sequence
    |> Result.ok_or_failwith
  in
  let metadata =
    { Payload.Metadata.empty with
      call_id = Value "opaque:007\\call"
    ; item_id = Value "provider/017"
    }
  in
  let retained sequence role kind view metadata =
    let semantic = Payload.Semantic.create view ~metadata |> Result.ok_or_failwith in
    let payload =
      Payload.captured
        semantic
        ~origin:Payload.Origin.unavailable
        ~raw:
          (`Object
              [ "opaque-id", `String "007\\caller"
              ; "negative-zero", `Number "-0"
              ; "exponent", `Number "1.2300e+02"
              ])
      |> Result.ok_or_failwith
    in
    P.History.
      { id = id sequence
      ; content_revision = Content_revision.zero
      ; role
      ; kind
      ; payload = Payload.to_json payload
      ; provenance = Canonical
      ; redacted = false
      }
  in
  let message form role text =
    Payload.Semantic.Message
      { form
      ; role
      ; content = [ Text { text; annotations = []; logprobs = Absent } ]
      ; phase = Absent
      }
  in
  [ retained 1 System Message (message Input Developer developer) Payload.Metadata.empty
  ; retained
      2
      Assistant
      Message
      (message Output Assistant "retained assistant")
      Payload.Metadata.empty
  ; retained
      3
      Assistant
      Tool_call
      (Payload.Semantic.Call
         { kind = Function
         ; name = "never_execute"
         ; namespace = Absent
         ; input_bytes = "{\"n\":1e0}"
         ; async = Absent
         })
      metadata
  ; retained
      4
      Tool
      Tool_output
      (Payload.Semantic.Result
         { relation = Bound (id 3); kind = Function; output = Text "retained result" })
      metadata
  ]
  |> Seed.Transcript.create
       ~next_history_sequence:0L
       ~limits:Document_schema.Limits.default
       ~max_entries:16
       ~max_work_steps:100_000
       ~checkpoint:Eio.Fiber.check
  |> ok
;;

let check ~spec ~request ~key_prefix =
  let command (command : P.Command.t) =
    request command |> ok |> Support.Public_view.non_history
  in
  let definition =
    match spec.P.Session.Spec.prompt with
    | Catalog definition -> definition
    | Local_path _ | Generated _ ->
      failwith "seed conformance requires an authentic catalog definition"
  in
  let revision =
    match command (Prompt_get { prompt_id = definition }) with
    | Prompt_get { current_revision = Some revision; _ } -> revision
    | _ -> failwith "seed conformance catalog is not ready"
  in
  let code : Seed.Code_selection.t =
    Catalog { definition_id = definition; expected_revision = revision }
  in
  let key = P.Idempotency_key.of_string (key_prefix ^ ":seed") |> ok in
  let seed transcript =
    Seed.Request.create spec ~code ~transcript ~idempotency_key:key |> ok
  in
  let retained = transcript ~developer:"literal <tool name=never/>\nopaque\\id" in
  let original = P.Command.Session_seed (seed retained) in
  let receipt () =
    match
      command
        (Command_receipt
           { method_name = P.Command.method_name original
           ; original_params = P.Command.params original
           })
    with
    | Command_receipt receipt -> receipt
    | _ -> failwith "seed receipt response kind"
  in
  (match receipt () with
   | Missing -> ()
   | _ -> failwith "unsubmitted seed has a receipt");
  let execute () =
    match command original with
    | Session_seed result -> result
    | _ -> failwith "seed response kind"
  in
  let created = execute () in
  (match receipt () with
   | Committed (Created_session session_id) ->
     assert (P.Id.Session.equal session_id created.session_id)
   | _ -> failwith "seed receipt does not name its committed session");
  assert (P.Id.Session.equal created.session_id (execute ()).session_id);
  let snapshot () =
    match
      request (Session_get { session_id = created.session_id; history = None }) |> ok
    with
    | Session_get snapshot -> snapshot
    | _ -> failwith "seed snapshot response kind"
  in
  let before = snapshot () in
  let fields = P.Public.Snapshot.fields before in
  assert (P.Id.Session.equal fields.session.id created.session_id);
  assert (P.Session.equal_desired_state fields.session.desired_state Stopped);
  (match fields.session.observed_state with
   | Stopped -> ()
   | Queued_for_slot
   | Starting
   | Recovering
   | Idle
   | Running_turn _
   | Compacting _
   | Waiting_for_permission _
   | Stopping
   | Failed _ -> failwith "seed activated its code");
  assert (
    Option.equal P.Id.Prompt_revision.equal fields.session.prompt_revision (Some revision));
  assert (Option.is_none fields.session.active_operation);
  assert fields.canonical_history.structurally_complete;
  List.iter2_exn
    (Seed.Transcript.entries retained)
    fields.canonical_history.entries
    ~f:(fun expected actual ->
      assert (P.History.Id.equal expected.id actual.P.Public.History.id);
      assert (
        P.History.Content_revision.equal expected.content_revision actual.content_revision);
      let payload = P.Public.History.full_payload actual |> Option.value_exn in
      assert (same_document expected.payload (Payload.to_json payload)));
  let changed = seed (transcript ~developer:"different retained developer") in
  (match request (Session_seed changed) with
   | Error error -> assert (P.Error.equal_code error.code Idempotency_conflict)
   | Ok _ -> failwith "changed seed reused the original key");
  assert (
    same_document
      (P.Public.Snapshot.to_json before)
      (P.Public.Snapshot.to_json (snapshot ())))
;;
