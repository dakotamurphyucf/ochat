open! Core
module S = Agent_store
module D = Document_schema
module P = Agent_protocol

let checked result =
  Result.map_error result ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let fixture =
  {|{"format":"ochat.document","schema_version":1,"kind":"store.blob_metadata","future_envelope":{"blob":"blb_unknown_reference"},"payload":{"blob":{"id":"blb_private_named","kind":"file","media_type":"application/vnd.ochat.completion+json","byte_length":"2","digest":"44136fa355b3678a1146ad16f7e8649e94fb4fc21fe77e8310c060f61caaff8a","future_blob":{"preserve":null}},"creating_principal":"pri_private_named","target_session":"ses_private_named","allowed_use":"job_result:ses_private_named:job_private_named:0:1","created_at":"2026-08-15T12:00:00Z","durable":false,"future_metadata":"blb_escaped_reference"}}|}
;;

let field json name =
  match D.Json.field json ~name with
  | Value value -> value
  | Null -> `Null
  | Absent -> failwith ("absent " ^ name)
;;

let%expect_test "metadata phase edits preserve nested unknowns and optional absence" =
  let metadata = S.Blob_metadata_document.of_bytes fixture |> checked in
  let stage = S.Blob_stage_documents.create metadata |> checked in
  let temporary = S.Blob_stage_documents.temporary_bytes stage in
  let durable = S.Blob_stage_documents.durable_bytes stage in
  let reopened =
    S.Blob_stage_documents.of_publications
      ~temporary_bytes:temporary
      ~durable_bytes:durable
    |> checked
  in
  assert (S.Blob_stage_documents.equal stage reopened);
  let durable_document =
    S.Blob_metadata_document.to_document (S.Blob_stage_documents.durable stage) |> checked
  in
  let json = D.Document.json durable_document in
  assert (
    Jsonaf.exactly_equal
      (field json "future_envelope")
      (`Object [ "blob", `String "blb_unknown_reference" ]));
  let payload = D.Document.payload durable_document in
  assert (
    Jsonaf.exactly_equal
      (field (field payload "blob") "future_blob")
      (`Object [ "preserve", `Null ]));
  assert (
    match D.Json.field payload ~name:"expires_at" with
    | Absent -> true
    | _ -> false);
  let spaced = " \n" ^ temporary ^ "\n " in
  let original =
    S.Blob_stage_documents.of_publications ~temporary_bytes:spaced ~durable_bytes:durable
    |> checked
  in
  assert (String.equal spaced (S.Blob_stage_documents.temporary_bytes original));
  print_endline
    "phase/unknowns/absence retained; exact original whitespace survives restart";
  [%expect
    {|phase/unknowns/absence retained; exact original whitespace survives restart|}]
;;

let edit_payload name value =
  let original =
    D.Document.decode ~limits:S.Blob_metadata_document.limits fixture |> checked
  in
  match D.Document.json original, D.Document.payload original with
  | `Object envelope, `Object fields ->
    D.Document.inspect
      ~limits:S.Blob_metadata_document.limits
      (`Object
          (List.Assoc.add
             envelope
             ~equal:String.equal
             "payload"
             (`Object (List.Assoc.add fields ~equal:String.equal name value))))
    |> checked
  | _ -> assert false
;;

let%expect_test "metadata optional null and unsupported required semantics reject" =
  List.iter [ "target_session"; "expires_at" ] ~f:(fun name ->
    assert (
      Result.is_error (S.Blob_metadata_document.of_document (edit_payload name `Null))));
  let original =
    D.Document.decode ~limits:S.Blob_metadata_document.limits fixture |> checked
  in
  let required =
    match D.Document.json original with
    | `Object fields ->
      D.Document.inspect
        ~limits:S.Blob_metadata_document.limits
        (`Object
            (List.Assoc.add
               fields
               ~equal:String.equal
               "required_semantics"
               (`Array [ `String "future_blob_owner" ])))
      |> checked
    | _ -> assert false
  in
  assert (Result.is_error (S.Blob_metadata_document.of_document required));
  let stage =
    S.Blob_stage_documents.create
      (S.Blob_metadata_document.of_document original |> checked)
    |> checked
  in
  let modified =
    edit_payload "future_metadata" (`String "other")
    |> S.Blob_metadata_document.of_document
    |> checked
  in
  let modified =
    S.Blob_metadata_document.with_value
      modified
      { (S.Blob_metadata_document.value modified) with durable = true }
    |> checked
  in
  assert (
    Result.is_error
      (S.Blob_stage_documents.of_publications
         ~temporary_bytes:(S.Blob_stage_documents.temporary_bytes stage)
         ~durable_bytes:(S.Blob_metadata_document.to_bytes modified |> checked)));
  print_endline
    "null/semantics reject; publication pair cannot drop or replace unknown evidence";
  [%expect
    {|null/semantics reject; publication pair cannot drop or replace unknown evidence|}]
;;

let%expect_test
    "intent exact metadata strings remain bounded and reference ownership validates"
  =
  let metadata = S.Blob_metadata_document.of_bytes fixture |> checked in
  let stage = S.Blob_stage_documents.create metadata |> checked in
  let value = S.Blob_metadata_document.value metadata in
  let reference =
    P.Job_artifact.create
      ~session_id:(Option.value_exn value.target_session)
      ~job_id:
        (P.Id.Job.of_string "job_private_named"
         |> Result.map_error ~f:(fun e -> e.P.Error.message)
         |> Result.ok_or_failwith)
      ~generation:0
      ~attempt:1
      ~blob:value.blob
    |> Result.map_error ~f:(fun e -> e.P.Error.message)
    |> Result.ok_or_failwith
  in
  let intent = S.Job_result_intent_document.create ~reference ~stage |> checked in
  let document = S.Job_result_intent_document.to_document intent |> checked in
  let reopened = S.Job_result_intent_document.of_document document |> checked in
  assert (S.Blob_stage_documents.equal stage (S.Job_result_intent_document.stage reopened));
  let wrong =
    P.Job_artifact.create
      ~session_id:reference.session_id
      ~job_id:reference.job_id
      ~generation:reference.generation
      ~attempt:2
      ~blob:reference.blob
    |> Result.map_error ~f:(fun e -> e.P.Error.message)
    |> Result.ok_or_failwith
  in
  assert (Result.is_error (S.Job_result_intent_document.create ~reference:wrong ~stage));
  let inflated =
    edit_payload "future_metadata" (`String (String.make 16000 'x'))
    |> S.Blob_metadata_document.of_document
    |> checked
  in
  let large_stage = S.Blob_stage_documents.create inflated |> checked in
  assert (
    Result.is_error (S.Job_result_intent_document.create ~reference ~stage:large_stage));
  print_endline
    "exact strings roundtrip; foreign attempt fails; 32KiB intent bound unchanged";
  [%expect
    {|exact strings roundtrip; foreign attempt fails; 32KiB intent bound unchanged|}]
;;
