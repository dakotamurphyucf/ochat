open! Core
module P = Agent_protocol
module Doc = Developer_result_intent_document

type t =
  { document : Doc.t
  ; file : Result_intent_file.t
  ; bytes : string
  }

let document t = t.document

let stage t =
  match Doc.selection t.document with
  | Doc.Selection.Inline _ -> None
  | Doc.Selection.Artifact { stage; _ } -> Some stage
;;

let corrupt message = Error (Store_error.Corrupt message)
let max_frame_bytes = 128 * 1024

let make ~runs ~root ~claim document =
  let job = Developer_run_store.Claim.job claim in
  let owner = Developer_run_store.Claim.owner claim in
  let open Result.Let_syntax in
  let%bind () =
    if
      Developer_run_store.Claim.issued_by claim runs
      && Developer_run_store.Result_root.issued_by runs root
      && P.Execution_owner.equal_developer owner (Doc.owner document)
      && P.Execution_owner.equal_developer owner (P.Developer_job.owner job)
      && Chatml_source.Digest.equal
           (P.Developer_job.capture_fingerprint job)
           (Doc.capture_fingerprint document)
      && P.Id.Job.equal (P.Developer_job.id job) (Doc.job_id document)
      && Int.equal (Developer_run_store.Claim.attempt claim) (Doc.attempt document)
    then Ok ()
    else corrupt "developer result intent differs from its physical claim or root"
  in
  let%bind _ =
    P.Developer_job.with_state job (Doc.after document)
    |> Result.map_error ~f:(fun _ ->
      Store_error.Corrupt "developer result intent has an invalid terminal state")
  in
  let%bind file =
    Result_intent_file.developer
      ~root
      ~owner
      ~job:(Doc.job_id document)
      ~attempt:(Doc.attempt document)
  in
  let%bind named = Doc.to_document document |> Document_fields.store in
  let%map bytes = Result_intent_file.encode named ~limits:Doc.limits in
  { document; file; bytes }
;;

let save t = Result_intent_file.save_exact t.file ~max_frame_bytes t.bytes

let remove_exact t =
  Result_intent_file.remove_exact t.file ~expected:t.bytes ~max_frame_bytes
;;
