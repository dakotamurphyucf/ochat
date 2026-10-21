open! Core
module P = Agent_protocol
module A = P.Authoring
module B = P.Authoring_bundle.Bundle
module W = A.Wire

let incompatible message =
  Error (P.Error.create Incompatible_protocol ~message ~retryable:false ())
;;

type t = Authoring_connection.t

let send = Authoring_connection.send
let create connection = Authoring_connection.create connection ~feature:A.Features.bundle
let capabilities = Authoring_connection.capabilities

let same_owner t owner =
  P.Id.Server.equal owner.B.Owner.host (Authoring_connection.host t)
  && P.Id.Principal.equal owner.principal (Authoring_connection.principal t)
;;

let member_length manifest id =
  List.find_map (B.Manifest.members manifest) ~f:(fun member ->
    let source = B.Member.source member in
    if Chatml_source.Id.equal (A.Source.id source) id
    then Some (A.Source.byte_length source)
    else None)
;;

let project_matches plan receipt =
  let module P = B.Manifest.Project in
  let module R = B.Receipt.Project in
  let entry_matches =
    match P.entry plan with
    | Declared ->
      List.exists (P.source_members plan) ~f:(fun source ->
        Chatml_source.Id.equal source (R.entry_source receipt))
    | Captured captured ->
      Chatml_source.Id.equal
        (A.Source.id (P.Captured_entry.source captured))
        (R.entry_source receipt)
  in
  let lock_matches =
    match P.mode plan with
    | Exact_lock id -> Chatml_source.Id.equal id (R.selected_lock receipt)
    | Compose { generated_lock; _ } ->
      Chatml_source.Id.equal generated_lock (R.selected_lock receipt)
  in
  Chatml_source.Id.equal (P.root_source plan) (R.root_source receipt)
  && entry_matches
  && lock_matches
  && Option.value_map (P.selection plan).environment ~default:true ~f:(fun requested ->
    String.equal requested (R.environment receipt))
;;

let receipt_matches_manifest manifest receipt =
  let module M = B.Manifest in
  let module R = B.Receipt in
  let rec scripts_match selected_scripts result_scripts =
    match selected_scripts, result_scripts with
    | [], [] -> true
    | selected :: selected_rest, result :: result_rest ->
      let selector = M.Script.selector selected in
      let selected_selector, result_digest, project =
        match result with
        | R.Script.Source_only { selector; body_digest } -> selector, body_digest, None
        | Selected_project { selector; body_digest; project } ->
          selector, body_digest, Some project
      in
      M.Script.Selector.equal selector selected_selector
      && Chatml_source.Digest.equal (M.Script.Selector.body_digest selector) result_digest
      && (match M.Script.choice selected, project with
          | Source_only, None -> true
          | Selected_project plan, Some result -> project_matches plan result
          | Source_only, Some _ | Selected_project _, None -> false)
      && scripts_match selected_rest result_rest
    | [], _ :: _ | _ :: _, [] -> false
  in
  Chatml_source.Digest.equal (M.fingerprint manifest) (R.manifest_fingerprint receipt)
  &&
  match M.basis manifest, R.basis receipt with
  | Project plan, Project result -> project_matches plan result
  | Chatmd_document plan, Chatmd_document result ->
    Chatml_source.Id.equal
      (M.Chatmd_document.root_source plan)
      (R.Chatmd_document.root_source result)
    && scripts_match (M.Chatmd_document.scripts plan) (R.Chatmd_document.scripts result)
  | Project _, Chatmd_document _ | Chatmd_document _, Project _ -> false
;;

let outcome_matches_manifest manifest = function
  | B.Commit.Published receipt -> receipt_matches_manifest manifest receipt
  | Accepted_for_validation | Rejected _ -> true
;;

let state_matches_manifest manifest = function
  | B.Status.Staging progress ->
    let lengths =
      List.fold
        (B.Manifest.members manifest)
        ~init:(Map.empty (module Chatml_source.Id))
        ~f:(fun lengths member ->
          let source = B.Member.source member in
          Map.set lengths ~key:(A.Source.id source) ~data:(A.Source.byte_length source))
    in
    List.for_all progress ~f:(fun item ->
      match Map.find lengths (B.Status.Progress.source item) with
      | None -> false
      | Some length -> B.Status.Progress.accepted_until_byte item <= length)
  | Published receipt -> receipt_matches_manifest manifest receipt
  | Validating | Rejected _ | Expired -> true
;;

let begin_upload t (request : B.Begin.request) =
  let open Result.Let_syntax in
  match send t (W.Request.Bundle_begin request) with
  | Ok (W.Response.Bundle_begin response)
    when same_owner t (B.Begin.owner response)
         && P.Idempotency_key.equal
              (B.Begin.idempotency_key response)
              request.idempotency_key
         && Chatml_source.Digest.equal
              (B.Begin.manifest_fingerprint response)
              (B.Manifest.fingerprint request.manifest)
         && Option.equal
              A.Context.Expected.equal
              (B.Begin.parent_context response)
              request.parent_context -> Ok response
  | Ok _ -> incompatible "bundle begin changed owner or request identity"
  | Error _ as failure -> failure
;;

let upload_chunk t ~expected_manifest (request : B.Chunk.request) =
  match member_length expected_manifest request.source with
  | None -> incompatible "bundle chunk source is absent from expected manifest"
  | Some length
    when request.byte_offset > length
         || String.length request.bytes > length - request.byte_offset ->
    incompatible "bundle chunk exceeds expected member length"
  | Some length ->
    let accepted_end = request.byte_offset + String.length request.bytes in
    (match send t (W.Request.Bundle_chunk request) with
     | Ok (W.Response.Bundle_chunk response)
       when same_owner t (B.Chunk.owner response)
            && P.Id.Authoring_upload.equal (B.Chunk.upload_id response) request.upload_id
            && Chatml_source.Id.equal (B.Chunk.source response) request.source
            && B.Chunk.accepted_until_byte response >= accepted_end
            && B.Chunk.accepted_until_byte response <= length -> Ok response
     | Ok _ -> incompatible "bundle chunk changed owner, source or accepted range"
     | Error _ as failure -> failure)
;;

let commit t ~expected_manifest (request : B.Commit.request) =
  match send t (W.Request.Bundle_commit request) with
  | Ok (W.Response.Bundle_commit response)
    when same_owner t (B.Commit.owner response)
         && P.Id.Authoring_upload.equal (B.Commit.upload_id response) request.upload_id
         && outcome_matches_manifest expected_manifest (B.Commit.outcome response)
         && P.Idempotency_key.equal
              (B.Commit.idempotency_key response)
              request.idempotency_key -> Ok response
  | Ok _ -> incompatible "bundle commit changed owner or request identity"
  | Error _ as failure -> failure
;;

let status t ~expected_manifest (request : B.Status.request) =
  match send t (W.Request.Bundle_status request) with
  | Ok (W.Response.Bundle_status response)
    when same_owner t (B.Status.owner response)
         && P.Id.Authoring_upload.equal (B.Status.upload_id response) request.upload_id
         && state_matches_manifest expected_manifest (B.Status.state response) ->
    Ok response
  | Ok _ -> incompatible "bundle status changed owner or upload identity"
  | Error _ as failure -> failure
;;
