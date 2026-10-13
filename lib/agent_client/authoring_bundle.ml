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

let upload_chunk t (request : B.Chunk.request) =
  let accepted_end = request.byte_offset + String.length request.bytes in
  match send t (W.Request.Bundle_chunk request) with
  | Ok (W.Response.Bundle_chunk response)
    when same_owner t (B.Chunk.owner response)
         && P.Id.Authoring_upload.equal (B.Chunk.upload_id response) request.upload_id
         && Chatml_source.Id.equal (B.Chunk.source response) request.source
         && B.Chunk.accepted_until_byte response >= accepted_end -> Ok response
  | Ok _ -> incompatible "bundle chunk changed owner, source or accepted range"
  | Error _ as failure -> failure
;;

let commit t (request : B.Commit.request) =
  match send t (W.Request.Bundle_commit request) with
  | Ok (W.Response.Bundle_commit response)
    when same_owner t (B.Commit.owner response)
         && P.Id.Authoring_upload.equal (B.Commit.upload_id response) request.upload_id
         && P.Idempotency_key.equal
              (B.Commit.idempotency_key response)
              request.idempotency_key -> Ok response
  | Ok _ -> incompatible "bundle commit changed owner or request identity"
  | Error _ as failure -> failure
;;

let status t (request : B.Status.request) =
  match send t (W.Request.Bundle_status request) with
  | Ok (W.Response.Bundle_status response)
    when same_owner t (B.Status.owner response)
         && P.Id.Authoring_upload.equal (B.Status.upload_id response) request.upload_id ->
    Ok response
  | Ok _ -> incompatible "bundle status changed owner or upload identity"
  | Error _ as failure -> failure
;;
