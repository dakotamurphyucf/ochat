open! Core
module P = Agent_protocol
module F = Document_fields
module D = Document_schema
module Ref = Recovery_asset_reference
module Stage = Blob_stage_documents

type t =
  { reference : Ref.t
  ; stage : Stage.t
  ; contents : string
  }

let reference t = t.reference
let stage t = t.stage
let contents t = t.contents
let kind = "store.recovery_asset_publication"
let max_frame_bytes = 16 * 1024
let limits = F.limits ~max_bytes:(max_frame_bytes - 52) |> Result.ok |> Option.value_exn

let filename citation ~part =
  let%map.Result tag = Blob_metadata.Recovery_use.for_part citation ~part in
  let digest = String.suffix tag 64 in
  digest ^ ".frame"
;;

let validate reference stage =
  let observed = Blob_metadata_document.value (Stage.temporary stage) in
  let source =
    P.Recovery_window.Citation.scope (Ref.citation reference)
    |> P.Recovery_window.Scope.session
  in
  let target = Blob_metadata.Target.Session (P.Session_ref.session_id source) in
  if
    Blob_metadata.blob_equal observed.blob (Ref.blob reference)
    && Blob_metadata.Target.equal observed.target target
    && String.equal observed.allowed_use (Ref.allowed_use reference)
    && Option.is_none observed.expires_at
    && not observed.durable
  then Ok ()
  else Error (Store_error.Corrupt "recovery asset stage differs from its exact selection")
;;

let select reference ~principal ~now =
  let open Result.Let_syntax in
  let source =
    P.Recovery_window.Citation.scope (Ref.citation reference)
    |> P.Recovery_window.Scope.session
  in
  let%bind metadata =
    Blob_metadata_document.create
      { blob = Ref.blob reference
      ; creating_principal = principal
      ; target = Blob_metadata.Target.Session (P.Session_ref.session_id source)
      ; allowed_use = Ref.allowed_use reference
      ; created_at = now
      ; expires_at = None
      ; durable = false
      }
    |> F.store
  in
  let%bind stage = Stage.create metadata |> F.store in
  let%bind () = validate reference stage in
  let payload =
    `Object
      [ "reference", Ref.to_json reference
      ; "temporary", `String (Stage.temporary_bytes stage)
      ; "durable", `String (Stage.durable_bytes stage)
      ]
  in
  let%bind document = D.Document.create ~limits ~kind ~version:1 ~payload |> F.store in
  let%map contents = Result_intent_file.encode document ~limits in
  { reference; stage; contents }
;;

let decode contents =
  let open Result.Let_syntax in
  let%bind document = Result_intent_file.decode contents ~limits ~max_frame_bytes in
  let%bind () = F.expect document ~kind ~version:1 |> F.store in
  let payload = D.Document.payload document in
  let%bind reference =
    F.required payload "reference" (fun json ->
      Ref.of_json json
      |> Result.map_error ~f:(fun _ -> D.Error.Malformed "invalid exact asset reference"))
    |> F.store
  in
  let%bind temporary_bytes = F.required payload "temporary" F.string |> F.store in
  let%bind durable_bytes = F.required payload "durable" F.string |> F.store in
  let%bind stage = Stage.of_publications ~temporary_bytes ~durable_bytes |> F.store in
  let%map () = validate reference stage in
  { reference; stage; contents }
;;
