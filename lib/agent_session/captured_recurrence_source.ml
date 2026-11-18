open! Core
module P = Agent_protocol
module Store = Agent_store.Prompt_artifact_store

type t =
  { revision : Prompt_revision.t
  ; origin : Recurrence_child_origin.t
  }

let revision t = t.revision
let origin t = t.origin

let restore ~artifact_store ~content_limits ~record ~state ~checkpoint =
  let open Result.Let_syntax in
  checkpoint ();
  let%bind origin = Recurrence_child_origin.of_record record in
  let%bind () = Recurrence_child_origin.validate_source origin state in
  let reservation = Recurrence_child_origin.reservation origin in
  let%bind artifact =
    Store.load
      artifact_store
      (Recurrence_child_reservation.artifact_revision_id reservation)
    |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
  in
  checkpoint ();
  let%bind () =
    let definition = Recurrence_child_origin.definition origin in
    if
      String.equal
        artifact.manifest_sha256
        (Recurrence_child_reservation.manifest_sha256 reservation)
      && Option.for_all
           artifact.prompt_definition_id
           ~f:(P.Id.Prompt_definition.equal definition.id)
      && Option.for_all artifact.source_receipt ~f:(fun receipt ->
        P.Id.Prompt_definition.equal
          (Agent_store.Prompt_source_receipt.definition_id receipt)
          definition.id
        && String.equal
             (Agent_store.Prompt_source_receipt.root_file receipt)
             definition.root_file)
    then Ok ()
    else
      Error
        (P.Error.invalid_request
           "installed captured child publication differs from reservation")
  in
  let%bind revision =
    Prompt_revision_builder.reparse
      ~checkpoint
      ?content_limits
      ~definition:(Recurrence_child_origin.definition origin)
      ~artifact
      ~materialized_tree:(Store.materialized_tree artifact_store artifact.revision_id)
      ()
    |> Result.map_error ~f:(fun diagnostics ->
      P.Error.create
        Prompt_unavailable
        ~message:
          (List.map
             diagnostics
             ~f:(fun (diagnostic : Prompt_revision_builder.Diagnostic.t) ->
               diagnostic.message)
           |> String.concat ~sep:"\n")
        ~retryable:false
        ())
  in
  checkpoint ();
  Ok { revision; origin }
;;
