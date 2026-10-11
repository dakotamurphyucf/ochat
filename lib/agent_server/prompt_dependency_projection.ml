open! Core
module P = Agent_protocol
module DTO = P.Prompt_dependency_selection
module Host = Prompt_catalog_host
module History = Prompt_catalog_publication.History
module Catalog = Agent_session.Prompt_catalog

let invalid message = Error (P.Error.create Invalid_state ~message ~retryable:false ())

let generation value =
  match DTO.Generation.of_int value with
  | Ok _ as result -> result
  | Error _ -> invalid "prepared dependency history has an invalid generation"
;;

let status host ~publication =
  let open Result.Let_syntax in
  let%map history =
    match Host.history host with
    | History.Tracked value ->
      generation value |> Result.map ~f:(fun value -> DTO.Status.History.Tracked value)
    | Missing -> Ok DTO.Status.History.Missing
    | Invalid -> Ok DTO.Status.History.Invalid
  in
  let publication =
    match publication with
    | Config_watcher.State.Available -> DTO.Status.Publication.Available
    | Busy -> Busy
    | Uncertain -> Uncertain
    | Closed -> Closed
  in
  DTO.Status.{ history; publication }
;;

let tracked = function
  | History.Tracked value -> generation value
  | Missing | Invalid -> invalid "dependency update requires tracked prepared history"
;;

let prepared_update prepared ~request ~checkpoint =
  let open Result.Let_syntax in
  let%bind previous_generation = tracked (Host.Prepared.previous_history prepared) in
  let%bind generation = tracked (Host.Prepared.history prepared) in
  let%bind entries =
    List.fold_result
      (Host.Prepared.entries prepared)
      ~init:String.Map.empty
      ~f:(fun entries (entry : Catalog.entry) ->
        checkpoint ();
        match
          Map.add
            entries
            ~key:(P.Id.Prompt_definition.to_string entry.definition.id)
            ~data:entry
        with
        | `Ok entries -> Ok entries
        | `Duplicate -> invalid "prepared catalog has duplicate dependency targets")
  in
  let%bind targets =
    List.map (DTO.Update_request.definitions request) ~f:(fun definition_id ->
      checkpoint ();
      let%bind entry =
        match Map.find entries (P.Id.Prompt_definition.to_string definition_id) with
        | Some entry -> Ok entry
        | None -> invalid "prepared catalog is missing a requested dependency target"
      in
      let%map outcome =
        match entry.Catalog.availability with
        | Ready revision ->
          Ok (DTO.Update_result.Target.Ready (Agent_session.Prompt_revision.id revision))
        | Unavailable _ ->
          Ok
            (DTO.Update_result.Target.Unavailable
               { reason = "prompt source is unavailable; inspect prompt diagnostics" })
        | Disabled -> invalid "prepared dependency update target is disabled"
      in
      DTO.Update_result.Target.{ definition_id; outcome })
    |> Result.all
  in
  let%bind result = DTO.Update_result.create ~previous_generation ~generation ~targets in
  if DTO.Update_result.matches_request result request
  then Ok result
  else invalid "prepared dependency history differs from the admitted request"
;;
