open! Core
module P = Agent_protocol
module D = Document_schema
module F = Document_fields

let to_json (owner : P.Execution_owner.developer) =
  `Object
    [ "host_id", P.Id.Server.to_json owner.host_id
    ; "run_id", P.Id.Developer_run.to_json owner.run_id
    ; "epoch", F.decimal_json (Int64.of_int owner.epoch)
    ; "principal_id", P.Id.Principal.to_json owner.principal_id
    ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind host_id =
    F.required json "host_id" (fun json -> P.Id.Server.of_json json |> F.protocol)
  in
  let%bind run_id =
    F.required json "run_id" (fun json -> P.Id.Developer_run.of_json json |> F.protocol)
  in
  let%bind epoch =
    F.required json "epoch" (fun json ->
      let%bind value = F.decimal json in
      match Int64.to_int value with
      | Some epoch -> Ok epoch
      | None -> F.invalid "epoch" "developer epoch exceeds machine range")
  in
  let%bind principal_id =
    F.required json "principal_id" (fun json -> P.Id.Principal.of_json json |> F.protocol)
  in
  P.Execution_owner.developer ~host_id ~run_id ~epoch ~principal_id
  |> F.protocol
  |> Result.map ~f:(function
    | P.Execution_owner.Developer owner -> owner
    | Conversation _ -> assert false)
;;

let shape =
  F.shape
    [ "host_id", D.Shape.value
    ; "run_id", D.Shape.value
    ; "epoch", D.Shape.value
    ; "principal_id", D.Shape.value
    ]
;;
