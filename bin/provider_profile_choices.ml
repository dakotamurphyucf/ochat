open! Core
module Error = Agent_protocol.Provider_operator.Error

let load ~env ~path =
  let open Result.Let_syntax in
  let%bind contents =
    Agent_store.Durable_file.load_bounded ~env ~path ~max_bytes:1_048_576
    |> Result.map_error ~f:(fun _ -> Error.Invalid_request)
  in
  Inference_host.Compatible_profile.of_string contents
  |> Result.map_error ~f:(fun _ -> Error.Invalid_request)
;;
