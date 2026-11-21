open! Core
module P = Agent_protocol
module Payload = History_entry.Payload
module Source = Recovery_source_reader

let projection_limits =
  Document_schema.Limits.create
    ~max_bytes:(4 * 1024 * 1024)
    ~max_fields:65_536
    ~max_nodes:131_072
    ~max_depth:64
  |> Result.ok
  |> Option.value_exn
;;

let project row ~part ~max_bytes =
  let open Result.Let_syntax in
  if part < 0 || part >= 4096 || max_bytes < 1 || max_bytes > 1_048_576
  then Error Source.Limit
  else (
    let%bind () =
      Document_schema.Json.validate ~limits:projection_limits row.P.History.payload
      |> Result.map_error ~f:(function
        | Document_schema.Error.Limit_exceeded _ -> Source.Limit
        | _ -> Corrupt)
    in
    let%bind entry =
      History_codec.of_protocol row |> Result.map_error ~f:(fun _ -> Source.Corrupt)
    in
    let semantic =
      History_entry.payload entry |> Payload.semantic |> Payload.Semantic.view
    in
    let%bind parts =
      match semantic with
      | Message { content; _ } | Result { output = Content content; _ } -> Ok content
      | Result { output = Text _; _ } | Call _ | Reasoning _ | Unknown _ ->
        Error Source.Unsupported
    in
    if Stdlib.List.compare_length_with parts 4096 > 0
    then Error Source.Limit
    else (
      match List.nth parts part with
      | None -> Error Source.Unavailable
      | Some (Payload.Content.Image { uri; _ }) ->
        P.Inline_image.of_data_uri uri ~max_bytes
        |> Result.map_error ~f:(function
          | Unsupported -> Source.Unsupported
          | Size_limit -> Limit
          | Invalid_encoding | Media_mismatch -> Corrupt)
      | Some (Text _ | Refusal _ | Unknown _) -> Error Source.Unsupported))
;;

let read store ~handle ~current ~max_frame_bytes citation ~part ~max_bytes =
  Source.read
    store
    ~handle
    ~current
    ~max_frame_bytes
    citation
    ~project:(project ~part ~max_bytes)
;;
