open! Core
module Payload = History_entry.Payload
module Digest = Chatml_source.Digest

type row =
  { ordinal : int
  ; occurrence : History_entry.Id.t
  ; digest : Digest.t
  }

type t =
  { preparation_id : string
  ; configuration_digest : Digest.t
  ; input_count : int
  ; rows : row list
  }

let preparation_id t = t.preparation_id
let configuration_digest t = t.configuration_digest
let input_count t = t.input_count
let rows t = t.rows
let max_input_count = 2048
let digest json = Digest.of_contents (Jsonaf.to_string json) ~checkpoint:(fun () -> ())
let max_member_bytes = 64 * 1024
let max_projection_bytes = 2 * 1024 * 1024
let max_parts = 1024

let supported_part = function
  | Payload.Content.Unknown _ -> false
  | Text _ | Refusal _ | Image _ -> true
;;

let supported_parts content =
  Stdlib.List.compare_length_with content max_parts <= 0
  && List.for_all content ~f:supported_part
;;

let measure_projection entry ~remaining_bytes =
  let semantic = Payload.semantic (History_entry.payload entry) in
  let supported =
    match Payload.Semantic.view semantic with
    | Message { content; _ } | Result { output = Content content; _ } ->
      supported_parts content
    | Reasoning { readable_summary } ->
      Stdlib.List.compare_length_with readable_summary max_parts <= 0
    | Call _ | Result { output = Text _; _ } -> true
    | Unknown _ -> false
  in
  if (not supported) || remaining_bytes <= 0
  then None
  else
    let open Option.Let_syntax in
    let%bind limits =
      Document_schema.Limits.create
        ~max_bytes:(Int.min max_member_bytes remaining_bytes)
        ~max_depth:24
        ~max_fields:2048
        ~max_nodes:4096
      |> Result.ok
    in
    let json = Payload.to_json (Payload.authored semantic) in
    let%map size = Document_schema.Json.validate_and_measure json ~limits |> Result.ok in
    json, size
;;

let projected_digest entry ~remaining_bytes =
  match measure_projection entry ~remaining_bytes with
  | None -> None, Int.max 0 (remaining_bytes - max_member_bytes)
  | Some (json, bytes) -> Some (digest json), remaining_bytes - bytes
;;

let capture request ~configuration ~omit =
  let history = Inference.Request.history request in
  if Stdlib.List.compare_length_with history max_input_count > 0
  then None
  else (
    let rows_rev, _ =
      List.foldi
        history
        ~init:([], max_projection_bytes)
        ~f:(fun ordinal (rows, remaining_bytes) entry ->
          let occurrence = History_entry.id entry in
          if omit occurrence
          then rows, remaining_bytes
          else (
            let digest, remaining_bytes = projected_digest entry ~remaining_bytes in
            Option.value_map digest ~default:(rows, remaining_bytes) ~f:(fun digest ->
              { ordinal; occurrence; digest } :: rows, remaining_bytes)))
    in
    let module Configuration = Inference.Observation.Configuration in
    Some
      { preparation_id = Configuration.preparation_id configuration
      ; configuration_digest = digest (Configuration.to_json configuration)
      ; input_count = List.length history
      ; rows = List.rev rows_rev
      })
;;
