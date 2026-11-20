open! Core
module Capacity = Inference_runtime.Context_capacity
module Source = Capacity.Source
module Observation = Inference.Observation
module Json = Document_schema.Json

let encoder = Capacity.Encoder.Openai_responses_v1

let estimator =
  match
    Observation.Estimator.create ~method_:O200k_serialized_prepared_request ~version:1
  with
  | Ok estimator -> estimator
  | Error _ -> assert false
;;

let token_codec =
  Domain.DLS.new_key (fun () -> Tikitoken.create_codec Tiktoken_data.o200k_base)
;;

let counter_limits =
  let module L = Document_schema.Limits in
  let default = L.default in
  match
    L.create
      ~max_bytes:(1024 * 1024)
      ~max_depth:(L.max_depth default)
      ~max_fields:(L.max_fields default)
      ~max_nodes:(L.max_nodes default)
  with
  | Ok limits -> limits
  | Error _ -> assert false
;;

let only_fields json known =
  match json with
  | `Object fields ->
    if List.for_all fields ~f:(fun (name, _) -> List.mem known name ~equal:String.equal)
    then Ok ()
    else Error Source.Unknown_wire_shape
  | _ -> Error Source.Unknown_wire_shape
;;

let empty_field json name =
  match Json.field json ~name with
  | Absent | Null | Value (`Array []) -> true
  | Value _ -> false
;;

let content json =
  match Json.field json ~name:"type" with
  | Value (`String "input_text") -> only_fields json [ "type"; "text" ]
  | Value (`String "output_text") ->
    if empty_field json "annotations" && empty_field json "logprobs"
    then only_fields json [ "type"; "text"; "annotations"; "logprobs" ]
    else Error Source.Unknown_wire_shape
  | Value (`String "refusal") -> only_fields json [ "type"; "refusal" ]
  | Value (`String "input_image") -> Error Image
  | Value (`String "input_file") -> Error Document
  | _ -> Error Unknown_wire_shape
;;

let text_or_contents json ~name =
  match Json.field json ~name with
  | Value (`String _) -> Ok ()
  | Value (`Array parts) ->
    List.fold_result parts ~init:() ~f:(fun () part -> content part)
  | _ -> Error Source.Unknown_wire_shape
;;

let cover_item json =
  let open Result.Let_syntax in
  let metadata = [ "type"; "id"; "status" ] in
  let call_metadata = "call_id" :: "namespace" :: metadata in
  match Json.field json ~name:"type" with
  | Absent | Value (`String "message") ->
    let%bind () = only_fields json ([ "role"; "content"; "phase" ] @ metadata) in
    text_or_contents json ~name:"content"
  | Value (`String "function_call") ->
    only_fields json ([ "name"; "async"; "arguments" ] @ call_metadata)
  | Value (`String "custom_tool_call") ->
    only_fields json ([ "name"; "async"; "input" ] @ call_metadata)
  | Value (`String ("function_call_output" | "custom_tool_call_output")) ->
    let%bind () = only_fields json ([ "name"; "output" ] @ call_metadata) in
    text_or_contents json ~name:"output"
  | Value (`String "reasoning") -> Error Source.Opaque_input
  | _ -> Error Source.Unknown_wire_shape
;;

let read request =
  let json = Responses_request.jsonaf_of_t request in
  match Json.validate_and_measure ~limits:counter_limits json with
  | Error (Limit_exceeded _) -> Source.Incomplete Counter_limit
  | Error _ -> Incomplete Unknown_wire_shape
  | Ok _ ->
    (match
       List.fold_result (Responses_request.input request) ~init:() ~f:(fun () ->
         cover_item)
     with
     | Error reason -> Incomplete reason
     | Ok () ->
       (try
          let tokens =
            Jsonaf.to_string json
            |> (fun text -> Tikitoken.encode ~codec:(Domain.DLS.get token_codec) ~text)
            |> List.length
            |> Int64.of_int
          in
          match Observation.Count.create (Estimated { tokens; estimator }) with
          | Ok count -> Covered count
          | Error _ -> Incomplete Counter_unavailable
        with
        | Pcre.Error _ -> Incomplete Counter_unavailable))
;;

let source request = Source.create ~encoder ~read:(fun () -> read request)
