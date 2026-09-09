open Core
module Id = Agent_protocol.Id.Capability
module Schema = Chatmd_shell_spec.Tool_schema
module Metadata = Chatmd_shell_spec.Authoring_metadata

let digest = Chatmd_shell_spec.Source_ref.digest

type reference =
  { version : int
  ; id : Id.t
  ; name : string
  ; owner : string
  ; implementation_revision : string
  ; fingerprint : string
  ; input_schema : Jsonaf.t
  }

type error =
  { code : string
  ; message : string
  }
[@@deriving sexp]

type binding =
  { reference : reference
  ; implementation : Ochat_function.t
  ; metadata : Metadata.t
  ; permission_fingerprint : string
  }

type t = binding String.Map.t

let error code message = Error { code; message }
let reference binding = binding.reference
let implementation binding = binding.implementation
let metadata binding = binding.metadata
let permission_fingerprint binding = binding.permission_fingerprint
let references t = Map.data t |> List.map ~f:reference

let valid_digest value =
  String.length value = 64
  && String.for_all value ~f:(function
    | '0' .. '9' | 'a' .. 'f' -> true
    | _ -> false)
;;

let json_shape =
  match Schema.compile `True with
  | Ok value -> value
  | Error _ -> assert false
;;

let create ?(metadata = []) ~owner ~resource_fingerprint registrations =
  if
    String.is_empty owner
    || String.length owner > 256
    || not (valid_digest resource_fingerprint)
  then error "capability.invalid_registration" "invalid host identity or resource digest"
  else if List.length registrations > 4096 || List.length metadata > 4096
  then error "capability.resource_limit" "too many registered tool capabilities"
  else (
    let names =
      List.map registrations ~f:(fun (_, implementation) ->
        implementation.Ochat_function.info.function_.name)
    in
    let metadata_valid =
      Option.is_none (List.find_a_dup (List.map metadata ~f:fst) ~compare:String.compare)
      && List.for_all metadata ~f:(fun (name, data) ->
        List.mem names name ~equal:String.equal
        && Result.is_ok (Metadata.validate ~tool_name:name data))
    in
    if not metadata_valid
    then error "capability.invalid_metadata" "invalid or unbound authoring metadata"
    else (
      match List.find_a_dup names ~compare:String.compare with
      | Some _ -> error "capability.duplicate_name" "registered tool names must be unique"
      | None ->
        List.fold
          registrations
          ~init:(Ok String.Map.empty)
          ~f:(fun result (implementation_revision, implementation) ->
            let open Result.Let_syntax in
            let%bind registry = result in
            let info = implementation.Ochat_function.info.function_ in
            let name = info.name in
            let metadata =
              List.Assoc.find metadata ~equal:String.equal name
              |> Option.value ~default:Metadata.empty
            in
            if
              String.is_empty name
              || String.length name > 256
              || not (valid_digest implementation_revision)
            then
              error
                "capability.invalid_registration"
                "invalid tool name or implementation digest"
            else (
              match
                Schema.validate
                  json_shape
                  (Openai.Completions.jsonaf_of_tool implementation.info)
              with
              | Error _ ->
                error
                  "capability.invalid_schema_data"
                  "tool schema is not bounded valid JSON"
              | Ok () ->
                let id = Id.create () in
                let interface =
                  Openai.Completions.jsonaf_of_tool implementation.info
                  |> Jsonaf.to_string
                in
                let fingerprint =
                  [%sexp
                    ("ochat.capability.v2" : string)
                  , (metadata : Metadata.t)
                  , (Id.to_string id : string)
                  , (owner : string)
                  , (name : string)
                  , (implementation_revision : string)
                  , (resource_fingerprint : string)
                  , (interface : string)]
                  |> Sexp.to_string
                  |> digest
                in
                let permission_fingerprint =
                  [%sexp
                    ("ochat.capability-permission.v1" : string)
                  , (metadata : Metadata.t)
                  , (owner : string)
                  , (name : string)
                  , (implementation_revision : string)
                  , (resource_fingerprint : string)
                  , (interface : string)]
                  |> Sexp.to_string
                  |> digest
                in
                let reference =
                  { version = 1
                  ; id
                  ; name
                  ; owner
                  ; implementation_revision
                  ; fingerprint
                  ; input_schema = info.parameters
                  }
                in
                Ok
                  (Map.set
                     registry
                     ~key:name
                     ~data:{ reference; implementation; metadata; permission_fingerprint })))))
;;

let find t ~name =
  match Map.find t name with
  | Some binding -> Ok binding
  | None -> error "capability.not_selected" "tool is not in the selected capability set"
;;

let select t ~names =
  if List.length names > 4096
  then error "capability.resource_limit" "too many selected tools"
  else (
    match List.find_a_dup names ~compare:String.compare with
    | Some _ ->
      error "capability.duplicate_selection" "selected tool names must be unique"
    | None ->
      List.fold names ~init:(Ok String.Map.empty) ~f:(fun result name ->
        let open Result.Let_syntax in
        let%bind selected = result in
        let%map binding = find t ~name in
        Map.set selected ~key:name ~data:binding))
;;

let resolve t ~id ~fingerprint =
  match
    List.find (Map.data t) ~f:(fun binding -> Id.compare binding.reference.id id = 0)
  with
  | Some binding when String.equal binding.reference.fingerprint fingerprint -> Ok binding
  | _ ->
    error
      "capability.stale_reference"
      "capability reference is stale, foreign or not selected"
;;

let fingerprint t =
  references t
  |> List.map ~f:(fun reference -> reference.fingerprint)
  |> [%sexp_of: string list]
  |> Sexp.to_string
  |> digest
;;
