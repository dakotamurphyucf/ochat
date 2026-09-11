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

type result_contract =
  | Native_output
  | Invocation_v1
[@@deriving sexp, equal]

type implementation =
  | Native of Ochat_function.t
  | Managed of Chatmd_shell_spec.Extension_spec.implementation

type managed_registration =
  { descriptor : Openai.Completions.tool
  ; target : Chatmd_shell_spec.Extension_spec.implementation
  ; implementation_revision : string
  ; metadata : Metadata.t
  }

type binding =
  { reference : reference
  ; implementation : implementation
  ; descriptor : Openai.Completions.tool
  ; metadata : Metadata.t
  ; permission_fingerprint : string
  ; result_contract : result_contract
  ; delegation_restriction : string option
  }

type t = binding String.Map.t

let error code message = Error { code; message }
let reference binding = binding.reference
let implementation binding = binding.implementation
let descriptor binding = binding.descriptor

let check_delegation binding =
  match binding.delegation_restriction with
  | None -> Ok ()
  | Some message -> error "delegation.native_context_unavailable" message
;;

let native_implementation binding =
  match binding.implementation with
  | Native implementation -> Some implementation
  | Managed _ -> None
;;

let metadata binding = binding.metadata
let permission_fingerprint binding = binding.permission_fingerprint
let result_contract binding = binding.result_contract
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

let bind
      ~owner
      ~resource_fingerprint
      ~implementation_revision
      ~implementation
      ~descriptor
      ~metadata
      ~result_contract
      ~delegation_restriction
  =
  let info = descriptor.Openai.Completions.function_ in
  let name = info.name in
  let open Result.Let_syntax in
  let%bind () =
    if
      String.is_empty owner
      || String.length owner > 256
      || String.is_empty name
      || String.length name > 256
      || (not (valid_digest resource_fingerprint))
      || not (valid_digest implementation_revision)
    then error "capability.invalid_registration" "invalid tool or host identity"
    else Ok ()
  in
  let%bind () =
    Schema.validate json_shape (Openai.Completions.jsonaf_of_tool descriptor)
    |> Result.map_error ~f:(fun _ ->
      { code = "capability.invalid_schema_data"
      ; message = "tool schema is not bounded valid JSON"
      })
  in
  let id = Id.create () in
  let interface = Openai.Completions.jsonaf_of_tool descriptor |> Jsonaf.to_string in
  let interface =
    match implementation, result_contract with
    | Native _, Native_output -> interface
    | Native _, Invocation_v1 ->
      [%sexp ("ochat.native-result.v1" : string), (interface : string)] |> Sexp.to_string
    | Managed target, _ ->
      [%sexp
        ("ochat.managed-tool.v1" : string)
      , (target : Chatmd_shell_spec.Extension_spec.implementation)
      , (interface : string)]
      |> Sexp.to_string
  in
  let interface =
    match delegation_restriction with
    | None -> interface
    | Some reason ->
      [%sexp
        ("ochat.delegation-restriction.v1" : string)
      , (interface : string)
      , (reason : string)]
      |> Sexp.to_string
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
  Ok
    { reference =
        { version = 1
        ; id
        ; name
        ; owner
        ; implementation_revision
        ; fingerprint
        ; input_schema = info.parameters
        }
    ; implementation
    ; descriptor
    ; metadata
    ; permission_fingerprint
    ; result_contract
    ; delegation_restriction
    }
;;

let create
      ?(metadata = [])
      ?(result_contracts = [])
      ?(delegation_restrictions = [])
      ~owner
      ~resource_fingerprint
      registrations
  =
  if
    String.is_empty owner
    || String.length owner > 256
    || not (valid_digest resource_fingerprint)
  then error "capability.invalid_registration" "invalid host identity or resource digest"
  else if
    List.length registrations > 4096
    || List.length metadata > 4096
    || List.length result_contracts > 4096
    || List.length delegation_restrictions > 4096
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
    let contracts_valid =
      Option.is_none
        (List.find_a_dup (List.map result_contracts ~f:fst) ~compare:String.compare)
      && List.for_all result_contracts ~f:(fun (name, _) ->
        List.mem names name ~equal:String.equal)
    in
    let delegation_valid =
      Option.is_none
        (List.find_a_dup
           (List.map delegation_restrictions ~f:fst)
           ~compare:String.compare)
      && List.for_all delegation_restrictions ~f:(fun (name, reason) ->
        List.mem names name ~equal:String.equal
        && (not (String.is_empty (String.strip reason)))
        && String.length reason <= 1024)
    in
    if not delegation_valid
    then
      error
        "capability.invalid_delegation_restriction"
        "duplicate, unbound or invalid delegation restriction"
    else if not contracts_valid
    then
      error
        "capability.invalid_result_contract"
        "duplicate or unbound native result contract"
    else if not metadata_valid
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
            let result_contract =
              List.Assoc.find result_contracts ~equal:String.equal name
              |> Option.value ~default:Native_output
            in
            let%map binding =
              bind
                ~owner
                ~resource_fingerprint
                ~implementation_revision
                ~implementation:(Native implementation)
                ~descriptor:implementation.info
                ~metadata
                ~result_contract
                ~delegation_restriction:
                  (List.Assoc.find delegation_restrictions ~equal:String.equal name)
            in
            Map.set registry ~key:name ~data:binding)))
;;

let extend_managed t ~owner ~resource_fingerprint registrations =
  let open Result.Let_syntax in
  let%bind () =
    if
      String.is_empty owner
      || String.length owner > 256
      || not (valid_digest resource_fingerprint)
    then error "capability.invalid_registration" "invalid managed host identity"
    else if List.length registrations + Map.length t > 4096
    then error "capability.resource_limit" "too many registered tool capabilities"
    else Ok ()
  in
  List.fold_result
    registrations
    ~init:t
    ~f:(fun registry (registration : managed_registration) ->
      let name = registration.descriptor.function_.name in
      let%bind () =
        if Map.mem registry name
        then
          error
            "capability.duplicate_name"
            "managed tool conflicts with a registered name"
        else Ok ()
      in
      let%bind () =
        let script, valid_entrypoint =
          match registration.target with
          | Chatmd_shell_spec.Extension_spec.Moderator script -> script, true
          | Standalone { script; entrypoint } -> script, String.equal entrypoint "run"
        in
        if
          String.is_empty script
          || String.length script > 256
          || (not valid_entrypoint)
          || not (String.equal registration.descriptor.type_ "function")
        then error "capability.invalid_managed_target" "invalid managed tool target"
        else Ok ()
      in
      let%bind () =
        if
          Option.is_some registration.metadata.helper
          || Result.is_error (Metadata.validate ~tool_name:name registration.metadata)
        then
          error
            "capability.invalid_metadata"
            "managed tools cannot claim native helper roles"
        else Ok ()
      in
      let%map binding =
        bind
          ~owner
          ~resource_fingerprint
          ~implementation_revision:registration.implementation_revision
          ~implementation:(Managed registration.target)
          ~descriptor:registration.descriptor
          ~metadata:registration.metadata
          ~result_contract:Invocation_v1
          ~delegation_restriction:None
      in
      Map.set registry ~key:name ~data:binding)
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
