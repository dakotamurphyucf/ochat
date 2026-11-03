open! Core

type t =
  { id : Agent_protocol.Id.Prompt_definition.t
  ; config_name : string
  ; root_file : string
  ; root_invocation :
      Chat_response.Script_contract_plan.Program.Role.Moderator_invocation.t
  ; tool_contract : Chatml_compilation.Tool_contract.t
  ; allowed_workspaces : Agent_protocol.Id.Workspace_definition.t list
  ; permission_profile : string
  ; runtime_policy : string option
  ; enabled : bool
  ; description : string option
  }
[@@deriving sexp]

let create
      ~id
      ~config_name
      ~root_file
      ~root_invocation
      ~allowed_workspaces
      ~permission_profile
      ~runtime_policy
      ~enabled
      ~description
  =
  if String.is_empty config_name
  then Error (Agent_store.Store_error.Corrupt "prompt config name must be nonempty")
  else if not (Filename.is_absolute root_file)
  then Error (Agent_store.Store_error.Corrupt "prompt root file must be absolute")
  else if String.is_empty permission_profile
  then
    Error (Agent_store.Store_error.Corrupt "prompt permission profile must be nonempty")
  else if
    List.contains_dup
      allowed_workspaces
      ~compare:Agent_protocol.Id.Workspace_definition.compare
  then
    Error (Agent_store.Store_error.Corrupt "prompt allowed workspaces contain duplicates")
  else
    Ok
      { id
      ; config_name
      ; root_file
      ; root_invocation
      ; tool_contract = Chatml_compilation.Tool_contract.Standard
      ; allowed_workspaces
      ; permission_profile
      ; runtime_policy
      ; enabled
      ; description
      }
;;

let allows_workspace t workspace_id =
  List.mem t.allowed_workspaces workspace_id ~equal:(fun left right ->
    Agent_protocol.Id.Workspace_definition.compare left right = 0)
;;

let root_invocation t = t.root_invocation
let with_tool_contract t tool_contract = { t with tool_contract }
let tool_contract t = t.tool_contract

module P = Agent_protocol
module D = Document_schema
module X = Persistence_codec

let capture_limits =
  D.Limits.create ~max_bytes:262144 ~max_depth:32 ~max_fields:1024 ~max_nodes:2048
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let capture_error message = Error (P.Error.invalid_request message)

let bounded json =
  D.Json.validate ~limits:capture_limits json
  |> Result.map_error ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error)))
;;

let raw_json t =
  let root_invocation =
    match t.root_invocation with
    | Ui -> "ui"
    | Headless -> "headless"
  in
  let tool_contract =
    match t.tool_contract with
    | Standard -> "standard"
    | Developer_model_spawn -> "developer_model_spawn"
  in
  `Object
    [ "id", P.Id.Prompt_definition.to_json t.id
    ; "config_name", `String t.config_name
    ; "root_file", `String t.root_file
    ; "root_invocation", `String root_invocation
    ; "tool_contract", `String tool_contract
    ; ( "allowed_workspaces"
      , `Array (List.map t.allowed_workspaces ~f:P.Id.Workspace_definition.to_json) )
    ; "permission_profile", `String t.permission_profile
    ; "runtime_policy", X.option_json X.text_json t.runtime_policy
    ; "enabled", X.bool_json t.enabled
    ; "description", X.option_json X.text_json t.description
    ]
;;

let checked_selectors values =
  List.fold_result values ~init:0 ~f:(fun count id ->
    if count >= 256
    then capture_error "too many captured workspace selectors"
    else (
      let%map.Result _ =
        P.Id.Workspace_definition.of_json (P.Id.Workspace_definition.to_json id)
      in
      count + 1))
  |> Result.map ~f:(fun _ -> ())
;;

let validate_capture t =
  let open Result.Let_syntax in
  let%bind _ = P.Id.Prompt_definition.of_json (P.Id.Prompt_definition.to_json t.id) in
  let%bind () = checked_selectors t.allowed_workspaces in
  let%bind () = bounded (raw_json t) in
  let%map _ =
    create
      ~id:t.id
      ~config_name:t.config_name
      ~root_file:t.root_file
      ~root_invocation:t.root_invocation
      ~allowed_workspaces:t.allowed_workspaces
      ~permission_profile:t.permission_profile
      ~runtime_policy:t.runtime_policy
      ~enabled:t.enabled
      ~description:t.description
    |> Result.map_error ~f:(fun error ->
      P.Error.invalid_request
        (Sexp.to_string_hum (Agent_store.Store_error.sexp_of_t error)))
  in
  ()
;;

let to_jsonaf t =
  let%map.Result () = validate_capture t in
  raw_json t
;;

let of_jsonaf json =
  let open Result.Let_syntax in
  let%bind () = bounded json in
  let%bind fields = X.object_ json in
  let%bind id = X.required fields "id" P.Id.Prompt_definition.of_json in
  let%bind config_name = X.required fields "config_name" P.Json_codec.string in
  let%bind root_file = X.required fields "root_file" P.Json_codec.string in
  let%bind root_invocation =
    X.required
      fields
      "root_invocation"
      (P.Json_codec.enum
         ~name:"captured root invocation"
         [ "ui", Chat_response.Script_contract_plan.Program.Role.Moderator_invocation.Ui
         ; "headless", Headless
         ])
  in
  let%bind tool_contract =
    X.required
      fields
      "tool_contract"
      (P.Json_codec.enum
         ~name:"captured tool contract"
         [ "standard", Chatml_compilation.Tool_contract.Standard
         ; "developer_model_spawn", Developer_model_spawn
         ])
  in
  let%bind raw_workspaces =
    X.required fields "allowed_workspaces" (function
      | `Array values -> Ok values
      | _ -> capture_error "expected workspace selector array")
  in
  let%bind allowed_workspaces =
    List.fold_result raw_workspaces ~init:(0, []) ~f:(fun (count, ids) json ->
      if count >= 256
      then capture_error "too many captured workspace selectors"
      else (
        let%map id = P.Id.Workspace_definition.of_json json in
        count + 1, id :: ids))
    |> Result.map ~f:(fun (_, ids) -> List.rev ids)
  in
  let%bind permission_profile =
    X.required fields "permission_profile" P.Json_codec.string
  in
  let%bind runtime_policy =
    X.required fields "runtime_policy" (X.nullable P.Json_codec.string)
  in
  let%bind enabled = X.required fields "enabled" P.Json_codec.bool in
  let%bind description =
    X.required fields "description" (X.nullable P.Json_codec.string)
  in
  let%bind definition =
    create
      ~id
      ~config_name
      ~root_file
      ~root_invocation
      ~allowed_workspaces
      ~permission_profile
      ~runtime_policy
      ~enabled
      ~description
    |> Result.map_error ~f:(fun error ->
      P.Error.invalid_request
        (Sexp.to_string_hum (Agent_store.Store_error.sexp_of_t error)))
  in
  Ok (with_tool_contract definition tool_contract)
;;

let shape =
  let v = D.Shape.value in
  X.shape_exn
    (List.map
       [ "id"
       ; "config_name"
       ; "root_file"
       ; "root_invocation"
       ; "tool_contract"
       ; "allowed_workspaces"
       ; "permission_profile"
       ; "runtime_policy"
       ; "enabled"
       ; "description"
       ]
       ~f:(fun name -> name, v))
;;
