open! Core
module P = Agent_protocol
module Contract = Chatml.Chatml_compiler_contract
module Portable_path = Source.Portable_path

module Target = struct
  type t =
    | One_off_v1
    | Developer_tool_v1
    | Tool_v1 of
        { tool_name : string
        ; input_schema_json : string
        ; output_schema_json : string
        }
  [@@deriving compare, equal, sexp]
end

module Source = struct
  type document_origin =
    | Supplied_document
    | Host_document of
        { root : string
        ; member : string
        }
  [@@deriving compare, equal, sexp]

  type authoring_project =
    | Host_project of
        { root : string
        ; project : string
        ; request : Authoring_project_access_configuration.Selection.request
        }
    | Uploaded_project of { environment : string }
  [@@deriving compare, equal, sexp]

  type t =
    | Local_project of
        { root : string
        ; project : string
        ; request : Authoring_project_access_configuration.Selection.request
        }
    | Local_source of
        { root : string
        ; member : string
        }
    | Authoring_project of authoring_project
    | Document_project of
        { origin : document_origin
        ; script_id : string
        ; authored_project : Chatmd_shell_spec.Script_project.t
        }
    | Document_source of
        { origin : document_origin
        ; script_id : string
        }
  [@@deriving compare, equal, sexp]
end

module Principal_selector = struct
  type t =
    { principal_id : P.Id.Principal.t
    ; authentication_kind : string
    }
  [@@deriving compare, equal, sexp]
end

module Selection = struct
  type t =
    { source : Source.t
    ; target : Target.t
    ; principals : Principal_selector.t list
    }
  [@@deriving compare, equal, sexp]
end

module Capacity = struct
  type t =
    { max_runs : int
    ; max_active_commands : int
    ; max_active_agent_calls : int
    ; max_agent_sources : int
    ; max_native_bindings : int
    ; max_retained_tasks : int
    ; max_watches_per_connection : int
    ; max_pending_approvals : int
    ; max_repl_instances : int
    ; max_repl_keys_per_instance : int
    ; max_repl_submissions_per_epoch : int
    ; max_repl_source_bytes : int
    }
  [@@deriving compare, equal, sexp]

  let standard_v1 =
    { max_runs = 128
    ; max_active_commands = 4
    ; max_active_agent_calls = 8
    ; max_agent_sources = 6
    ; max_native_bindings = 32
    ; max_retained_tasks = 8
    ; max_watches_per_connection = 4
    ; max_pending_approvals = 4
    ; max_repl_instances = 8
    ; max_repl_keys_per_instance = 128
    ; max_repl_submissions_per_epoch = 64
    ; max_repl_source_bytes = 65_536
    }
  ;;

  let validate capacity =
    let positive value = value > 0 in
    if
      positive capacity.max_runs
      && capacity.max_runs <= 4096
      && positive capacity.max_active_commands
      && capacity.max_active_agent_calls >= 1
      && capacity.max_active_agent_calls <= 64
      && positive capacity.max_agent_sources
      && positive capacity.max_native_bindings
      && positive capacity.max_retained_tasks
      && positive capacity.max_watches_per_connection
      && positive capacity.max_pending_approvals
      && positive capacity.max_repl_instances
      && positive capacity.max_repl_keys_per_instance
      && positive capacity.max_repl_submissions_per_epoch
      && positive capacity.max_repl_source_bytes
    then Ok ()
    else Error (P.Error.invalid_request "invalid developer capacity")
  ;;
end

type model =
  | Disabled
  | Provider_default
[@@deriving compare, equal, sexp]

type approvals =
  | Disabled
  | Once
[@@deriving compare, equal, sexp]

type repl =
  | Disabled
  | Standard_v1
[@@deriving compare, equal, sexp]

type traces =
  | No_traces
  | Trace_summary
  | Trace_full
[@@deriving compare, equal, sexp]

type t =
  { selections : Selection.t list
  ; capacity : Capacity.t
  ; model : model
  ; approvals : approvals
  ; repl : repl
  ; portable : bool
  ; traces : traces [@sexp.default No_traces]
  }
[@@deriving compare, equal, sexp]

let invalid message = P.Error.invalid_request message

let check_text ~name ~max_bytes value =
  if String.is_empty value || String.length value > max_bytes
  then Error (invalid ("invalid developer " ^ name))
  else Ok ()
;;

let check_path name value =
  Portable_path.create value
  |> Result.map ~f:(fun _ -> ())
  |> Result.map_error ~f:(fun _ -> invalid ("invalid developer " ^ name))
;;

let check_project_request request =
  let target, environment =
    match request with
    | Authoring_project_access_configuration.Selection.Library { environment } ->
      P.Authoring.Context.Selection.Library, environment
    | Entry { name; environment } -> P.Authoring.Context.Selection.Entry name, environment
  in
  P.Authoring.Context.Selection.create ~target ~environment
  |> Result.map ~f:(fun _ -> ())
  |> Result.map_error ~f:(fun _ -> invalid "invalid developer project selection")
;;

let check_origin = function
  | Source.Supplied_document -> Ok ()
  | Host_document { root; member } ->
    let open Result.Let_syntax in
    let%bind () = check_text ~name:"host root" ~max_bytes:4096 root in
    check_path "host member" member
;;

let check_source = function
  | Source.Local_project { root; project; request } ->
    let open Result.Let_syntax in
    let%bind () = check_text ~name:"local root" ~max_bytes:4096 root in
    let%bind () = check_path "local project" project in
    check_project_request request
  | Local_source { root; member } ->
    let open Result.Let_syntax in
    let%bind () = check_text ~name:"local root" ~max_bytes:4096 root in
    check_path "local member" member
  | Authoring_project (Host_project { root; project; request }) ->
    let open Result.Let_syntax in
    let%bind () = check_text ~name:"host root" ~max_bytes:4096 root in
    let%bind () = check_path "host project" project in
    check_project_request request
  | Authoring_project (Uploaded_project { environment }) ->
    check_text ~name:"uploaded environment" ~max_bytes:256 environment
  | Document_project { origin; script_id; authored_project } ->
    let open Result.Let_syntax in
    let%bind () = check_origin origin in
    let%bind () = check_text ~name:"script ID" ~max_bytes:256 script_id in
    Chatmd_shell_spec.Script_project.create
      ~project:authored_project.project
      ~environment:authored_project.environment
    |> Result.map ~f:(fun _ -> ())
    |> Result.map_error ~f:(fun _ -> invalid "invalid developer authored project")
  | Document_source { origin; script_id } ->
    let open Result.Let_syntax in
    let%bind () = check_origin origin in
    check_text ~name:"script ID" ~max_bytes:256 script_id
;;

let check_principal (principal : Principal_selector.t) =
  let open Result.Let_syntax in
  let%bind id =
    P.Id.Principal.of_string (P.Id.Principal.to_string principal.principal_id)
    |> Result.map_error ~f:(fun _ -> invalid "invalid developer principal ID")
  in
  P.Principal.create
    ~id
    ~authentication_kind:principal.authentication_kind
    ~scopes:P.Scope.Set.empty
    ~attributes:[]
  |> Result.map ~f:(fun _ -> ())
  |> Result.map_error ~f:(fun _ -> invalid "invalid developer principal selector")
;;

let check_schema text =
  if String.length text > 1_048_576
  then Error (invalid "developer Tool schema byte limit")
  else
    Chatmd_shell_spec.Tool_schema.of_string text
    |> Result.map ~f:(fun _ -> ())
    |> Result.map_error ~f:(fun _ -> invalid "invalid developer Tool schema")
;;

let check_target = function
  | Target.One_off_v1 | Developer_tool_v1 -> Ok ()
  | Tool_v1 { tool_name; input_schema_json; output_schema_json } ->
    let open Result.Let_syntax in
    let%bind () = check_text ~name:"Tool name" ~max_bytes:256 tool_name in
    let%bind () = check_schema input_schema_json in
    check_schema output_schema_json
;;

let request_text_fields = function
  | Authoring_project_access_configuration.Selection.Library { environment } ->
    Option.to_list environment
  | Entry { name; environment } -> name :: Option.to_list environment
;;

let origin_text_fields = function
  | Source.Supplied_document -> []
  | Host_document { root; member } -> [ root; member ]
;;

let source_text_fields = function
  | Source.Local_project { root; project; request } ->
    root :: project :: request_text_fields request
  | Local_source { root; member } -> [ root; member ]
  | Authoring_project (Host_project { root; project; request }) ->
    root :: project :: request_text_fields request
  | Authoring_project (Uploaded_project { environment }) -> [ environment ]
  | Document_project { origin; script_id; authored_project } ->
    origin_text_fields origin
    @ (script_id
       :: authored_project.project
       :: Option.to_list authored_project.environment)
  | Document_source { origin; script_id } -> script_id :: origin_text_fields origin
;;

let target_text_fields = function
  | Target.One_off_v1 | Developer_tool_v1 -> []
  | Tool_v1 { tool_name; _ } -> [ tool_name ]
;;

let charge_text remaining fields =
  List.fold_result fields ~init:remaining ~f:(fun remaining field ->
    let bytes = String.length field in
    if bytes > remaining
    then Error (invalid "developer selection text limit")
    else Ok (remaining - bytes))
;;

let schema_text_bytes = function
  | Target.One_off_v1 | Developer_tool_v1 -> 0, 0
  | Tool_v1 { input_schema_json; output_schema_json; _ } ->
    String.length input_schema_json, String.length output_schema_json
;;

let validate t =
  let open Result.Let_syntax in
  let%bind () = Capacity.validate t.capacity in
  let rec rows count principal_count selector_bytes schema_bytes seen = function
    | [] -> Ok ()
    | (selection : Selection.t) :: rest ->
      if count >= 64
      then Error (invalid "developer selection row limit")
      else if List.is_empty selection.principals
      then Error (invalid "developer selection requires a principal")
      else (
        let input_bytes, output_bytes = schema_text_bytes selection.target in
        if
          input_bytes > 1_048_576
          || output_bytes > 1_048_576
          || schema_bytes > 8_388_608 - input_bytes - output_bytes
        then Error (invalid "developer aggregate Tool schema limit")
        else (
          let%bind selector_remaining =
            charge_text
              (65_536 - selector_bytes)
              (source_text_fields selection.source @ target_text_fields selection.target)
          in
          let%bind () = check_source selection.source in
          let%bind () = check_target selection.target in
          let rec principals principal_count selector_remaining seen = function
            | [] ->
              rows
                (count + 1)
                principal_count
                (65_536 - selector_remaining)
                (schema_bytes + input_bytes + output_bytes)
                seen
                rest
            | (principal : Principal_selector.t) :: remaining ->
              if principal_count >= 256
              then Error (invalid "developer principal selection limit")
              else (
                let%bind selector_remaining =
                  charge_text
                    selector_remaining
                    [ P.Id.Principal.to_string principal.principal_id
                    ; principal.authentication_kind
                    ]
                in
                let%bind () = check_principal principal in
                let key = selection.source, principal in
                if
                  List.mem
                    seen
                    key
                    ~equal:
                      (fun
                        (left_source, left_principal) (right_source, right_principal) ->
                      Source.equal left_source right_source
                      && Principal_selector.equal left_principal right_principal)
                then Error (invalid "duplicate developer source/principal selection")
                else
                  principals
                    (principal_count + 1)
                    selector_remaining
                    (key :: seen)
                    remaining)
          in
          principals principal_count selector_remaining seen selection.principals))
  in
  rows 0 0 0 0 [] t.selections
;;

let standard_targets t =
  let open Result.Let_syntax in
  let%map () = validate t in
  List.fold t.selections ~init:[] ~f:(fun targets selection ->
    let target =
      match selection.Selection.target with
      | Target.One_off_v1 -> Contract.Target.One_off_v1
      | Developer_tool_v1 -> Developer_tool_v1
      | Tool_v1 _ -> Tool_v1
    in
    if List.mem targets target ~equal:Contract.Target.equal
    then targets
    else targets @ [ target ])
;;
