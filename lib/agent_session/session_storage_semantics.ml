open! Core
module D = Document_schema
module Q = Queue_provenance_semantics

let name = Agent_protocol.Session_storage_semantics.recurrence_run_origin
let supported = Agent_protocol.Session_storage_semantics.supported_children

let index_requires index =
  match D.Json.field index ~name:"recurrence_origins" with
  | Value _ -> true
  | Absent | Null -> false
;;

let state_requires payload =
  match D.Json.field payload ~name:"run_state" with
  | Value index -> index_requires index
  | Absent | Null -> false
;;

let delta_requires payload =
  match D.Json.field payload ~name:"changes" with
  | Value (`Array changes) ->
    List.exists changes ~f:(fun change ->
      match D.Json.field change ~name:"kind" with
      | Value (`String "run_state_changed") ->
        (match D.Json.field change ~name:"value" with
         | Value index -> index_requires index
         | Absent | Null -> false)
      | Value (`String "created") ->
        (match D.Json.field change ~name:"state" with
         | Value state ->
           (match D.Json.field state ~name:"payload" with
            | Value payload -> state_requires payload
            | Absent | Null -> false)
         | Absent | Null -> false)
      | Value _ | Absent | Null -> false)
  | Value _ | Absent | Null -> false
;;

let check document ~name ~required =
  if
    (not required)
    || List.mem (D.Document.required_semantics document) name ~equal:String.equal
  then Ok ()
  else Error (D.Error.Required_semantics_unknown ("missing " ^ name))
;;

let require document ~name ~required ~limits =
  if
    (not required)
    || List.mem (D.Document.required_semantics document) name ~equal:String.equal
  then Ok document
  else (
    match D.Document.json document with
    | `Object fields ->
      let markers = D.Document.required_semantics document @ [ name ] in
      let replacement = `Array (List.map markers ~f:(fun value -> `String value)) in
      let fields =
        if List.Assoc.mem fields "required_semantics" ~equal:String.equal
        then
          List.map fields ~f:(fun (key, value) ->
            key, if String.equal key "required_semantics" then replacement else value)
        else fields @ [ "required_semantics", replacement ]
      in
      D.Document.inspect ~limits (`Object fields)
    | _ -> assert false)
;;

let native_invocation_requires invocation =
  match D.Json.field invocation ~name:"native_goal_resume" with
  | Absent -> false
  | Null | Value _ -> true
;;

let native_state_requires payload =
  match D.Json.field payload ~name:"invocations" with
  | Value (`Array invocations) -> List.exists invocations ~f:native_invocation_requires
  | Absent | Null | Value _ -> false
;;

let native_delta_requires payload =
  match D.Json.field payload ~name:"changes" with
  | Value (`Array changes) ->
    List.exists changes ~f:(fun change ->
      match D.Json.field change ~name:"kind" with
      | Value (`String ("invocation_changed" | "invocation_reconciled")) ->
        (match D.Json.field change ~name:"value" with
         | Value value -> native_invocation_requires value
         | _ -> false)
      | Value (`String "created") ->
        (match D.Json.field change ~name:"state" with
         | Value state ->
           (match D.Json.field state ~name:"payload" with
            | Value state -> native_state_requires state
            | _ -> false)
         | _ -> false)
      | _ -> false)
  | Absent | Null | Value _ -> false
;;

let native_name = Agent_protocol.Goal_native_resume_intent.persistence_semantics

let usage_state_requires payload =
  List.exists [ "delegated_usage"; "child_contributions" ] ~f:(fun name ->
    match D.Json.field payload ~name with
    | Value _ -> true
    | Absent | Null -> false)
;;

let usage_delta_requires payload =
  match D.Json.field payload ~name:"changes" with
  | Value (`Array changes) ->
    List.exists changes ~f:(fun change ->
      match D.Json.field change ~name:"kind" with
      | Value
          (`String ("delegated_inference_ledger_changed" | "child_contributions_changed"))
        -> true
      | Value (`String "created") ->
        (match D.Json.field change ~name:"state" with
         | Value state ->
           (match D.Json.field state ~name:"payload" with
            | Value payload -> usage_state_requires payload
            | Absent | Null -> false)
         | Absent | Null -> false)
      | Value _ | Absent | Null -> false)
  | Value _ | Absent | Null -> false
;;

let require_state document ~limits =
  let open Result.Let_syntax in
  let%bind document =
    Q.require document ~limits ~required:(Q.state_requires (D.Document.payload document))
  in
  let%bind document =
    require
      document
      ~name
      ~limits
      ~required:(state_requires (D.Document.payload document))
  in
  let%bind document =
    require
      document
      ~name:native_name
      ~limits
      ~required:(native_state_requires (D.Document.payload document))
  in
  require
    document
    ~name:Agent_protocol.Session_storage_semantics.delegated_usage
    ~limits
    ~required:(usage_state_requires (D.Document.payload document))
;;

let check_state document =
  let open Result.Let_syntax in
  let%bind () =
    Q.check document ~required:(Q.state_requires (D.Document.payload document))
  in
  let%bind () =
    check document ~name ~required:(state_requires (D.Document.payload document))
  in
  let%bind () =
    check
      document
      ~name:native_name
      ~required:(native_state_requires (D.Document.payload document))
  in
  check
    document
    ~name:Agent_protocol.Session_storage_semantics.delegated_usage
    ~required:(usage_state_requires (D.Document.payload document))
;;

let require_delta document ~limits =
  let open Result.Let_syntax in
  let%bind document =
    Q.require document ~limits ~required:(Q.delta_requires (D.Document.payload document))
  in
  let%bind document =
    require
      document
      ~name
      ~limits
      ~required:(delta_requires (D.Document.payload document))
  in
  let%bind document =
    require
      document
      ~name:native_name
      ~limits
      ~required:(native_delta_requires (D.Document.payload document))
  in
  require
    document
    ~name:Agent_protocol.Session_storage_semantics.delegated_usage
    ~limits
    ~required:(usage_delta_requires (D.Document.payload document))
;;

let check_delta document =
  let open Result.Let_syntax in
  let%bind () =
    Q.check document ~required:(Q.delta_requires (D.Document.payload document))
  in
  let%bind () =
    check document ~name ~required:(delta_requires (D.Document.payload document))
  in
  let%bind () =
    check
      document
      ~name:native_name
      ~required:(native_delta_requires (D.Document.payload document))
  in
  check
    document
    ~name:Agent_protocol.Session_storage_semantics.delegated_usage
    ~required:(usage_delta_requires (D.Document.payload document))
;;
