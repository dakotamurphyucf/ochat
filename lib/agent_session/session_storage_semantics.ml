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

let native_invocation_requires invocation ~kind =
  match D.Json.field invocation ~name:"native_goal_action" with
  | Value action ->
    (match D.Json.field action ~name:"kind" with
     | Value (`String actual) -> String.equal actual kind
     | Absent | Null | Value _ -> false)
  | Absent | Null -> false
;;

let native_state_requires payload ~kind =
  match D.Json.field payload ~name:"invocations" with
  | Value (`Array invocations) ->
    List.exists invocations ~f:(native_invocation_requires ~kind)
  | Absent | Null | Value _ -> false
;;

let native_delta_requires payload ~kind =
  match D.Json.field payload ~name:"changes" with
  | Value (`Array changes) ->
    List.exists changes ~f:(fun change ->
      match D.Json.field change ~name:"kind" with
      | Value (`String ("invocation_changed" | "invocation_reconciled")) ->
        (match D.Json.field change ~name:"value" with
         | Value value -> native_invocation_requires value ~kind
         | _ -> false)
      | Value (`String "created") ->
        (match D.Json.field change ~name:"state" with
         | Value state ->
           (match D.Json.field state ~name:"payload" with
            | Value state -> native_state_requires state ~kind
            | _ -> false)
         | _ -> false)
      | _ -> false)
  | Absent | Null | Value _ -> false
;;

let native_branches =
  [ "resume", Agent_protocol.Goal_native_resume_intent.persistence_semantics
  ; "start", Agent_protocol.Goal_native_start_intent.persistence_semantics
  ]
;;

let check_native_state_fields payload =
  match D.Json.field payload ~name:"invocations" with
  | Value (`Array invocations) ->
    List.fold_result invocations ~init:() ~f:(fun () invocation ->
      Agent_protocol.Native_goal_action_conversion.reject_old_field invocation)
  | Absent | Null | Value _ -> Ok ()
;;

let check_native_delta_fields payload =
  match D.Json.field payload ~name:"changes" with
  | Value (`Array changes) ->
    List.fold_result changes ~init:() ~f:(fun () change ->
      match D.Json.field change ~name:"kind" with
      | Value (`String ("invocation_changed" | "invocation_reconciled")) ->
        (match D.Json.field change ~name:"value" with
         | Value invocation ->
           Agent_protocol.Native_goal_action_conversion.reject_old_field invocation
         | Absent | Null -> Ok ())
      | Absent | Null | Value _ -> Ok ())
  | Absent | Null | Value _ -> Ok ()
;;

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
          (`String
             ( "delegated_inference_ledger_changed"
             | "child_contributions_changed"
             | "goal_descendants_changed" )) -> true
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

(* Inspect raw fields before decoding: old readers must not silently discard
   scoped attribution, even when it is nested in a replacement child slot. *)
let populated value name =
  match D.Json.field value ~name with
  | Value _ -> true
  | Absent | Null -> false
;;

let descendant_children_require document =
  match D.Json.field document ~name:"payload" with
  | Value payload ->
    (match D.Json.field payload ~name:"children" with
     | Value (`Array children) ->
       List.exists children ~f:(fun child ->
         populated child "goal_origin"
         ||
         match D.Json.field child ~name:"snapshot" with
         | Value snapshot -> populated snapshot "goal_scopes"
         | Absent | Null -> false)
     | Value _ | Absent | Null -> false)
  | Absent | Null -> false
;;

let descendant_state_requires payload =
  populated payload "goal_descendants"
  ||
  match D.Json.field payload ~name:"child_contributions" with
  | Value children -> descendant_children_require children
  | Absent | Null -> false
;;

let descendant_delta_requires payload =
  match D.Json.field payload ~name:"changes" with
  | Value (`Array changes) ->
    List.exists changes ~f:(fun change ->
      match D.Json.field change ~name:"kind" with
      | Value (`String "goal_descendants_changed") -> true
      | Value (`String "child_contributions_changed") ->
        (match D.Json.field change ~name:"value" with
         | Value children -> descendant_children_require children
         | Absent | Null -> false)
      | Value (`String "created") ->
        (match D.Json.field change ~name:"state" with
         | Value state ->
           (match D.Json.field state ~name:"payload" with
            | Value payload -> descendant_state_requires payload
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
    List.fold_result native_branches ~init:document ~f:(fun document (kind, name) ->
      require
        document
        ~name
        ~limits
        ~required:(native_state_requires (D.Document.payload document) ~kind))
  in
  let%bind document =
    require
      document
      ~name:Agent_protocol.Session_storage_semantics.delegated_usage
      ~limits
      ~required:(usage_state_requires (D.Document.payload document))
  in
  require
    document
    ~name:Agent_protocol.Session_storage_semantics.goal_descendants
    ~limits
    ~required:(descendant_state_requires (D.Document.payload document))
;;

let check_state document =
  let open Result.Let_syntax in
  let%bind () = check_native_state_fields (D.Document.payload document) in
  let%bind () =
    Q.check document ~required:(Q.state_requires (D.Document.payload document))
  in
  let%bind () =
    check document ~name ~required:(state_requires (D.Document.payload document))
  in
  let%bind () =
    List.fold_result native_branches ~init:() ~f:(fun () (kind, name) ->
      check
        document
        ~name
        ~required:(native_state_requires (D.Document.payload document) ~kind))
  in
  let%bind () =
    check
      document
      ~name:Agent_protocol.Session_storage_semantics.delegated_usage
      ~required:(usage_state_requires (D.Document.payload document))
  in
  check
    document
    ~name:Agent_protocol.Session_storage_semantics.goal_descendants
    ~required:(descendant_state_requires (D.Document.payload document))
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
    List.fold_result native_branches ~init:document ~f:(fun document (kind, name) ->
      require
        document
        ~name
        ~limits
        ~required:(native_delta_requires (D.Document.payload document) ~kind))
  in
  let%bind document =
    require
      document
      ~name:Agent_protocol.Session_storage_semantics.delegated_usage
      ~limits
      ~required:(usage_delta_requires (D.Document.payload document))
  in
  require
    document
    ~name:Agent_protocol.Session_storage_semantics.goal_descendants
    ~limits
    ~required:(descendant_delta_requires (D.Document.payload document))
;;

let check_delta document =
  let open Result.Let_syntax in
  let%bind () = check_native_delta_fields (D.Document.payload document) in
  let%bind () =
    Q.check document ~required:(Q.delta_requires (D.Document.payload document))
  in
  let%bind () =
    check document ~name ~required:(delta_requires (D.Document.payload document))
  in
  let%bind () =
    List.fold_result native_branches ~init:() ~f:(fun () (kind, name) ->
      check
        document
        ~name
        ~required:(native_delta_requires (D.Document.payload document) ~kind))
  in
  let%bind () =
    check
      document
      ~name:Agent_protocol.Session_storage_semantics.delegated_usage
      ~required:(usage_delta_requires (D.Document.payload document))
  in
  check
    document
    ~name:Agent_protocol.Session_storage_semantics.goal_descendants
    ~required:(descendant_delta_requires (D.Document.payload document))
;;
