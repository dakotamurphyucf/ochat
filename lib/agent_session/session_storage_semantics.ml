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

(* Null is an explicit captured unavailable producer, not historical absence.
   Schema6 also requires the marker even if malformed input omits the field. *)
let queue_receipt_requires receipt =
  (match D.Json.field receipt ~name:"schema_version" with
   | Value (`Number "6") -> true
   | Value _ | Absent | Null -> false)
  ||
  match D.Json.field receipt ~name:"queue_occurrence" with
  | Value capture ->
    (match D.Json.field capture ~name:"producer" with
     | Absent -> false
     | Null | Value _ -> true)
  | Absent | Null -> false
;;

let queue_receipt_state_requires payload =
  match D.Json.field payload ~name:"moderator_executions" with
  | Value (`Array receipts) -> List.exists receipts ~f:queue_receipt_requires
  | Absent | Null | Value _ -> false
;;

let queue_receipt_delta_requires payload =
  match D.Json.field payload ~name:"changes" with
  | Value (`Array changes) ->
    List.exists changes ~f:(fun change ->
      match D.Json.field change ~name:"kind" with
      | Value
          (`String
             ( "moderator_execution_changed"
             | "moderator_execution_reconciled"
             | "moderator_lifecycle_claimed" )) ->
        (match D.Json.field change ~name:"value" with
         | Value receipt -> queue_receipt_requires receipt
         | Absent | Null -> false)
      | Value (`String "created") ->
        (match D.Json.field change ~name:"state" with
         | Value state ->
           (match D.Json.field state ~name:"payload" with
            | Value payload -> queue_receipt_state_requires payload
            | Absent | Null -> false)
         | Absent | Null -> false)
      | Value _ | Absent | Null -> false)
  | Value _ | Absent | Null -> false
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
      | Value (`String "policy_inference_ledger_changed") ->
        (match D.Json.field change ~name:"usage" with
         | Value _ -> true
         | Absent | Null -> false)
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

(* An old reader supporting queue-provenance.v1 still ignores an optional
   ingress original. This feature therefore has its own child semantics fence. *)
let ingress_registration_requires value =
  match D.Json.field value ~name:"inference_provenance" with
  | Value _ -> true
  | Absent | Null -> false
;;

let ingress_queue_requires moderator =
  match D.Json.field moderator ~name:"queue_provenance" with
  | Value metadata ->
    (match D.Json.field metadata ~name:"entries" with
     | Value (`Array entries) ->
       List.exists entries ~f:(fun entry ->
         match D.Json.field entry ~name:"producer" with
         | Value producer ->
           (match D.Json.field producer ~name:"kind" with
            | Value (`String "ingress_delivery") -> true
            | Absent | Null | Value _ -> false)
         | Absent | Null -> false)
     | Absent | Null | Value _ -> false)
  | Absent | Null -> false
;;

let ingress_state_requires payload =
  (match D.Json.field payload ~name:"ingress_registrations" with
   | Value (`Array values) -> List.exists values ~f:ingress_registration_requires
   | Absent | Null | Value _ -> false)
  ||
  match D.Json.field payload ~name:"moderator" with
  | Value moderator -> ingress_queue_requires moderator
  | Absent | Null -> false
;;

let ingress_delta_requires payload =
  match D.Json.field payload ~name:"changes" with
  | Value (`Array changes) ->
    List.exists changes ~f:(fun change ->
      match D.Json.field change ~name:"kind" with
      | Value (`String "ingress_changed") ->
        (match D.Json.field change ~name:"value" with
         | Value value -> ingress_registration_requires value
         | Absent | Null -> false)
      | Value (`String "moderator_changed") ->
        (match D.Json.field change ~name:"moderator" with
         | Value value -> ingress_queue_requires value
         | Absent | Null -> false)
      | Value (`String "created") ->
        (match D.Json.field change ~name:"state" with
         | Value state ->
           (match D.Json.field state ~name:"payload" with
            | Value payload -> ingress_state_requires payload
            | Absent | Null -> false)
         | Absent | Null -> false)
      | Absent | Null | Value _ -> false)
  | Absent | Null | Value _ -> false
;;

let policy_state_requires payload = populated payload "policy_canonical_usage"

let policy_delta_requires payload =
  match D.Json.field payload ~name:"changes" with
  | Value (`Array changes) ->
    List.exists changes ~f:(fun change ->
      match D.Json.field change ~name:"kind" with
      | Value (`String ("policy_cause_changed" | "policy_inference_ledger_changed")) ->
        true
      | Value (`String "created") ->
        (match D.Json.field change ~name:"state" with
         | Value state ->
           (match D.Json.field state ~name:"payload" with
            | Value payload -> policy_state_requires payload
            | Absent | Null -> false)
         | Absent | Null -> false)
      | Value _ | Absent | Null -> false)
  | Value _ | Absent | Null -> false
;;

let goal_policy_state_requires payload = populated payload "goal_policy_contributions"

let goal_policy_delta_requires payload =
  match D.Json.field payload ~name:"changes" with
  | Value (`Array changes) ->
    List.exists changes ~f:(fun change ->
      match D.Json.field change ~name:"kind" with
      | Value (`String "goal_policy_contributions_changed") -> true
      | Value (`String "created") ->
        (match D.Json.field change ~name:"state" with
         | Value state ->
           (match D.Json.field state ~name:"payload" with
            | Value payload -> goal_policy_state_requires payload
            | Absent | Null -> false)
         | Absent | Null -> false)
      | Value _ | Absent | Null -> false)
  | Value _ | Absent | Null -> false
;;

let require_state document ~limits =
  let open Result.Let_syntax in
  let%bind document =
    require
      document
      ~name:Agent_protocol.Session_storage_semantics.queue_occurrence_producer
      ~required:(queue_receipt_state_requires (D.Document.payload document))
      ~limits
  in
  let%bind document =
    require
      document
      ~name:Agent_protocol.Session_storage_semantics.policy_canonical_usage
      ~required:(policy_state_requires (D.Document.payload document))
      ~limits
  in
  let%bind document =
    require
      document
      ~name:Agent_protocol.Session_storage_semantics.goal_policy_contributions
      ~required:(goal_policy_state_requires (D.Document.payload document))
      ~limits
  in
  let%bind document =
    require
      document
      ~name:Agent_protocol.Session_storage_semantics.ingress_provenance
      ~required:(ingress_state_requires (D.Document.payload document))
      ~limits
  in
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
  let%bind () =
    check
      document
      ~name:Agent_protocol.Session_storage_semantics.queue_occurrence_producer
      ~required:(queue_receipt_state_requires (D.Document.payload document))
  in
  let%bind () =
    check
      document
      ~name:Agent_protocol.Session_storage_semantics.policy_canonical_usage
      ~required:(policy_state_requires (D.Document.payload document))
  in
  let%bind () =
    check
      document
      ~name:Agent_protocol.Session_storage_semantics.goal_policy_contributions
      ~required:(goal_policy_state_requires (D.Document.payload document))
  in
  let%bind () = check_native_state_fields (D.Document.payload document) in
  let%bind () =
    check
      document
      ~name:Agent_protocol.Session_storage_semantics.ingress_provenance
      ~required:(ingress_state_requires (D.Document.payload document))
  in
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
    require
      document
      ~name:Agent_protocol.Session_storage_semantics.queue_occurrence_producer
      ~required:(queue_receipt_delta_requires (D.Document.payload document))
      ~limits
  in
  let%bind document =
    require
      document
      ~name:Agent_protocol.Session_storage_semantics.policy_canonical_usage
      ~required:(policy_delta_requires (D.Document.payload document))
      ~limits
  in
  let%bind document =
    require
      document
      ~name:Agent_protocol.Session_storage_semantics.goal_policy_contributions
      ~required:(goal_policy_delta_requires (D.Document.payload document))
      ~limits
  in
  let%bind document =
    require
      document
      ~name:Agent_protocol.Session_storage_semantics.ingress_provenance
      ~required:(ingress_delta_requires (D.Document.payload document))
      ~limits
  in
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
  let%bind () =
    check
      document
      ~name:Agent_protocol.Session_storage_semantics.queue_occurrence_producer
      ~required:(queue_receipt_delta_requires (D.Document.payload document))
  in
  let%bind () =
    check
      document
      ~name:Agent_protocol.Session_storage_semantics.policy_canonical_usage
      ~required:(policy_delta_requires (D.Document.payload document))
  in
  let%bind () =
    check
      document
      ~name:Agent_protocol.Session_storage_semantics.goal_policy_contributions
      ~required:(goal_policy_delta_requires (D.Document.payload document))
  in
  let%bind () = check_native_delta_fields (D.Document.payload document) in
  let%bind () =
    check
      document
      ~name:Agent_protocol.Session_storage_semantics.ingress_provenance
      ~required:(ingress_delta_requires (D.Document.payload document))
  in
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

(* Explicit null is a known cleared slot, not historical absence. Old writers
   must not retain this private eligibility DATA without its invalidation laws. *)
let lifecycle_index_requires index =
  match D.Json.field index ~name:"moderator_lifecycle" with
  | Absent -> false
  | Null | Value _ -> true
;;

let lifecycle_state_requires payload =
  match D.Json.field payload ~name:"run_state" with
  | Value index -> lifecycle_index_requires index
  | Absent | Null -> false
;;

let lifecycle_delta_requires payload =
  match D.Json.field payload ~name:"changes" with
  | Value (`Array changes) ->
    List.exists changes ~f:(fun change ->
      match D.Json.field change ~name:"kind" with
      | Value (`String "moderator_lifecycle_claimed") -> true
      | Value (`String "run_state_changed") ->
        (match D.Json.field change ~name:"value" with
         | Value index -> lifecycle_index_requires index
         | Absent | Null -> false)
      | Value (`String "created") ->
        (match D.Json.field change ~name:"state" with
         | Value state ->
           (match D.Json.field state ~name:"payload" with
            | Value payload -> lifecycle_state_requires payload
            | Absent | Null -> false)
         | Absent | Null -> false)
      | Value _ | Absent | Null -> false)
  | Value _ | Absent | Null -> false
;;

let recurrence_goal_name =
  Agent_protocol.Session_storage_semantics.recurrence_controller_goal
;;

let recurrence_goal_definition_requires definition =
  (match D.Json.field definition ~name:"required_semantics" with
   | Value (`Array names) ->
     List.exists names ~f:(function
       | `String name -> String.equal name recurrence_goal_name
       | _ -> false)
   | Absent | Null | Value _ -> false)
  ||
  match D.Json.field definition ~name:"payload" with
  | Value payload ->
    (match D.Json.field payload ~name:"goal_binding" with
     | Value _ | Null -> true
     | Absent -> false)
  | Absent | Null -> false
;;

let recurrence_goal_entry_requires row =
  match D.Json.field row ~name:"document" with
  | Value entry ->
    (match D.Json.field entry ~name:"payload" with
     | Value payload ->
       (match D.Json.field payload ~name:"definitions" with
        | Value (`Array definitions) ->
          List.exists definitions ~f:(fun row ->
            match D.Json.field row ~name:"value" with
            | Value value -> recurrence_goal_definition_requires value
            | Absent | Null -> false)
        | Absent | Null | Value _ -> false)
     | Absent | Null -> false)
  | Absent | Null -> false
;;

let recurrence_goal_catalog_requires catalog =
  match D.Json.field catalog ~name:"entries" with
  | Value (`Array rows) -> List.exists rows ~f:recurrence_goal_entry_requires
  | Absent | Null | Value _ -> false
;;

let recurrence_goal_state_requires payload =
  match D.Json.field payload ~name:"recurrence_catalog" with
  | Value catalog -> recurrence_goal_catalog_requires catalog
  | Absent | Null -> false
;;

let recurrence_goal_delta_requires payload =
  match D.Json.field payload ~name:"changes" with
  | Value (`Array changes) ->
    List.exists changes ~f:(fun change ->
      match D.Json.field change ~name:"kind" with
      | Value (`String "recurrence_catalog_changed") ->
        (match D.Json.field change ~name:"value" with
         | Value catalog -> recurrence_goal_catalog_requires catalog
         | Absent | Null -> false)
      | Value (`String "created") ->
        (match D.Json.field change ~name:"state" with
         | Value state ->
           (match D.Json.field state ~name:"payload" with
            | Value payload -> recurrence_goal_state_requires payload
            | Absent | Null -> false)
         | Absent | Null -> false)
      | Absent | Null | Value _ -> false)
  | Absent | Null | Value _ -> false
;;

let require_state_without_lifecycle = require_state

let require_state document ~limits =
  let%bind.Result document = require_state_without_lifecycle document ~limits in
  let payload = D.Document.payload document in
  let%bind.Result document =
    require
      document
      ~name:Agent_protocol.Session_storage_semantics.moderator_lifecycle
      ~required:(lifecycle_state_requires payload)
      ~limits
  in
  require
    document
    ~name:recurrence_goal_name
    ~required:(recurrence_goal_state_requires payload)
    ~limits
;;

let check_state_without_lifecycle = check_state

let check_state document =
  let%bind.Result () = check_state_without_lifecycle document in
  let payload = D.Document.payload document in
  let%bind.Result () =
    check
      document
      ~name:Agent_protocol.Session_storage_semantics.moderator_lifecycle
      ~required:(lifecycle_state_requires payload)
  in
  check
    document
    ~name:recurrence_goal_name
    ~required:(recurrence_goal_state_requires payload)
;;

let require_delta_without_lifecycle = require_delta

let require_delta document ~limits =
  let%bind.Result document = require_delta_without_lifecycle document ~limits in
  let payload = D.Document.payload document in
  let%bind.Result document =
    require
      document
      ~name:Agent_protocol.Session_storage_semantics.moderator_lifecycle
      ~required:(lifecycle_delta_requires payload)
      ~limits
  in
  require
    document
    ~name:recurrence_goal_name
    ~required:(recurrence_goal_delta_requires payload)
    ~limits
;;

let check_delta_without_lifecycle = check_delta

let check_delta document =
  let%bind.Result () = check_delta_without_lifecycle document in
  let payload = D.Document.payload document in
  let%bind.Result () =
    check
      document
      ~name:Agent_protocol.Session_storage_semantics.moderator_lifecycle
      ~required:(lifecycle_delta_requires payload)
  in
  check
    document
    ~name:recurrence_goal_name
    ~required:(recurrence_goal_delta_requires payload)
;;
