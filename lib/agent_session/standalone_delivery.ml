open Core
module P = Agent_protocol

type t =
  { revision : int64
  ; delivery : P.Delivery.t
  }

let missing message = P.Error.invalid_request message

let subjects (state : Session_state.t) invocation_id job_id =
  let open Result.Let_syntax in
  let%bind invocation =
    List.find state.invocations ~f:(fun value ->
      P.Id.Invocation.equal value.P.Invocation.context.id invocation_id)
    |> Result.of_option ~error:(missing "standalone adapter invocation is missing")
  in
  let%map job =
    List.find state.jobs ~f:(fun value -> P.Id.Job.equal value.P.Job.id job_id)
    |> Result.of_option ~error:(missing "standalone adapter job is missing")
  in
  invocation, job
;;

let prepare
      ~state
      ~invocation_id
      ~job_id
      ~completion
      ~current_capabilities
      ~delivery_id
      ~now
      ~wake
  =
  let open Result.Let_syntax in
  let%bind invocation, job = subjects state invocation_id job_id in
  let%bind () =
    Extension_invariants.owner
      ~session_id:state.identity.session_id
      ~generation:state.identity.generation
      job.session_id
      job.generation
  in
  let%bind projected =
    Standalone_completion_contract.project
      ~invocation
      ~job
      ~completion
      ~current_capabilities
  in
  let%bind delivery =
    P.Delivery.create
      ~disclosure_pins:projected.disclosure_pins
      ~completion_projection:projected.receipt
      { id = delivery_id
      ; session_id = job.session_id
      ; generation = job.generation
      ; invocation_id = Some invocation_id
      ; work = Some (Job job_id)
      ; correlation = P.Id.Invocation.to_string invocation_id
      ; source = Job_adapter
      ; completion = projected.completion
      ; wake
      ; created_at = now
      ; ownership = None
      }
  in
  let%map () =
    Standalone_completion_contract.validate_projection ~invocation ~job delivery
  in
  { revision = state.counters.revision; delivery }
;;

let revalidate
      ~(state : Session_state.t)
      ~staged
      ~(limits : Staged_notifications.limits)
      plan
  =
  let open Result.Let_syntax in
  let c = plan.delivery.context in
  let%bind () =
    match Int64.equal state.counters.revision plan.revision with
    | true -> Ok ()
    | false ->
      Error
        (P.Error.create
           Conflict
           ~message:"standalone completion snapshot changed"
           ~retryable:true
           ())
  in
  let%bind () =
    Extension_invariants.owner
      ~session_id:state.identity.session_id
      ~generation:state.identity.generation
      c.session_id
      c.generation
  in
  let%bind invocation, job =
    match c.invocation_id, c.work with
    | Some invocation_id, Some (Job job_id) -> subjects state invocation_id job_id
    | _ -> Error (missing "standalone completion has no originating work")
  in
  let%bind () =
    Standalone_completion_contract.validate_projection ~invocation ~job plan.delivery
  in
  let%bind () =
    match invocation.publication_discarded with
    | None -> Ok ()
    | Some _ -> Error (missing "standalone acknowledgement was discarded")
  in
  let retained = state.deliveries @ staged in
  let%bind () =
    match
      List.exists retained ~f:(fun value ->
        P.Id.Delivery.equal value.P.Delivery.context.id c.id
        || Option.equal P.Invocation.equal_work value.context.work c.work)
    with
    | false -> Ok ()
    | true ->
      Error
        (P.Error.create
           Conflict
           ~message:"standalone work already has a delivery owner"
           ~retryable:false
           ())
  in
  let%bind () = Staged_notifications.validate_limits limits in
  let pending =
    List.filter retained ~f:(fun value ->
      match value.P.Delivery.status with
      | Pending -> true
      | _ -> false)
  in
  let publisher_count =
    List.count pending ~f:(fun value ->
      match value.P.Delivery.context.invocation_id with
      | None -> false
      | Some id ->
        List.exists state.invocations ~f:(fun other ->
          P.Id.Invocation.equal other.context.id id
          && Option.equal
               P.Completion_contract.equal
               other.completion_contract
               invocation.completion_contract))
  in
  let%bind () =
    match
      List.length retained < limits.max_retained
      && List.length pending < limits.max_pending
      && publisher_count < limits.max_per_source
    with
    | true -> Ok ()
    | false ->
      Error
        (P.Error.create
           Resource_limit
           ~message:"standalone notification quota reached"
           ~retryable:true
           ())
  in
  P.Json_codec.validate_limits
    ~max_bytes:limits.max_payload_bytes
    ~max_depth:limits.max_payload_depth
    (P.Completion.to_json c.completion)
;;
