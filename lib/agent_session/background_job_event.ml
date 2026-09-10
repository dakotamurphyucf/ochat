open Core
module P = Agent_protocol

let source ~state (job : P.Job.t) =
  let open Result.Let_syntax in
  let%bind () =
    Extension_invariants.owner
      ~session_id:state.Session_state.identity.session_id
      ~generation:state.identity.generation
      job.session_id
      job.generation
  in
  let%bind () =
    Job_launch.validate
      ~invocations:state.invocations
      ~events:state.moderator_executions
      ~jobs:state.jobs
      job
  in
  match job.kind, job.launch with
  | Async_tool, Some { moderator_source = Some source; _ } -> Ok (Some source)
  | Async_tool, Some { owner = Invocation id; _ } ->
    let%map invocation =
      List.find state.invocations ~f:(fun invocation ->
        P.Id.Invocation.equal invocation.context.id id)
      |> Result.of_option
           ~error:(P.Error.invalid_request "background event creator is missing")
    in
    Option.map invocation.observation ~f:(fun observation ->
      observation.P.Invocation.observer)
  | Async_tool, Some { owner = Moderator_event id; _ } ->
    let%map event =
      List.find state.moderator_executions ~f:(fun event ->
        P.Id.Moderator_execution.equal event.context.id id)
      |> Result.of_option
           ~error:(P.Error.invalid_request "background event creator is missing")
    in
    Some event.context.source
  | _ -> Ok None
;;

let frame ~state ~observer job =
  let open Result.Let_syntax in
  let%bind source = source ~state job in
  match source with
  | Some source when P.Invocation.equal_observer source observer ->
    Chat_response.Background_delivery.create ~source job
    |> Result.map_error ~f:P.Error.invalid_request
  | _ ->
    Error
      (P.Error.create
         Permission_denied
         ~message:"background event belongs to another or absent moderator source"
         ~retryable:false
         ())
;;
