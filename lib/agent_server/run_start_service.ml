open! Core
module P = Agent_protocol
module A = Agent_session

type t =
  { server_id : P.Id.Server.t
  ; authorize : Operator_authorization.t -> A.Session_state.t -> (unit, P.Error.t) result
  }

let create ~server_id ~authorize = { server_id; authorize }

let start_attempt
      t
      ~principal_id
      ~authorize
      ~begin_preparation
      ~(entry : Session_registry.entry)
      ~(request : A.Run_admission_request.t)
      ~command_audit
  =
  let open Result.Let_syntax in
  (* Enqueue and await form one acquisition. Cancellation must not abandon a
     permit which the actor can install after this caller has gone away. *)
  let%bind decision = Eio.Cancel.protect begin_preparation in
  match decision with
  | A.Run_preparation.Decision.Retained receipt ->
    Eio.Fiber.check ();
    Ok (A.Run_admission_outcome.Admitted receipt)
  | A.Run_preparation.Decision.Prepare preparation ->
    let result =
      try
        (* Cleanup below is established before observing pending cancellation.
           Do not let a cancelled acquisition enter cold constructor effects. *)
        Eio.Fiber.check ();
        Ok
          (Runtime_owner.with_prepared_run_runtime
             entry.runtime
             ~preparation
             (fun runtime ->
                let%bind observer =
                  Option.bind
                    runtime.moderator_manager
                    ~f:Chat_response.Moderator_manager.invocation_observer
                  |> Result.of_option
                       ~error:
                         (P.Error.create
                            Invalid_state
                            ~retryable:false
                            ~message:"run admission requires compiled orchestration"
                            ())
                in
                let startup_pending () =
                  Option.value_map
                    runtime.moderator_activation
                    ~default:false
                    ~f:(fun activation -> activation.startup_pending ())
                in
                let%bind scope =
                  A.Run_admission.Scope.create
                    ~principal_id
                    ~observer
                    ~startup_pending
                    ~authorize
                in
                let%bind history =
                  match request.input with
                  | Authored_start -> Ok None
                  | User_submission content ->
                    let%bind reservation =
                      A.Session_actor.reserve_run_history_block
                        entry.actor
                        ~preparation
                        ~count:1
                    in
                    let%bind sequence =
                      if Int64.(reservation.first_sequence > of_int Int.max_value)
                      then
                        Error
                          (P.Error.invalid_request
                             "run input history sequence exceeds platform range")
                      else Ok (Int64.to_int_exn reservation.first_sequence)
                    in
                    let%bind id =
                      History_entry.Id.create
                        ~namespace:(A.History_id_source.namespace entry.history_ids)
                        ~sequence
                      |> Result.map_error ~f:P.Error.invalid_request
                    in
                    let%map history = runtime.parse_user_content ~id content in
                    Some (A.History_codec.to_protocol history)
                in
                let session =
                  P.Session_ref.create
                    ~server_id:t.server_id
                    ~session_id:request.session_id
                in
                A.Session_actor.admit_prepared_run
                  entry.actor
                  ~command_audit
                  ~preparation
                  ~scope
                  ~session
                  ~entry:history))
      with
      | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
    in
    let cleaned =
      try
        Eio.Cancel.protect (fun () ->
          A.Session_actor.end_run_preparation entry.actor preparation)
        |> fun result -> Ok result
      with
      | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
    in
    (match result, cleaned with
     | Error (exn, backtrace), _ -> Exn.raise_with_original_backtrace exn backtrace
     | Ok (Error _ as failure), _ -> failure
     | Ok (Ok outcome), Ok (Ok ()) -> Ok outcome
     | Ok (Ok outcome), Ok (Error _) -> Ok outcome
     | Ok (Ok ((Rejected _ | Uncertain _) as outcome)), Error _ -> Ok outcome
     | Ok (Ok (Admitted _)), Error (exn, backtrace) ->
       Exn.raise_with_original_backtrace exn backtrace)
;;

let outcome = function
  | Ok outcome -> outcome
  | Error failure -> A.Run_admission_outcome.Rejected failure
;;

let start t ~actor ~entry ~request ~command_audit =
  let principal_id = (Operator_authorization.principal actor).id in
  let authorize state =
    if Operator_authorization.is_current actor
    then t.authorize actor state
    else
      Error
        (P.Error.create
           Permission_denied
           ~retryable:false
           ~message:"run authority expired or was revoked"
           ())
  in
  let result =
    let open Result.Let_syntax in
    let normalized = A.Run_admission_request.of_writer request in
    let%bind request_sha256 = A.Run_admission_request.request_sha256 normalized in
    start_attempt
      t
      ~principal_id
      ~authorize
      ~begin_preparation:(fun () ->
        A.Session_actor.begin_run_preparation
          entry.Session_registry.actor
          ~authorize
          ~principal_id
          ~request
          ~request_sha256)
      ~entry
      ~request:normalized
      ~command_audit
  in
  outcome result
;;

let start_headless_command t ~actor ~entry ~request ~command_audit =
  let principal_id = (Operator_authorization.principal actor).id in
  let host_policy = t.authorize actor in
  let authorize = A.Run_command_authorization.check actor ~authorize:host_policy in
  start_attempt
    t
    ~principal_id
    ~authorize
    ~begin_preparation:(fun () ->
      A.Session_actor.begin_headless_run_preparation
        entry.Session_registry.actor
        ~actor
        ~authorize:host_policy
        ~request)
    ~entry
    ~request:(A.Run_admission_request.of_headless request)
    ~command_audit
  |> outcome
;;
