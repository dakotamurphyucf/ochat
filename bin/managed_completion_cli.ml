open! Core
module P = Agent_protocol
module C = Agent_client

module Activation = struct
  type t =
    | Require_running
    | Start_stopped
end

module Request = struct
  type t =
    | Foreground_continue of Activation.t
    | Authored_start of P.Run.Mode.t
    | User_submission of
        { mode : P.Run.Mode.t
        ; content : P.Session.Message_content.t
        ; activation : Activation.t
        }
    | Observe_run of P.Run_receipt.t
end

module Evidence = struct
  type t =
    | Foreground of
        { operation : P.Operation.t
        ; admission : P.Mutation_result.t
        }
    | Managed_run of
        { admission : P.Run_receipt.t
        ; terminal : P.Run_query.View.t
        }
end

module Completed = struct
  type t =
    { evidence : Evidence.t
    ; export : C.Canonical_export.t
    ; chatmd : string
    }
end

module Error = struct
  type t =
    | Protocol of P.Error.t
    | Continuation_unavailable of P.History_edit.Continuation.unavailable
    | Rendering of History_chatmd.Rendering.Error.t
end

let protocol result = Result.map_error result ~f:(fun error -> Error.Protocol error)
let error code message = P.Error.create code ~message ~retryable:false ()

let current handle =
  Eio.Fiber.check ();
  match C.Session_handle.last_error handle with
  | Some failure -> Error failure
  | None ->
    if C.Session_handle.is_closed handle
    then Error (error Interrupted "managed completion reader is closed")
    else (
      let projection = C.Session_handle.projection handle in
      match C.Projection.synchronization projection with
      | Snapshot_required failure -> Error failure
      | Current -> Ok (P.Public.Snapshot.fields (C.Projection.snapshot projection)))
;;

let activate handle session activation =
  match session.P.Session.desired_state, session.observed_state, activation with
  | Running, Idle, _ -> Ok session
  | Stopped, Stopped, Activation.Start_stopped ->
    let%bind.Result activated = C.Session_handle.start handle ~queue_if_limited:false in
    (match activated.desired_state, activated.observed_state with
     | Running, Idle -> Ok activated
     | _ -> Error (error Conflict "selected session is not idle after activation"))
  | _, _, _ -> Error (error Conflict "selected session is not available for execution")
;;

let run
      connection
      ~clock
      ~session_id
      ~expected_generation
      ~expected_revision
      ~request
      ~prepare
      ~document_limits
      ~rendering_limits
      ~checkpoint
      ~on_view
      ~on_admission
  =
  let poll () =
    Eio.Fiber.check ();
    checkpoint ();
    Eio.Fiber.check ()
  in
  let present projection =
    poll ();
    on_view projection;
    poll ()
  in
  poll ();
  match C.Connection.initialization connection with
  | None ->
    Error (Error.Protocol (P.Error.invalid_request "connection is not initialized"))
  | Some initialized ->
    Eio.Switch.run (fun sw ->
      let completion = C.Completion_view.create session_id in
      let waiter = ref None in
      let changed = Eio.Condition.create () in
      let observe projection =
        C.Completion_view.observe completion projection;
        Option.iter !waiter ~f:(fun waiter ->
          Managed_run_waiter.observe waiter projection);
        Eio.Condition.broadcast changed
      in
      let open Result.Let_syntax in
      let%bind handle =
        C.Session_handle.attach
          ~sw
          ~clock
          ~connection
          ~session_id
          ~mode:Read_write
          ~on_update:observe
          ()
        |> protocol
      in
      let execute () =
        let%bind initial = current handle |> protocol in
        let%bind () =
          if
            Int.equal initial.session.generation expected_generation
            && Int64.(expected_revision >= 0L && expected_revision < max_value)
            && Int64.equal initial.revision (Int64.succ expected_revision)
          then Ok ()
          else Error (Error.Protocol (error Conflict "selected session anchor changed"))
        in
        let validate_selected session =
          let%bind actual = current handle |> protocol in
          if
            P.Id.Session.equal session.P.Session.id actual.session.id
            && Int.equal session.generation actual.session.generation
            && Int64.equal session.revision actual.session.revision
          then Ok ()
          else Error (Error.Protocol (error Conflict "prepared session anchor changed"))
        in
        let await_selected session =
          let wait () =
            Eio.Condition.loop_no_mutex changed (fun () ->
              match current handle with
              | Error failure -> Some (Error (Error.Protocol failure))
              | Ok actual ->
                if not (P.Id.Session.equal session.P.Session.id actual.session.id)
                then
                  Some
                    (Error (Error.Protocol (error Conflict "prepared session changed")))
                else if
                  Int.equal session.generation actual.session.generation
                  && Int64.equal session.revision actual.session.revision
                then Some (Ok ())
                else if
                  actual.session.generation < session.generation
                  || (Int.equal actual.session.generation session.generation
                      && Int64.(actual.session.revision < session.revision))
                then None
                else
                  Some
                    (Error
                       (Error.Protocol (error Conflict "prepared session anchor changed"))))
          in
          let closed () =
            C.Session_handle.await_closed handle;
            match current handle with
            | Error failure -> Error (Error.Protocol failure)
            | Ok _ -> Error (Error.Protocol (error Interrupted "session reader closed"))
          in
          Eio.Fiber.first wait closed
        in
        let%bind request, selected =
          match prepare with
          | None -> Ok (request, initial.session)
          | Some prepare -> prepare handle initial.session
        in
        poll ();
        let%bind () = await_selected selected in
        let%bind () = validate_selected selected in
        let await_run admission expected_mode =
          let views = C.Run_views.create connection ~server_id:initialized.server_id in
          let lookup : P.Run_query.Lookup_request.t =
            { session = P.Session_ref.create ~server_id:initialized.server_id ~session_id
            ; run_id = admission.P.Run_receipt.run_id
            }
          in
          let owned =
            Managed_run_waiter.create views ~sw ~request:lookup ~admission ~expected_mode
          in
          waiter := Some owned;
          let%map terminal =
            Managed_run_waiter.await_terminal owned ~handle ~on_view:present |> protocol
          in
          Evidence.Managed_run { admission; terminal }, terminal.session_revision
        in
        let admit mode input session =
          poll ();
          let%bind () = validate_selected session in
          let%bind admission =
            C.Session_handle.start_run
              handle
              ~generation:session.P.Session.generation
              ~expected_revision:session.revision
              ~mode
              ~input
            |> protocol
          in
          poll ();
          on_admission admission;
          poll ();
          await_run admission (Some mode)
        in
        let%bind evidence, revision =
          match request with
          | Request.Observe_run admission -> await_run admission None
          | Authored_start mode -> admit mode Authored_start selected
          | User_submission { mode; content; activation } ->
            let%bind session = activate handle selected activation |> protocol in
            let%bind () = await_selected session in
            admit mode (User_submission content) session
          | Foreground_continue activation ->
            let%bind session = activate handle selected activation |> protocol in
            let%bind () = await_selected session in
            poll ();
            let%bind () = validate_selected session in
            let%bind continued =
              C.Session_handle.continue_history
                handle
                ~expected_generation:session.generation
                ~expected_revision:session.revision
              |> protocol
            in
            (match continued.continuation with
             | Not_started reason -> Error (Error.Continuation_unavailable reason)
             | Not_requested ->
               Error
                 (Error.Protocol (error Invalid_state "continuation was not requested"))
             | Started operation ->
               let%bind projection =
                 C.Completion_view.await_terminal
                   completion
                   ~handle
                   ~operation
                   ~after:continued.mutation
                   ~on_view:present
                 |> protocol
               in
               let operation =
                 C.Projection.terminal_operation projection |> Option.value_exn
               in
               let fields = P.Public.Snapshot.fields (C.Projection.snapshot projection) in
               Ok
                 ( Evidence.Foreground { operation; admission = continued.mutation }
                 , fields.revision ))
        in
        poll ();
        let%bind export =
          C.Canonical_export.read
            handle
            ~revision
            ~limits:document_limits
            ~checkpoint:poll
          |> protocol
        in
        let fields = P.Public.Snapshot.fields (C.Canonical_export.snapshot export) in
        let generation, event_sequence =
          match evidence with
          | Foreground { operation; _ } ->
            operation.generation, fields.latest_event_sequence
          | Managed_run { admission; terminal } ->
            admission.source.generation, terminal.event_sequence
        in
        let%bind () =
          if
            Int.equal fields.session.generation generation
            && Int64.equal fields.latest_event_sequence event_sequence
          then Ok ()
          else
            Error
              (Error.Protocol
                 (error Conflict "canonical export does not match terminal evidence"))
        in
        let%bind chatmd =
          Agent_session.Chatmd_export.render_public_bounded
            fields.canonical_history.entries
            ~limits:rendering_limits
            ~checkpoint:poll
          |> Result.map_error ~f:(fun error -> Error.Rendering error)
        in
        poll ();
        Ok Completed.{ evidence; export; chatmd }
      in
      let result =
        try Ok (execute ()) with
        | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
      in
      let cleaned =
        try
          Eio.Cancel.protect (fun () -> C.Session_handle.close handle);
          Ok ()
        with
        | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
      in
      match result, cleaned with
      | Error (exn, backtrace), _ -> Exn.raise_with_original_backtrace exn backtrace
      | Ok (Error _ as failure), _ -> failure
      | Ok result, Ok () ->
        Eio.Fiber.check ();
        result
      | Ok (Ok _), Error (exn, backtrace) ->
        Exn.raise_with_original_backtrace exn backtrace)
;;
