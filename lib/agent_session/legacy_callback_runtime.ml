open! Core
module Manager = Chat_response.Moderator_manager
module Moderation = Chat_response.Moderation

module Services = struct
  type schedules =
    { after_ms : delay_ms:int -> payload:Jsonaf.t -> (string, string) result
    ; cancel : id:string -> (unit, string) result
    }

  type t =
    { jobs : Runtime_model_services.t
    ; schedules : schedules
    ; execute : Native_tool_invocation.executor
    ; executing : Agent_protocol.Moderator_execution.t
    ; check_current : unit -> (unit, Agent_protocol.Error.t) result
    ; is_halted : unit -> bool
    ; checkpoint : unit -> (Jsonaf.t option, Agent_protocol.Error.t) result
    }
end

type claim =
  manager:Manager.t
  -> callback:Manager.Callback.t
  -> (services:Services.t
      -> commit:
           (snapshot:Session.Moderator_state.Identity_snapshot.t
            -> requests:Agent_protocol.Invocation.follow_up
            -> notifications:string list
            -> (unit, Agent_protocol.Error.t) result)
      -> (unit, Agent_protocol.Error.t) result)
  -> (bool, Agent_protocol.Error.t) result

type t =
  { outcome : Moderation.Outcome.t
  ; checkpoint : Jsonaf.t option
  }

let failure message =
  Agent_protocol.Error.create Invalid_state ~message ~retryable:false ()
;;

let run
      manager
      ~(claim : claim)
      ~model_recipes
      ~script_tools
      ~selected
      ~selection
      ~session_id:session_text
      ~history
      ~available_tools:tools
      ~session_meta
      ~now_ms
  =
  let open Result.Let_syntax in
  let checkpoint = ref None in
  (* The Manager's script-facing API uses strings. Keep the first actual host
     rejection only for this callback so an unsuccessful return can restore its
     typed cause after all Manager and Actor brackets have unwound. *)
  let host_rejection : Agent_protocol.Error.t option ref = ref None in
  let record_host_rejection (error : Agent_protocol.Error.t) =
    if Option.is_none !host_rejection then host_rejection := Some error;
    error.message
  in
  let%bind outcome =
    Manager.handle_legacy_callback_entries
      manager
      ~selection
      ~session_id:session_text
      ~now_ms
      ~history
      ~available_tools:tools
      ~session_meta
      ~with_admission:(fun callback ~snapshot:_ run ->
        let outcome = ref None in
        let%bind committed =
          claim ~manager ~callback (fun ~services ~commit ->
            let scoped_tools =
              Script_tool_calls.with_lifecycle script_tools ~is_halted:services.is_halted
            in
            Script_tool_calls.with_legacy_event
              scoped_tools
              ~selected
              ~execute:services.execute
              ~executing:services.executing
              (fun tool_call ->
                 let binding : Manager.callback_binding =
                   { services =
                       { model_recipes = model_recipes services.jobs
                       ; tool_call
                       ; after_ms =
                           (fun ~delay_ms ~payload ->
                             let%bind payload =
                               Chatml.Chatml_value_codec.Snapshot.of_value payload
                             in
                             services.schedules.after_ms
                               ~delay_ms
                               ~payload:
                                 (Chatml.Chatml_value_codec.Snapshot.to_jsonaf payload))
                       ; cancel = services.schedules.cancel
                       }
                   ; authorize =
                       (fun () ->
                         services.check_current ()
                         |> Result.map_error ~f:record_host_rejection)
                   ; prepare =
                       (fun ~(outcome : Moderation.Outcome.t) ~snapshot ->
                         let requests : Agent_protocol.Invocation.follow_up =
                           { request_turn =
                               Chat_response.Runtime_semantics.request_turn
                                 outcome.runtime_requests
                           ; request_compaction =
                               Chat_response.Runtime_semantics.request_compaction
                                 outcome.runtime_requests
                           ; end_session =
                               Chat_response.Runtime_semantics.should_end_session
                                 outcome.runtime_requests
                           }
                         in
                         Ok
                           { Manager.persist =
                               (fun () ->
                                 let result =
                                   let%bind () =
                                     commit
                                       ~snapshot
                                       ~requests
                                       ~notifications:outcome.ui_notifications
                                   in
                                   let%map acknowledged = services.checkpoint () in
                                   checkpoint := Some acknowledged
                                 in
                                 Result.map_error result ~f:record_host_rejection)
                           ; install = ignore
                           })
                   }
                 in
                 run binding
                 |> Result.map_error ~f:failure
                 |> Result.map ~f:(fun value -> outcome := Some value)))
          |> Result.map_error ~f:record_host_rejection
        in
        match committed, !outcome with
        | true, Some outcome -> Ok outcome
        | false, _ -> Error "legacy callback has no current physical owner"
        | true, None -> Error "legacy callback committed without its outcome")
    |> Result.map_error ~f:(fun message ->
      Option.value !host_rejection ~default:(failure message))
  in
  match outcome, !checkpoint with
  | None, _ -> Ok None
  | Some outcome, Some checkpoint -> Ok (Some { outcome; checkpoint })
  | Some _, None -> Error (failure "legacy callback did not acknowledge its checkpoint")
;;

module Installed = struct
  type callback =
    selection:Manager.Callback.Selection.t
    -> history:History_entry.t list
    -> available_tools:Openai.Responses.Request.Tool.t list
    -> now_ms:int
    -> (t option, Agent_protocol.Error.t) result

  type t =
    { manager : Manager.t
    ; run : callback
    }

  let create manager ~run = { manager; run }

  let halted t =
    let open Result.Let_syntax in
    let%map snapshot =
      Manager.identity_snapshot t.manager |> Result.map_error ~f:failure
    in
    match snapshot.halted with
    | false -> None
    | true ->
      Some
        { Moderation.Outcome.empty with
          runtime_requests =
            [ End_session
                (Option.value snapshot.halted_reason ~default:"moderator halted")
            ]
        }
  ;;

  let acknowledged_outcome result =
    (* The exact Event ACK has already published these notifications. Keep the
       actual outcome's other facts for its one downstream consumption policy. *)
    { result.outcome with ui_notifications = [] }
  ;;

  let drain t ~history ~available_tools ~now_ms ~max_events =
    let open Result.Let_syntax in
    let rec loop remaining outcomes =
      match remaining with
      | 0 -> Ok (List.rev outcomes)
      | _ ->
        let%bind stopped = halted t in
        (match stopped with
         | Some outcome -> Ok (List.rev (outcome :: outcomes))
         | None ->
           let%bind next =
             t.run ~selection:Queue_head ~history ~available_tools ~now_ms
           in
           (match next with
            | None -> Ok (List.rev outcomes)
            | Some result ->
              let outcome = acknowledged_outcome result in
              (match
                 Chat_response.Runtime_semantics.should_end_session
                   outcome.runtime_requests
               with
               | Some _ -> Ok (List.rev (outcome :: outcomes))
               | None -> loop (remaining - 1) (outcome :: outcomes))))
    in
    loop (Int.clamp_exn max_events ~min:0 ~max:256) []
  ;;

  let foreground_result = function
    | Ok value -> Ok value
    | Error error -> raise (Chat_response.Ctx.Effect_rejected error)
  ;;

  let foreground_handlers t ~capabilities =
    let open Result.Let_syntax in
    let%bind snapshot =
      Manager.identity_snapshot t.manager |> Result.map_error ~f:failure
    in
    let observer : Agent_protocol.Invocation.observer =
      { script_id = snapshot.script_id; source_sha256 = snapshot.script_source_hash }
    in
    let%bind () =
      capabilities.Operation_worker.Capabilities.manage_moderator_follow_up ~observer
    in
    let handle ~history ~available_tools ~now_ms ~event =
      (let%bind stopped = halted t in
       match stopped with
       | Some outcome -> Ok (Some outcome)
       | None ->
         (match t.run ~selection:(Boundary event) ~history ~available_tools ~now_ms with
          | Ok (Some result) ->
            Ok (Some (Moderator_event.foreground_outcome (acknowledged_outcome result)))
          | Ok None -> Error (failure "foreground legacy callback was not admitted")
          | Error error -> Error error))
      |> foreground_result
    in
    let drain ~history ~available_tools ~now_ms ~max_events =
      drain t ~history ~available_tools ~now_ms ~max_events
      |> Result.map ~f:(List.map ~f:Moderator_event.foreground_outcome)
      |> foreground_result
    in
    Ok
      Chat_response.In_memory_stream.
        { handle
        ; drain
        ; before_model_call =
            (fun () -> capabilities.admit_moderator_turn () |> foreground_result)
        }
  ;;
end
