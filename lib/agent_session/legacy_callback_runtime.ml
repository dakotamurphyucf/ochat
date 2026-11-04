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
                         |> Result.map_error ~f:(fun error ->
                           error.Agent_protocol.Error.message))
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
                                 Result.map_error result ~f:(fun error ->
                                   error.Agent_protocol.Error.message))
                           ; install = ignore
                           })
                   }
                 in
                 run binding
                 |> Result.map_error ~f:failure
                 |> Result.map ~f:(fun value -> outcome := Some value)))
          |> Result.map_error ~f:(fun error -> error.Agent_protocol.Error.message)
        in
        match committed, !outcome with
        | true, Some outcome -> Ok outcome
        | false, _ -> Error "legacy callback has no current physical owner"
        | true, None -> Error "legacy callback committed without its outcome")
    |> Result.map_error ~f:failure
  in
  match outcome, !checkpoint with
  | None, _ -> Ok None
  | Some outcome, Some checkpoint -> Ok (Some { outcome; checkpoint })
  | Some _, None -> Error (failure "legacy callback did not acknowledge its checkpoint")
;;
