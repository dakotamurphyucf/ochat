open! Core
module C = Agent_protocol.Session_configuration

module Token = struct
  type t =
    { identity : unit ref
    ; operation_id : Agent_protocol.Id.Operation.t
    ; revision : int64
    ; generation : int
    ; target : Inference.Request.Target.t
    ; policy : Configuration_policy.t
    ; configuration : Inference.Observation.Configuration.t option
    }

  let operation_id t = t.operation_id
end

type t = { mutable current : (Token.t * C.phase) option }

let create () = { current = None }

let begin_capture t ~operation_id ~generation ~revision ~target ~policy =
  let token : Token.t =
    { identity = ref ()
    ; operation_id
    ; generation
    ; revision
    ; target
    ; policy
    ; configuration = None
    }
  in
  t.current <- Some (token, C.Preparing);
  token
;;

let owns t token =
  Option.exists t.current ~f:(fun (current, _) ->
    phys_equal current.Token.identity token.Token.identity)
;;

let mark t token configuration =
  if owns t token
  then (
    t.current <- Some ({ token with configuration = Some configuration }, C.Effective);
    Ok ())
  else
    Error
      (Agent_protocol.Error.create
         Conflict
         ~message:"root capture no longer owns preparation"
         ~retryable:false
         ())
;;

let finish t token ~success =
  if owns t token
  then
    if success
    then t.current <- Option.map t.current ~f:(fun (current, _) -> current, C.Retained)
    else t.current <- None
;;

let view t ~generation ~revision ~selected =
  let open Result.Let_syntax in
  let selection = selected in
  let%bind selected =
    match Inference.Selection.view selection with
    | Unresolved -> Ok None
    | Captured target ->
      Result.map (Configuration_transition.safe_view target) ~f:Option.some
  in
  let retained =
    Option.filter t.current ~f:(fun (token, _) ->
      Int.equal token.Token.generation generation)
  in
  let%map capture =
    match retained with
    | None -> Ok None
    | Some (token, phase) ->
      let%map configuration =
        match token.configuration with
        | Some configuration -> Ok configuration
        | None -> Configuration_transition.safe_view token.target
      in
      Some
        C.
          { operation_id = token.operation_id
          ; revision = token.revision
          ; phase
          ; configuration
          }
  in
  C.
    { revision
    ; selected
    ; capture
    ; pending =
        Option.exists retained ~f:(fun (token, _) ->
          match Inference.Selection.view selection with
          | Unresolved -> true
          | Captured selected ->
            not (Inference.Request.Target.equal token.target selected))
    }
;;

let require = function
  | Ok value -> value
  | Error error -> raise (Native_tool_dispatch.Dispatch_error error)
;;

let root_port ~begin_capture ~mark ~finish =
  Chat_response.Root_context.
    { with_context =
        (fun ~previous ~history f ->
          let token : Token.t = begin_capture () |> require in
          let complete success =
            Eio.Cancel.protect (fun () -> finish token ~success |> require)
          in
          match
            let context = token.policy.resolve token.target |> require in
            let context = Inference_runtime.Context.reuse_unchanged context ~previous in
            Inference_runtime.Context.preflight_history context history
            |> Result.map_error ~f:(fun _ ->
              Agent_protocol.Error.invalid_request
                "captured target cannot retain current history")
            |> require;
            f context ~on_dispatch:(fun configuration ->
              mark token configuration |> require)
          with
          | result ->
            complete true;
            result
          | exception exn ->
            let backtrace = Stdlib.Printexc.get_raw_backtrace () in
            (* Cleanup cannot replace the request's original failure/cancellation. *)
            (try complete false with
             | _ -> ());
            Stdlib.Printexc.raise_with_backtrace exn backtrace)
    }
;;
