open Core
module P = Agent_protocol
module B = Chat_response.Background_request
module C = Chat_response.Tool_capability
module Ops = Chat_response.Background_job_operations

type host =
  { stage : P.Job.launch_owner -> B.t -> (P.Job.t, P.Error.t) result
  ; select : P.Job.launch_owner -> P.Id.Job.t list -> (unit, P.Error.t) result
  ; abort : P.Job.launch_owner -> P.Id.Job.t -> unit
  ; get : P.Job.launch_owner -> P.Id.Job.t -> (P.Job.t, P.Error.t) result
  ; cancel : P.Job.launch_owner -> P.Id.Job.t -> (unit, P.Error.t) result
  }

type t =
  { env : Eio_unix.Stdenv.base
  ; policy : Chat_response.One_off_request.policy
  ; current_capabilities : unit -> C.t
  ; host : host
  }

type scope =
  { service : t
  ; owner : P.Job.launch_owner
  ; selected : C.t
  ; active : bool Atomic.t
  ; mutable issued : P.Id.Job.t list
  ; mutable control : Chatml.Chatml_lang.execution_control option
  ; mutable commit_state : commit_state
  }

and commit_state =
  | Open
  | Prepared
  | Committed

let create ~env ~policy ~current_capabilities ~host =
  { env; policy; current_capabilities; host }
;;

let message result = Result.map_error result ~f:(fun error -> error.P.Error.message)

let view (job : P.Job.t) =
  let open Result.Let_syntax in
  let%map completion = P.Job.terminal_completion job in
  let status =
    match job.status with
    | Queued -> "queued"
    | Running -> "running"
    | Waiting_permission _ -> "waiting_permission"
    | Waiting_completion _ -> "waiting_completion"
    | Succeeded -> "succeeded"
    | Failed _ -> "failed"
    | Cancelled -> "cancelled"
    | Interrupted _ -> "interrupted"
  in
  `Object
    [ "version", `Number "1"
    ; "id", P.Id.Job.to_json job.id
    ; "status", `String status
    ; "attempt", `Number (Int.to_string job.attempt)
    ; "created_at", P.Timestamp.to_json job.created_at
    ; ( "completed_at"
      , Option.value_map job.completed_at ~default:`Null ~f:P.Timestamp.to_json )
    ; "completion", Option.value_map completion ~default:`Null ~f:P.Completion.to_json
    ]
;;

let check scope =
  match Atomic.get scope.active, scope.commit_state with
  | false, _ | true, (Prepared | Committed) -> Error "background script scope has ended"
  | true, Open ->
    let open Result.Let_syntax in
    let names =
      List.map (C.references scope.selected) ~f:(fun reference -> reference.C.name)
    in
    let%bind current =
      C.select (scope.service.current_capabilities ()) ~names
      |> Result.map_error ~f:(fun error -> error.C.message)
    in
    (match String.equal (C.fingerprint current) (C.fingerprint scope.selected) with
     | true -> Ok ()
     | false -> Error "background tool selection changed")
;;

let abort scope id =
  scope.issued <- List.filter scope.issued ~f:(fun other -> not (P.Id.Job.equal id other));
  Eio.Cancel.protect (fun () -> scope.service.host.abort scope.owner id)
;;

let abort_all scope = List.iter scope.issued ~f:(abort scope)

let with_scope service ~owner ~selected ~error f =
  let scope =
    { service
    ; owner
    ; selected
    ; active = Atomic.make true
    ; issued = []
    ; control = None
    ; commit_state = Open
    }
  in
  Exn.protect
    ~finally:(fun () -> Atomic.set scope.active false)
    ~f:(fun () ->
      let result =
        try
          Result.bind
            (Result.map_error (check scope) ~f:error)
            ~f:(fun () ->
              let open Result.Let_syntax in
              let%bind value = f scope in
              let%map () =
                match scope.commit_state with
                | Committed -> Ok ()
                | Prepared -> Error (error "background commit was not acknowledged")
                | Open ->
                  Eio.Fiber.yield ();
                  Option.iter scope.control ~f:(fun control -> control.checkpoint ());
                  check scope |> Result.map_error ~f:error
              in
              value)
        with
        | exn ->
          let backtrace = Stdlib.Printexc.get_raw_backtrace () in
          abort_all scope;
          Stdlib.Printexc.raise_with_backtrace exn backtrace
      in
      match result with
      | Ok _ -> result
      | Error _ ->
        abort_all scope;
        result)
;;

let stage scope request =
  let open Result.Let_syntax in
  let%bind () = check scope in
  Eio.Cancel.protect (fun () ->
    let%map job = scope.service.host.stage scope.owner request |> message in
    scope.issued <- job.id :: scope.issued;
    job.id)
;;

let handlers scope =
  let open Result.Let_syntax in
  let accessible_job id =
    let%bind () = check scope in
    let%bind job = scope.service.host.get scope.owner id |> message in
    let%bind () = check scope in
    let%bind () =
      match job.kind with
      | P.Job.Async_tool -> Ok ()
      | _ -> Error "job is not owned generic script work"
    in
    let%bind request = B.of_json ~policy:scope.service.policy job.payload |> message in
    let%map () =
      B.validate_capabilities request ~capabilities:scope.selected |> message
    in
    job
  in
  let handlers : Ops.handlers =
    { start_tool =
        (fun ~name ~input ->
          let%bind () = check scope in
          let%bind binding =
            C.find scope.selected ~name
            |> Result.map_error ~f:(fun error -> error.C.message)
          in
          let%bind request =
            B.tool
              ~capabilities:scope.selected
              ~reference:(C.reference binding)
              ~input
              ~policy:scope.service.policy
            |> message
          in
          stage scope request)
    ; start_script =
        (fun json ->
          let%bind () = check scope in
          let diagnostics errors =
            List.map errors ~f:Chatmd_shell_spec.Diagnostic.to_string
            |> String.concat ~sep:"\n"
          in
          let%bind request =
            Chat_response.One_off_request.decode ~policy:scope.service.policy json
            |> Result.map_error ~f:diagnostics
          in
          let%bind prepared =
            Chat_response.One_off_script.prepare_in_domain
              ~env:scope.service.env
              ~capabilities:scope.selected
              ~tools:request.tools
              ~source:request.source
              ~limits:request.policy.compilation
              ()
            |> Result.map_error ~f:diagnostics
          in
          let%bind request =
            B.script ~prepared ~input:request.input ~policy:request.policy |> message
          in
          stage scope request)
    ; get =
        (fun id ->
          let%bind job = accessible_job id in
          view job |> message)
    ; cancel =
        (fun id ->
          let%bind _ = accessible_job id in
          scope.service.host.cancel scope.owner id |> message)
    ; rollback_start = abort scope
    }
  in
  handlers
;;

let install ?control scope config =
  scope.control <- control;
  Ops.install ?control ~handlers:(handlers scope) config
;;

let moderator_transaction scope : Ops.transaction =
  { handlers = handlers scope
  ; prepare =
      (fun ids ->
        let open Result.Let_syntax in
        let%bind () = check scope in
        let%bind () = scope.service.host.select scope.owner ids |> message in
        Eio.Fiber.yield ();
        let%map () = check scope in
        scope.commit_state <- Prepared;
        fun () ->
          scope.commit_state <- Committed;
          scope.issued <- [])
  }
;;

let select scope effects =
  let open Result.Let_syntax in
  let%bind () = check scope in
  let%bind ids, ordinary = Ops.split_starts effects in
  let%map () = scope.service.host.select scope.owner ids |> message in
  ordinary
;;

let validate_work scope = function
  | P.Invocation.Subscription _ -> Error "subscription service is not installed"
  | Job id ->
    let open Result.Let_syntax in
    let%bind () = check scope in
    (match List.mem scope.issued id ~equal:P.Id.Job.equal with
     | true -> Ok ()
     | false -> Error "pending job was not started by this invocation")
;;
