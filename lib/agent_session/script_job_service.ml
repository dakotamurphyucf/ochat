open Core
module P = Agent_protocol
module B = Chat_response.Background_request
module C = Chat_response.Tool_capability
module Ops = Chat_response.Background_job_operations
module Kernel = Script_job_kernel

type host =
  { stage : P.Job.launch_owner -> B.t -> (P.Job.t, P.Error.t) result
  ; select : P.Job.launch_owner -> P.Id.Job.t list -> (unit, P.Error.t) result
  ; abort : P.Job.launch_owner -> P.Id.Job.t -> unit
  ; get : P.Job.launch_owner -> P.Id.Job.t -> (P.Job.t, P.Error.t) result
  ; materialize : P.Job.launch_owner -> P.Job.t -> (P.Completion.t, P.Error.t) result
  ; cancel : P.Job.launch_owner -> P.Id.Job.t -> (unit, P.Error.t) result
  }

type t =
  { env : Eio_unix.Stdenv.base
  ; policy : Chat_response.One_off_request.policy
  ; current_capabilities : unit -> C.t
  ; prepare_source :
      binding:C.binding
      -> owner:P.Job.launch_owner
      -> request:B.t
      -> checkpoint:(unit -> unit)
      -> (B.Agent_artifact.t option, P.Error.t) Result.t
  ; host : host
  }

type scope =
  { service : t
  ; owner : P.Job.launch_owner
  ; selected : C.t
  ; kernel : Kernel.t
  }

let create ~env ~policy ~current_capabilities ~prepare_source ~host =
  { env; policy; current_capabilities; prepare_source; host }
;;

let with_current_capabilities t current_capabilities = { t with current_capabilities }
let message result = Result.map_error result ~f:(fun error -> error.P.Error.message)

let view (job : P.Job.t) =
  let open Result.Let_syntax in
  let%map completion = P.Job.terminal_result job in
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
    ; "progress", Option.value_map job.progress ~default:`Null ~f:P.Job_progress.to_json
    ; "attempt", `Number (Int.to_string job.attempt)
    ; "created_at", P.Timestamp.to_json job.created_at
    ; ( "completed_at"
      , Option.value_map job.completed_at ~default:`Null ~f:P.Timestamp.to_json )
    ; ( "completion"
      , Option.value_map completion ~default:`Null ~f:P.Stored_completion.to_json )
    ]
;;

let check_selection scope = Kernel.check_selection scope.kernel
let check scope = Kernel.check_open scope.kernel
let abort scope id = Kernel.rollback_start scope.kernel id

let with_scope service ~owner ~selected ~error f =
  Kernel.with_scope
    ~selected
    ~current_capabilities:service.current_capabilities
    ~abort:(service.host.abort owner)
    ~error
    (fun kernel -> f { service; owner; selected; kernel })
;;

let stage scope request =
  let open Result.Let_syntax in
  let%bind () = check scope in
  Eio.Cancel.protect (fun () ->
    let%bind job = scope.service.host.stage scope.owner request |> message in
    let recorded =
      try `Returned (Kernel.record_start scope.kernel job.id) with
      | exn -> `Raised (exn, Stdlib.Printexc.get_raw_backtrace ())
    in
    match recorded with
    | `Returned (Ok ()) -> Ok job.id
    | `Returned (Error error) ->
      scope.service.host.abort scope.owner job.id;
      Error error
    | `Raised (exn, backtrace) ->
      (try scope.service.host.abort scope.owner job.id with
       | _ -> ());
      Stdlib.Printexc.raise_with_backtrace exn backtrace)
;;

let accessible_job ~check_scope scope id =
  let open Result.Let_syntax in
  let%bind () = check_scope scope in
  let%bind job = scope.service.host.get scope.owner id |> message in
  let%bind () = check_scope scope in
  let%bind () =
    match job.kind with
    | P.Job.Async_tool -> Ok ()
    | _ -> Error "job is not owned generic script work"
  in
  let%bind request = B.of_json ~policy:scope.service.policy job.payload |> message in
  let%map () = B.validate_capabilities request ~capabilities:scope.selected |> message in
  job
;;

let validate_notification_access scope id =
  accessible_job ~check_scope:check_selection scope id |> Result.map ~f:ignore
;;

let handlers scope =
  let open Result.Let_syntax in
  let accessible_job id = accessible_job ~check_scope:check scope id in
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
          let checkpoint () = Kernel.checkpoint scope.kernel in
          checkpoint ();
          let%bind artifact =
            scope.service.prepare_source ~binding ~owner:scope.owner ~request ~checkpoint
            |> message
          in
          checkpoint ();
          let%bind () = check scope in
          let%bind request =
            match artifact with
            | None -> Ok request
            | Some artifact -> B.with_agent_artifact request artifact |> message
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
    ; read_result =
        (fun id ->
          let%bind job = accessible_job id in
          let%bind stored = P.Job.terminal_result job |> message in
          match stored with
          | None -> Ok `Null
          | Some (Inline completion) -> Ok (P.Completion.to_json completion)
          | Some (Artifact _) ->
            let%bind completion =
              scope.service.host.materialize scope.owner job |> message
            in
            let%map () = check scope in
            P.Completion.to_json completion)
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
  Kernel.install ?control scope.kernel ~handlers:(handlers scope) config
;;

let moderator_transaction scope : Ops.transaction =
  { handlers = handlers scope
  ; prepare =
      (fun ids ->
        let open Result.Let_syntax in
        let%bind () = check scope in
        let%bind () = scope.service.host.select scope.owner ids |> message in
        Eio.Fiber.yield ();
        Kernel.mark_prepared scope.kernel)
  }
;;

let select scope effects =
  let open Result.Let_syntax in
  let%bind ids, ordinary = Kernel.split_starts scope.kernel effects in
  let%map () = scope.service.host.select scope.owner ids |> message in
  ordinary
;;

let validate_work scope work = Kernel.validate_work scope.kernel work
