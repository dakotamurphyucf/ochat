open! Core
module CM = Prompt.Chat_markdown
module Lang = Chatml.Chatml_lang
module Value_codec = Chatml.Chatml_value_codec

type exec_context =
  { ctx : Eio_unix.Stdenv.base Ctx.t
  ; run_agent :
      ?history_compaction:bool
      -> ?prompt_dir:Eio.Fs.dir_ty Eio.Path.t
      -> ?session_id:string
      -> ctx:Eio_unix.Stdenv.base Ctx.t
      -> string
      -> CM.content_item list
      -> string
  ; fetch_prompt :
      ctx:Eio_unix.Stdenv.base Ctx.t
      -> prompt:string
      -> is_local:bool
      -> (string * Eio.Fs.dir_ty Eio.Path.t option, string) result
  }

module Selected_source = struct
  type t =
    { with_selected :
        Model_spawn_request.t
        -> f:
             (config:CM.config
              -> run:
                   (ctx:Eio_unix.Stdenv.base Ctx.t
                    -> input:CM.content_item list
                    -> history_compaction:bool
                    -> session_id:string
                    -> (string, string) result)
              -> (Moderation.Capabilities.model_call_result, string) result)
        -> (Moderation.Capabilities.model_call_result, string) result
    }

  let create ~with_selected = { with_selected }
end

type execution =
  | Ordinary of exec_context
  | Selected of
      { ctx : Eio_unix.Stdenv.base Ctx.t
      ; source : Selected_source.t
      }

type job_terminal =
  | Succeeded of Jsonaf.t
  | Failed of string

type job_phase =
  | Pending
  | Result_ready of job_terminal
  | Completed of job_terminal
  | Escaped of job_terminal option * exn * Stdlib.Printexc.raw_backtrace

type delivery_state =
  | Undelivered
  | Enqueueing
  | Delivered
[@@deriving equal]

type job =
  { session_id : string
  ; recipe_name : string
  ; payload : Jsonaf.t
  ; mutable phase : job_phase
  ; mutable delivery : delivery_state
  ; mutable worker : unit Eio.Promise.or_exn option
  }

type t =
  { sw : Eio.Switch.t
  ; domain_id : int
  ; execution : execution
  ; jobs : job String.Table.t
  ; next_job_id : int ref
  ; max_spawned_jobs : int
  ; sessions : Moderator_manager.t String.Table.t
  ; session_wakeups : (unit -> unit) String.Table.t
  ; changed : Eio.Condition.t
  }

module Session_settlement = struct
  type t =
    { spawned_jobs : int
    ; delivered_events : int
    }
  [@@deriving sexp_of]
end

type job_state =
  [ `Pending
  | `Succeeded of Jsonaf.t
  | `Failed of string
  ]

let agent_prompt_v1_name = Model_spawn_request.recipe_name

let create_with_execution ~sw ~execution ~max_spawned_jobs =
  { sw
  ; domain_id = (Stdlib.Domain.self () :> int)
  ; execution
  ; jobs = String.Table.create ()
  ; next_job_id = ref 1
  ; max_spawned_jobs
  ; sessions = String.Table.create ()
  ; session_wakeups = String.Table.create ()
  ; changed = Eio.Condition.create ()
  }
;;

let create ~sw ~exec_context ?(max_spawned_jobs = 100) () =
  create_with_execution ~sw ~execution:(Ordinary exec_context) ~max_spawned_jobs
;;

let create_selected ~sw ~ctx ~source ?(max_spawned_jobs = 100) () =
  create_with_execution ~sw ~execution:(Selected { ctx; source }) ~max_spawned_jobs
;;

let notify_session_wakeup (t : t) ~(session_id : string) : unit =
  match Hashtbl.find t.session_wakeups session_id with
  | None -> ()
  | Some on_wakeup ->
    (try on_wakeup () with
     | Eio.Cancel.Cancelled _ as exn -> raise exn
     | exn ->
       Log.emit
         `Error
         (Printf.sprintf
            "Model-executor session wakeup failed for %s: %s"
            session_id
            (Exn.to_string exn)))
;;

let unregister_session_wakeup (t : t) ~(session_id : string) : unit =
  Hashtbl.remove t.session_wakeups session_id
;;

let internal_event_succeeded
      ~(job_id : string)
      ~(recipe_name : string)
      ~(result_json : Jsonaf.t)
  : Lang.value
  =
  Lang.VVariant
    ( "Model_job_succeeded"
    , [ Lang.VString job_id
      ; Lang.VString recipe_name
      ; Value_codec.jsonaf_to_value result_json
      ] )
;;

let internal_event_failed ~(job_id : string) ~(recipe_name : string) ~(message : string)
  : Lang.value
  =
  Lang.VVariant
    ( "Model_job_failed"
    , [ Lang.VString job_id; Lang.VString recipe_name; Lang.VString message ] )
;;

let deliver_if_possible (t : t) ~(job_id : string) (job : job) ~(terminal : job_terminal)
  : (unit, string) result
  =
  match job.delivery with
  | Delivered -> Ok ()
  | Enqueueing -> Error "Model job delivery is already in progress."
  | Undelivered ->
    (match Hashtbl.find t.sessions job.session_id with
     | None -> Error "Model job has no registered moderator session."
     | Some manager ->
       let event =
         match terminal with
         | Succeeded json ->
           internal_event_succeeded ~job_id ~recipe_name:job.recipe_name ~result_json:json
         | Failed msg ->
           internal_event_failed ~job_id ~recipe_name:job.recipe_name ~message:msg
       in
       job.delivery <- Enqueueing;
       let enqueued =
         Exn.protect
           ~f:(fun () -> Moderator_manager.enqueue_internal_event manager event)
           ~finally:(fun () ->
             if equal_delivery_state job.delivery Enqueueing
             then job.delivery <- Undelivered)
       in
       (match enqueued with
        | Ok () ->
          job.delivery <- Delivered;
          Eio.Condition.broadcast t.changed;
          notify_session_wakeup t ~session_id:job.session_id;
          Ok ()
        | Error reason -> Error reason))
;;

let register_session
      ?on_wakeup
      (t : t)
      ~(session_id : string)
      ~(manager : Moderator_manager.t)
  : unit
  =
  Hashtbl.set t.sessions ~key:session_id ~data:manager;
  (match on_wakeup with
   | None -> unregister_session_wakeup t ~session_id
   | Some on_wakeup -> Hashtbl.set t.session_wakeups ~key:session_id ~data:on_wakeup);
  (* A busy manager retains the completion for a later strict settlement. *)
  Hashtbl.to_alist t.jobs
  |> List.iter ~f:(fun (job_id, job) ->
    if
      String.equal job.session_id session_id
      && equal_delivery_state job.delivery Undelivered
    then (
      match job.phase with
      | Completed terminal ->
        ignore (deliver_if_possible t ~job_id job ~terminal : (unit, string) result)
      | Pending | Result_ready _ | Escaped _ -> ()))
;;

let agent_prompt_result_json
      ~(prompt : string)
      ~(is_local : bool)
      ~(session_id : string)
      ~(final_text : string)
  : Jsonaf.t
  =
  `Object
    [ "recipe", `String agent_prompt_v1_name
    ; "prompt", `String prompt
    ; ("is_local", if is_local then `True else `False)
    ; "session_id", `String session_id
    ; "final_text", `String final_text
    ; "terminated_normally", `True
    ]
;;

let run_agent_prompt_v1_body
      ~(ctx : Eio_unix.Stdenv.base Ctx.t)
      ~request
      ?inference_context
      ?before_inference_attempt
      ~capture_recipe_target
      ~(config : CM.config)
      ~run
      ()
  =
  let open Result.Let_syntax in
  let prompt = Model_spawn_request.prompt request in
  let is_local = Model_spawn_request.is_local request in
  let input = Model_spawn_request.input request in
  let history_compaction_opt = Model_spawn_request.history_compaction request in
  let nested_session_id_opt = Model_spawn_request.nested_prompt_id request in
  let nested_session_id = Option.value nested_session_id_opt ~default:prompt in
  let ctx =
    match inference_context with
    | None -> ctx
    | Some inference_context -> Ctx.with_inference ctx ~inference_context
  in
  let ctx =
    match before_inference_attempt with
    | None -> ctx
    | Some before_attempt -> Ctx.with_inference_attempt_guard ctx ~before_attempt
  in
  let%bind target =
    Inference_config.apply_overrides
      (Inference_runtime.Context.target ctx.inference_context)
      config
      ~limits:Transcript.Admission.default
    |> Result.map_error ~f:(fun error ->
      Sexp.to_string_hum (Inference_runtime.Preparation_error.sexp_of_t error))
  in
  let%bind () = capture_recipe_target target in
  let%bind inference_context =
    Inference_runtime.Context.derive ctx.inference_context ~target
    |> Result.map_error ~f:(fun error ->
      Sexp.to_string_hum (Inference_runtime.Preparation_error.sexp_of_t error))
  in
  let ctx = Ctx.with_inference ctx ~inference_context in
  let basic_item : CM.basic_content_item =
    { type_ = "text"
    ; text = Some input
    ; image_url = None
    ; document_url = None
    ; is_local = false
    ; cleanup_html = false
    ; markdown = false
    }
  in
  let history_compaction = Option.value history_compaction_opt ~default:false in
  let%map final_text =
    run
      ~ctx
      ~input:[ CM.Basic basic_item ]
      ~history_compaction
      ~session_id:nested_session_id
  in
  Moderation.Capabilities.Model_ok
    (agent_prompt_result_json ~prompt ~is_local ~session_id:nested_session_id ~final_text)
;;

let call_agent_prompt_v1_strict
      (exec_context : exec_context)
      ~request
      ?inference_context
      ?before_inference_attempt
      ~capture_recipe_target
      ()
  =
  match Model_spawn_request.source_mode request with
  | Model_spawn_request.Source_mode.Captured ->
    Error "agent_prompt_v1: captured source requires selected source custody"
  | Model_spawn_request.Source_mode.File ->
    let open Result.Let_syntax in
    let prompt = Model_spawn_request.prompt request in
    let%bind prompt_xml, prompt_dir =
      exec_context.fetch_prompt
        ~ctx:exec_context.ctx
        ~prompt
        ~is_local:(Model_spawn_request.is_local request)
    in
    let elements =
      CM.parse_chat_inputs
        ~source:prompt
        ~dir:(Option.value prompt_dir ~default:(Ctx.dir exec_context.ctx))
        prompt_xml
    in
    run_agent_prompt_v1_body
      ~ctx:exec_context.ctx
      ~request
      ?inference_context
      ?before_inference_attempt
      ~capture_recipe_target
      ~config:(Config.of_elements elements)
      ~run:(fun ~ctx ~input ~history_compaction ~session_id ->
        Ok
          (exec_context.run_agent
             ~history_compaction
             ?prompt_dir
             ~session_id
             ~ctx
             prompt_xml
             input))
      ()
;;

let spawn_job (t : t) ~session_id ~call ~payload =
  if Hashtbl.length t.jobs >= t.max_spawned_jobs
  then Error "Model.spawn: exceeded maximum spawned job limit."
  else (
    let job_id =
      let id = !(t.next_job_id) in
      t.next_job_id := id + 1;
      Printf.sprintf "model-job-%d" id
    in
    let job =
      { session_id
      ; recipe_name = agent_prompt_v1_name
      ; payload
      ; phase = Pending
      ; delivery = Undelivered
      ; worker = None
      }
    in
    (* The immediate check and fork have no intervening yield. A refused
       switch cannot leave a recorded job with no executing worker. *)
    Eio.Switch.check t.sw;
    Hashtbl.set t.jobs ~key:job_id ~data:job;
    let worker () =
      let terminal_result = ref None in
      match
        let terminal =
          match call ~payload with
          | Ok (Moderation.Capabilities.Model_ok json) -> Succeeded json
          | Ok (Moderation.Capabilities.Model_refused msg)
          | Ok (Moderation.Capabilities.Model_error msg)
          | Error msg -> Failed msg
        in
        terminal_result := Some terminal;
        job.phase <- Result_ready terminal;
        ignore (deliver_if_possible t ~job_id job ~terminal : (unit, string) result);
        Eio.Cancel.protect (fun () ->
          job.phase <- Completed terminal;
          Eio.Condition.broadcast t.changed)
      with
      | () -> ()
      | exception exn ->
        let backtrace = Stdlib.Printexc.get_raw_backtrace () in
        Eio.Cancel.protect (fun () ->
          job.phase <- Escaped (!terminal_result, exn, backtrace);
          Eio.Condition.broadcast t.changed);
        Exn.raise_with_original_backtrace exn backtrace
    in
    (match
       Execution_gate.without_context (fun () -> Eio.Fiber.fork_promise ~sw:t.sw worker)
     with
     | promise ->
       job.worker <- Some promise;
       Eio.Condition.broadcast t.changed
     | exception exn ->
       let backtrace = Stdlib.Printexc.get_raw_backtrace () in
       Hashtbl.remove t.jobs job_id;
       Exn.raise_with_original_backtrace exn backtrace);
    Ok job_id)
;;

let recipe_agent_prompt_v1
      (t : t)
      ~(session_id : string)
      ?inference_context
      ?before_inference_attempt
      ?(capture_recipe_target = fun _ -> Ok ())
      ()
  : Moderation.Capabilities.model_recipe
  =
  let initial_ctx =
    match t.execution with
    | Ordinary exec_context -> exec_context.ctx
    | Selected { ctx; source = _ } -> ctx
  in
  let rec recipe_for_ctx ~rebound current_ctx =
    let call ~payload =
      let open Result.Let_syntax in
      let%bind request = Model_spawn_request.decode payload in
      let inference_context = if rebound then None else inference_context in
      let before_inference_attempt = if rebound then None else before_inference_attempt in
      let run () =
        match t.execution with
        | Ordinary exec_context ->
          call_agent_prompt_v1_strict
            exec_context
            ~request
            ?inference_context
            ?before_inference_attempt
            ~capture_recipe_target
            ()
        | Selected { ctx = _; source } ->
          source.with_selected request ~f:(fun ~config ~run ->
            run_agent_prompt_v1_body
              ~ctx:current_ctx
              ~request
              ?inference_context
              ?before_inference_attempt
              ~capture_recipe_target
              ~config
              ~run:(fun ~ctx ~input ~history_compaction ~session_id ->
                let recipe = recipe_for_ctx ~rebound:true ctx in
                run
                  ~ctx:(Ctx.with_agent_prompt_v1_recipe ctx recipe)
                  ~input
                  ~history_compaction
                  ~session_id)
              ())
      in
      try
        match run () with
        | Ok r -> Ok r
        | Error msg -> Ok (Moderation.Capabilities.Model_error msg)
      with
      | Ctx.Inference_admission_rejected ->
        Ok (Moderation.Capabilities.Model_error "model inference admission was revoked")
      | Eio.Cancel.Cancelled _ as exn ->
        let backtrace = Stdlib.Printexc.get_raw_backtrace () in
        Exn.raise_with_original_backtrace exn backtrace
      | exn ->
        let backtrace = Stdlib.Printexc.get_raw_backtrace () in
        (match t.execution with
         | Selected _ -> Exn.raise_with_original_backtrace exn backtrace
         | Ordinary _ -> Ok (Moderation.Capabilities.Model_error (Exn.to_string exn)))
    in
    let spawn ~payload = spawn_job t ~session_id ~call ~payload in
    { Moderation.Capabilities.call; spawn }
  in
  recipe_for_ctx ~rebound:false initial_ctx
;;

let await_job (t : t) ~(job_id : string) : (unit, string) result =
  match Hashtbl.find t.jobs job_id with
  | None -> Error (Printf.sprintf "Unknown model job id %S" job_id)
  | Some job ->
    (match job.worker with
     | None -> Error "Model job is still being admitted."
     | Some worker ->
       (try Ok (Eio.Promise.await_exn worker) with
        | exn -> Error (Exn.to_string exn)))
;;

let audit_joined_session (t : t) ~(session_id : string) ~(allowed_cancelled_reason : exn)
  : (unit, string) result
  =
  if not (Int.equal t.domain_id (Stdlib.Domain.self () :> int))
  then invalid_arg "Model_executor.audit_joined_session requires its owning domain";
  let jobs =
    Hashtbl.to_alist t.jobs
    |> List.filter ~f:(fun (_, job) -> String.equal job.session_id session_id)
  in
  let unexpected =
    List.find_map jobs ~f:(fun (_, job) ->
      match job.phase with
      | Escaped (_, Eio.Cancel.Cancelled reason, _)
        when phys_equal reason allowed_cancelled_reason -> None
      | Escaped (_, exn, backtrace) -> Some (exn, backtrace)
      | Pending | Result_ready _ | Completed _ -> None)
  in
  match unexpected with
  | Some (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
  | None ->
    List.fold jobs ~init:(Ok ()) ~f:(fun result (job_id, job) ->
      let open Result.Let_syntax in
      let%bind () = result in
      match job.phase, job.worker with
      | Pending, _ | Result_ready _, _ ->
        Error (Printf.sprintf "Model job %s is not terminal after switch join." job_id)
      | (Completed _ | Escaped _), None ->
        Error (Printf.sprintf "Model job %s has no worker after switch join." job_id)
      | Completed _, Some worker ->
        (match Eio.Promise.peek worker with
         | Some (Ok ()) -> Ok ()
         | None | Some (Error _) ->
           Error (Printf.sprintf "Model job %s has an inconsistent worker." job_id))
      | Escaped (_, escaped, _), Some worker ->
        (match Eio.Promise.peek worker with
         | Some (Error worker_error) when phys_equal escaped worker_error -> Ok ()
         | None | Some (Ok () | Error _) ->
           Error (Printf.sprintf "Model job %s has an inconsistent worker." job_id)))
;;

let session_job_count (t : t) ~(session_id : string) =
  if not (Int.equal t.domain_id (Stdlib.Domain.self () :> int))
  then invalid_arg "Model_executor.session_job_count requires its owning domain";
  Hashtbl.count t.jobs ~f:(fun job -> String.equal job.session_id session_id)
;;

module Session_progress = struct
  type cursor =
    { owner : t
    ; session_id : string
    ; spawned : int
    ; delivered : int
    }

  type stop =
    | Progressed
    | All_completed
end

let session_progress_counts (t : t) ~(session_id : string) =
  Hashtbl.fold t.jobs ~init:(0, 0) ~f:(fun ~key:_ ~data:job (spawned, delivered) ->
    if String.equal job.session_id session_id
    then
      ( spawned + 1
      , delivered + if equal_delivery_state job.delivery Delivered then 1 else 0 )
    else spawned, delivered)
;;

let session_progress_cursor (t : t) ~(session_id : string) =
  if not (Int.equal t.domain_id (Stdlib.Domain.self () :> int))
  then invalid_arg "Model_executor.session_progress_cursor requires its owning domain";
  let spawned, delivered = session_progress_counts t ~session_id in
  Session_progress.{ owner = t; session_id; spawned; delivered }
;;

let rec await_session_progress (t : t) (cursor : Session_progress.cursor)
  : (Session_progress.stop, string) result
  =
  if not (Int.equal t.domain_id (Stdlib.Domain.self () :> int))
  then invalid_arg "Model_executor.await_session_progress requires its owning domain";
  if not (phys_equal cursor.owner t)
  then Error "Model session progress cursor belongs to another executor."
  else (
    let observation =
      Eio.Condition.loop_no_mutex t.changed (fun () ->
        let selected =
          Hashtbl.to_alist t.jobs
          |> List.filter ~f:(fun (_, job) ->
            String.equal job.session_id cursor.session_id)
        in
        List.iter selected ~f:(fun (_, job) ->
          match job.phase with
          | Escaped (_, exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
          | Pending | Result_ready _ | Completed _ -> ());
        match
          List.find_map selected ~f:(fun (job_id, job) ->
            match job.phase, job.delivery with
            | Completed terminal, Undelivered -> Some (job_id, job, terminal)
            | Pending, _
            | Result_ready _, _
            | Completed _, (Enqueueing | Delivered)
            | Escaped _, _ -> None)
        with
        | Some ready -> Some (`Retry ready)
        | None ->
          let spawned, delivered =
            session_progress_counts t ~session_id:cursor.session_id
          in
          if spawned > cursor.spawned || delivered > cursor.delivered
          then Some `Progressed
          else if
            List.for_all selected ~f:(fun (_, job) ->
              match job.phase with
              | Completed _ -> true
              | Pending | Result_ready _ | Escaped _ -> false)
          then Some `All_completed
          else None)
    in
    match observation with
    | `Progressed -> Ok Progressed
    | `All_completed -> Ok All_completed
    | `Retry (job_id, job, terminal) ->
      (match deliver_if_possible t ~job_id job ~terminal with
       | Error _ as error -> error
       | Ok () -> await_session_progress t cursor))
;;

let rec settle_session (t : t) ~(session_id : string)
  : (Session_settlement.t, string) result
  =
  let jobs () =
    Hashtbl.to_alist t.jobs
    |> List.filter ~f:(fun (_, job) -> String.equal job.session_id session_id)
  in
  let workers =
    Eio.Condition.loop_no_mutex t.changed (fun () ->
      let selected = jobs () in
      List.iter selected ~f:(fun (_, job) ->
        match job.phase with
        | Escaped (_, exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
        | Pending | Result_ready _ | Completed _ -> ());
      let workers =
        List.map selected ~f:(fun (_, job) ->
          match job.worker with
          | Some worker -> worker
          | None -> failwith "Model job has no admitted worker")
      in
      let pre_fork_failure =
        List.find_map workers ~f:(fun worker ->
          match Eio.Promise.peek worker with
          | Some (Error exn) -> Some exn
          | None | Some (Ok ()) -> None)
      in
      Option.iter pre_fork_failure ~f:raise;
      if
        List.for_all selected ~f:(fun (_, job) ->
          match job.phase with
          | Completed _ -> true
          | Pending | Result_ready _ | Escaped _ -> false)
      then Some workers
      else None)
  in
  List.iter workers ~f:Eio.Promise.await_exn;
  let deliver_and_rescan () =
    let selected = jobs () in
    let open Result.Let_syntax in
    let%bind () =
      List.fold selected ~init:(Ok ()) ~f:(fun acc (job_id, job) ->
        let%bind () = acc in
        match job.phase with
        | Completed terminal -> deliver_if_possible t ~job_id job ~terminal
        | Escaped (_, exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
        | Pending | Result_ready _ -> Ok ())
    in
    let current = jobs () in
    List.iter current ~f:(fun (_, job) ->
      match job.phase with
      | Escaped (_, exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
      | Pending | Result_ready _ | Completed _ -> ());
    if
      List.length current <> List.length selected
      || List.exists current ~f:(fun (_, job) ->
        match job.phase with
        | Pending | Result_ready _ -> true
        | Completed _ | Escaped _ -> false)
    then settle_session t ~session_id
    else
      Ok
        { Session_settlement.spawned_jobs = List.length current
        ; delivered_events =
            List.count current ~f:(fun (_, job) ->
              equal_delivery_state job.delivery Delivered)
        }
  in
  deliver_and_rescan ()
;;

let job_state (t : t) ~(job_id : string) : job_state option =
  match Hashtbl.find t.jobs job_id with
  | None -> None
  | Some job ->
    Some
      (match job.phase with
       | Pending -> `Pending
       | Result_ready (Succeeded json) -> `Succeeded json
       | Result_ready (Failed msg) -> `Failed msg
       | Completed (Succeeded json) -> `Succeeded json
       | Completed (Failed msg) -> `Failed msg
       | Escaped (Some (Succeeded json), _, _)
         when equal_delivery_state job.delivery Delivered -> `Succeeded json
       | Escaped (Some (Failed msg), _, _)
         when equal_delivery_state job.delivery Delivered -> `Failed msg
       | Escaped (_, exn, _) -> `Failed (Exn.to_string exn))
;;
