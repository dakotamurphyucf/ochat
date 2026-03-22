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

type job_status =
  | Pending
  | Succeeded of Jsonaf.t
  | Failed of string

type job =
  { session_id : string
  ; recipe_name : string
  ; payload : Jsonaf.t
  ; mutable status : job_status
  ; mutable delivered : bool
  ; completion : unit Eio.Promise.or_exn
  }

type t =
  { sw : Eio.Switch.t
  ; exec_context : exec_context
  ; jobs : job String.Table.t
  ; next_job_id : int ref
  ; max_spawned_jobs : int
  ; sessions : Moderator_manager.t String.Table.t
  }

type job_state =
  [ `Pending
  | `Succeeded of Jsonaf.t
  | `Failed of string
  ]

let agent_prompt_v1_name = "agent_prompt_v1"

let create ~sw ~exec_context ?(max_spawned_jobs = 100) () =
  { sw
  ; exec_context
  ; jobs = String.Table.create ()
  ; next_job_id = ref 1
  ; max_spawned_jobs
  ; sessions = String.Table.create ()
  }
;;

let register_session (t : t) ~(session_id : string) ~(manager : Moderator_manager.t)
  : unit
  =
  Hashtbl.set t.sessions ~key:session_id ~data:manager;
  (* Deliver any already-completed undelivered jobs for this session. *)
  Hashtbl.iteri t.jobs ~f:(fun ~key:job_id ~data:job ->
    if String.equal job.session_id session_id && not job.delivered
    then (
      let event =
        match job.status with
        | Pending -> None
        | Succeeded json ->
          Some
            (Lang.VVariant
               ( "Model_job_succeeded"
               , [ Lang.VString job_id
                 ; Lang.VString job.recipe_name
                 ; Value_codec.jsonaf_to_value json
                 ] ))
        | Failed msg ->
          Some
            (Lang.VVariant
               ( "Model_job_failed"
               , [ Lang.VString job_id; Lang.VString job.recipe_name; Lang.VString msg ]
               ))
      in
      match event with
      | None -> ()
      | Some event ->
        (match Moderator_manager.enqueue_internal_event manager event with
         | Ok () -> job.delivered <- true
         | Error _ -> ())))
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

let deliver_if_possible (t : t) ~(job_id : string) (job : job) : unit =
  if job.delivered
  then ()
  else (
    match Hashtbl.find t.sessions job.session_id with
    | None -> ()
    | Some manager ->
      let event =
        match job.status with
        | Pending -> None
        | Succeeded json ->
          Some
            (internal_event_succeeded
               ~job_id
               ~recipe_name:job.recipe_name
               ~result_json:json)
        | Failed msg ->
          Some (internal_event_failed ~job_id ~recipe_name:job.recipe_name ~message:msg)
      in
      (match event with
       | None -> ()
       | Some event ->
         (match Moderator_manager.enqueue_internal_event manager event with
          | Ok () -> job.delivered <- true
          | Error _ -> ())))
;;

let json_object_find (fields : (string * Jsonaf.t) list) (key : string) : Jsonaf.t option =
  List.find_map fields ~f:(fun (k, v) -> if String.equal k key then Some v else None)
;;

let expect_string_field ~name fields key : (string, string) result =
  match json_object_find fields key with
  | Some (`String s) -> Ok s
  | Some _ -> Error (Printf.sprintf "%s: field %S must be a string" name key)
  | None -> Error (Printf.sprintf "%s: missing field %S" name key)
;;

let expect_bool_field_opt fields key : (bool option, string) result =
  match json_object_find fields key with
  | None -> Ok None
  | Some `True -> Ok (Some true)
  | Some `False -> Ok (Some false)
  | Some _ -> Error (Printf.sprintf "field %S must be a bool when present" key)
;;

let expect_string_field_opt fields key : (string option, string) result =
  match json_object_find fields key with
  | None -> Ok None
  | Some (`String s) -> Ok (Some s)
  | Some _ -> Error (Printf.sprintf "field %S must be a string when present" key)
;;

let decode_agent_prompt_payload (payload : Jsonaf.t)
  : (string * bool * string * bool option * string option, string) result
  =
  match payload with
  | `Object fields ->
    let open Result.Let_syntax in
    let%bind prompt = expect_string_field ~name:agent_prompt_v1_name fields "prompt" in
    let%bind input = expect_string_field ~name:agent_prompt_v1_name fields "input" in
    let%bind is_local =
      match json_object_find fields "is_local" with
      | Some `True -> Ok true
      | Some `False -> Ok false
      | Some _ ->
        Error
          (Printf.sprintf "%s: field %S must be a bool" agent_prompt_v1_name "is_local")
      | None -> Ok false
    in
    let%bind history_compaction = expect_bool_field_opt fields "history_compaction" in
    let%bind session_id = expect_string_field_opt fields "session_id" in
    Ok (prompt, is_local, input, history_compaction, session_id)
  | _ -> Error (Printf.sprintf "%s: payload must be an object" agent_prompt_v1_name)
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

let recipe_agent_prompt_v1 (t : t) ~(session_id : string)
  : Moderation.Capabilities.model_recipe
  =
  let call ~payload =
    let open Result.Let_syntax in
    let%bind prompt, is_local, input, history_compaction_opt, nested_session_id_opt =
      decode_agent_prompt_payload payload
    in
    let nested_session_id = Option.value nested_session_id_opt ~default:prompt in
    let job () =
      let%bind prompt_xml, prompt_dir =
        t.exec_context.fetch_prompt ~ctx:t.exec_context.ctx ~prompt ~is_local
      in
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
      let final_text =
        t.exec_context.run_agent
          ~history_compaction
          ?prompt_dir
          ~session_id:nested_session_id
          ~ctx:t.exec_context.ctx
          prompt_xml
          [ CM.Basic basic_item ]
      in
      Ok
        (Moderation.Capabilities.Model_ok
           (agent_prompt_result_json
              ~prompt
              ~is_local
              ~session_id:nested_session_id
              ~final_text))
    in
    try
      match job () with
      | Ok r -> Ok r
      | Error msg -> Ok (Moderation.Capabilities.Model_error msg)
    with
    | exn -> Ok (Moderation.Capabilities.Model_error (Exn.to_string exn))
  in
  let spawn ~payload =
    if Hashtbl.length t.jobs >= t.max_spawned_jobs
    then Error "Model.spawn: exceeded maximum spawned job limit."
    else (
      let job_id =
        let id = !(t.next_job_id) in
        t.next_job_id := id + 1;
        Printf.sprintf "model-job-%d" id
      in
      let completion_promise, completion_resolver = Eio.Promise.create () in
      let job =
        { session_id
        ; recipe_name = agent_prompt_v1_name
        ; payload
        ; status = Pending
        ; delivered = false
        ; completion = completion_promise
        }
      in
      Hashtbl.set t.jobs ~key:job_id ~data:job;
      let _promise =
        Eio.Fiber.fork_promise ~sw:t.sw (fun () ->
          (match call ~payload with
           | Ok (Moderation.Capabilities.Model_ok json) ->
             job.status <- Succeeded json;
             deliver_if_possible t ~job_id job;
             Eio.Promise.resolve_ok completion_resolver ();
             Ok ()
           | Ok (Moderation.Capabilities.Model_refused msg) ->
             job.status <- Failed msg;
             deliver_if_possible t ~job_id job;
             Eio.Promise.resolve_ok completion_resolver ();
             Ok ()
           | Ok (Moderation.Capabilities.Model_error msg) ->
             job.status <- Failed msg;
             deliver_if_possible t ~job_id job;
             Eio.Promise.resolve_ok completion_resolver ();
             Ok ()
           | Error msg ->
             job.status <- Failed msg;
             deliver_if_possible t ~job_id job;
             Eio.Promise.resolve_ok completion_resolver ();
             Ok ())
          |> fun r ->
          match r with
          | Ok _ -> Ok ()
          | Error _ -> Ok ())
      in
      Ok job_id)
  in
  { Moderation.Capabilities.call; spawn }
;;

let await_job (t : t) ~(job_id : string) : (unit, string) result =
  match Hashtbl.find t.jobs job_id with
  | None -> Error (Printf.sprintf "Unknown model job id %S" job_id)
  | Some job ->
    (try Ok (Eio.Promise.await_exn job.completion) with
     | exn -> Error (Exn.to_string exn))
;;

let job_state (t : t) ~(job_id : string) : job_state option =
  match Hashtbl.find t.jobs job_id with
  | None -> None
  | Some job ->
    Some
      (match job.status with
       | Pending -> `Pending
       | Succeeded json -> `Succeeded json
       | Failed msg -> `Failed msg)
;;
