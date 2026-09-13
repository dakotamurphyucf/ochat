open Core
module P = Agent_protocol
module Embedded = Agent_server.Embedded

exception Protocol_error of P.Error.t [@@deriving sexp]
exception Scenario_failure of string [@@deriving sexp]

type session =
  { connection : Agent_client.Connection.t
  ; session_id : P.Id.Session.t
  ; attachment : P.Session.Attachment.t
  ; replay_job_delivery : (P.Job.t -> unit) option
  }

let session_id session = session.session_id
let attachment session = session.attachment

let require condition message =
  match condition with
  | true -> ()
  | false -> raise (Scenario_failure message)
;;

let get = function
  | Ok value -> value
  | Error error -> raise (Protocol_error error)
;;

(* Construct the actual native registrations from a fixed evaluator-owned root.
   This host is for reference/validation metadata only: no candidate definitions,
   tool calls or model requests are run here. Its scope owns every resource. *)
let with_capabilities ~env ~declarations ~files f =
  let module R = Chat_response.Agent_runtime in
  Mirage_crypto_rng_unix.use_default ();
  let root = Agent_server_test_support.temporary_root env in
  Exn.protect
    ~finally:(fun () ->
      Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
    ~f:(fun () ->
      let root = Eio.Path.(Eio.Stdenv.fs env / root) in
      let workspace = Eio.Path.(root / "workspace") in
      Eio.Path.mkdir ~perm:0o700 workspace;
      List.iter files ~f:(fun (name, source) ->
        Eio.Path.save ~create:(`Exclusive 0o600) Eio.Path.(root / name) source);
      let prompt_elements =
        Prompt.Chat_markdown.parse_chat_inputs
          ~source:"agent.chatmd"
          ~dir:root
          declarations
      in
      let runtime_result = function
        | Ok value -> value
        | Error diagnostics ->
          List.map diagnostics ~f:R.diagnostic_to_string
          |> String.concat ~sep:"\n"
          |> failwith
      in
      let ctx =
        Chat_response.Ctx.create
          ~env
          ~dir:root
          ~tool_dir:root
          ~cache:(Chat_response.Cache.create ~max_size:1 ())
      in
      let host =
        R.host
          ~env
          ~workspace
          ~tool_dir:root
          ~prompt_dir:root
          ~session_dir:root
          ~cache_dir:root
          ~home:root
          ~session_id:"evaluation-metadata"
          ~prompt_elements
        |> runtime_result
      in
      Eio.Switch.run (fun sw ->
        let runtime =
          R.create
            ~sw
            ~ctx
            ~host
            ~platform:(R.platform ())
            ~prompt_elements
            ~manifest_authorizer:Shell_runtime.Manifest_authorizer.assume_authorized
            ~approval_provider:Shell_runtime.Approval_broker.Auto_deny
            ~approval_store:(Shell_access.Approval.create_store ())
            ~run_agent:
              (fun
                ?prompt_dir:_ ?session_id:_ ?observer:_ ~source:_ ~ctx:_ _ _ ->
              failwith "metadata construction attempted to run an agent")
            ()
          |> runtime_result
        in
        let capabilities =
          Lazy.force runtime.capabilities
          |> Result.map_error ~f:(fun error ->
            error.Chat_response.Tool_capability.message)
          |> Result.ok_or_failwith
        in
        f capabilities))
;;

let call_events calls =
  List.concat_mapi calls ~f:(fun index (id, name, arguments) ->
    let open Openai.Responses.Response_stream in
    [ Output_item_added
        { item =
            Function_call
              { name
              ; arguments = ""
              ; call_id = id
              ; _type = "function_call"
              ; id = Some id
              ; status = None
              }
        ; output_index = index
        ; type_ = "response.output_item.added"
        }
    ; Function_call_arguments_done
        { arguments = Jsonaf.to_string arguments
        ; item_id = id
        ; output_index = index
        ; type_ = "response.function_call_arguments.done"
        }
    ])
  |> Stdlib.List.to_seq
;;

let request embedded command =
  Agent_client.Connection.request embedded.connection command |> get
;;

let snapshot embedded =
  match
    request embedded (Session_get { session_id = session_id embedded; history = None })
  with
  | Session_get snapshot -> snapshot
  | _ -> failwith "evaluation host received another protocol response"
;;

type background =
  { after_ack : workspace:string -> session -> P.Snapshot.t -> unit
  ; settled : P.Snapshot.t -> bool
  ; final_requests : int
  }

(* Candidate sources execute in an actual embedded session. Callers supply only
   host-approved file names and root declarations; candidate bindings must pass
   their scenario's captured-source admission before reaching this helper.
   Calls traverse provider serialization, invocation admission and tool dispatch.
   The scripted provider is local and never opens a model connection. *)
let with_session
      ?audit
      ?(durable = false)
      ?(replay_job_delivery = false)
      ~env
      ~sources
      ~workspace_files
      ~post_stream
      f
  =
  Mirage_crypto_rng_unix.use_default ();
  let root = Agent_server_test_support.temporary_root env in
  let sentinel = "PRIVATE-EVALUATION-" ^ P.Id.Session.(create () |> to_string) in
  let sources = sources @ [ "private.json", sentinel ] in
  let post_stream ~sw ~inputs =
    Option.iter audit ~f:(fun audit ->
      List.iter inputs ~f:(fun input ->
        Openai.Responses.Item.jsonaf_of_t input
        |> Jsonaf.to_string
        |> Execution_audit.text
             audit
             ~check:"provider-input:no-private-file-content"
             ~sentinel));
    post_stream ~sw ~inputs
  in
  Exn.protect
    ~finally:(fun () ->
      Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
    ~f:(fun () ->
      let workspace = Filename.concat root "workspace" in
      Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
      let save base (name, source) =
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / base / name)
          source
      in
      List.iter sources ~f:(save root);
      List.iter workspace_files ~f:(save workspace);
      let observed session =
        Exn.protect
          ~f:(fun () -> f ~workspace session)
          ~finally:(fun () ->
            Option.iter audit ~f:(fun audit ->
              Eio.Cancel.protect (fun () ->
                Execution_audit.files
                  audit
                  ~scope:"source"
                  ~dir:Eio.Path.(Eio.Stdenv.fs env / root)
                  sources;
                Execution_audit.files
                  audit
                  ~scope:"workspace-input"
                  ~dir:Eio.Path.(Eio.Stdenv.fs env / workspace)
                  workspace_files;
                match
                  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 2. (fun () ->
                    snapshot session)
                with
                | value ->
                  P.Snapshot.to_json value
                  |> Jsonaf.to_string
                  |> Execution_audit.text
                       audit
                       ~check:"snapshot:no-private-file-content"
                       ~sentinel
                | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
                | exception _ -> ())))
      in
      Eio.Switch.run (fun sw ->
        match replay_job_delivery with
        | true ->
          let module D = Agent_server.Daemon in
          let module Support = Agent_server_test_support in
          let daemon =
            D.start
              ~sw
              ~env
              ~config:
                (Support.config root workspace (Filename.concat root "agent.chatmd"))
              ~tool_dir:root
              ~home:root
              ~process_start_identity:None
              ~options:
                { D.default_options with
                  qualify_chatml_extensions = true
                ; model_post_stream = Some post_stream
                }
              ()
            |> get
          in
          Exn.protect
            ~finally:(fun () -> D.shutdown daemon |> get)
            ~f:(fun () ->
              let connection = Support.connection daemon (Support.principal ()) in
              Exn.protect
                ~finally:(fun () -> Agent_client.Connection.close connection)
                ~f:(fun () ->
                  Support.initialize connection;
                  let created, attachment =
                    Support.create_session ~start_immediately:true connection
                  in
                  let entry =
                    Agent_server.Session_registry.find (D.registry daemon) created.id
                    |> Option.value_exn
                  in
                  let replay (job : P.Job.t) =
                    let checkpoint () =
                      Agent_server.Runtime_owner.with_background_runtime
                        entry.runtime
                        (fun runtime ->
                           let manager =
                             Option.value_exn
                               runtime.Agent_session.Runtime_builder.moderator_manager
                           in
                           Chat_response.Moderator_manager.identity_snapshot manager
                           |> Result.map
                                ~f:Agent_session.Runtime_builder.encode_moderator_snapshot
                           |> Result.map_error ~f:P.Error.invalid_request)
                      |> get
                    in
                    let before = checkpoint () in
                    List.iter
                      [ job; { job with delivery = Pending } ]
                      ~f:(fun retry ->
                        (match
                           ( retry.delivery
                           , Agent_server.Runtime_owner.deliver_background_job_completion
                               entry.runtime
                               retry )
                         with
                         | Delivered _, Error { code = Already_resolved; _ } -> ()
                         | ( Pending
                           , Error
                               { code = Conflict
                               ; message = "background delivery job changed"
                               ; _
                               } ) -> ()
                         | _, Error error -> raise (Protocol_error error)
                         | _, Ok () ->
                           raise
                             (Scenario_failure
                                "host accepted an already acknowledged completion"));
                        require
                          (Jsonaf.exactly_equal before (checkpoint ()))
                          "rejected completion replay changed the moderator checkpoint")
                  in
                  observed
                    { connection
                    ; session_id = created.id
                    ; attachment
                    ; replay_job_delivery = Some replay
                    }))
        | false ->
          let embedded =
            Embedded.start
              ~sw
              ~env
              ~daemon_options:
                { Agent_server.Daemon.default_options with
                  qualify_chatml_extensions = true
                ; model_post_stream = Some post_stream
                }
              { prompt_file = Filename.concat root "agent.chatmd"
              ; workspace
              ; tool_dir = root
              ; home = root
              ; data_root =
                  (match durable with
                   | false -> None
                   | true -> Some (Filename.concat root "data"))
              ; start_immediately = true
              ; permission_profile =
                  { Embedded.default_permission_profile with
                    tool_default = Allow
                  ; manifest_authorization = Assume_authorized
                  }
              ; attachment_mode = Read_write
              ; event_capacity = 512
              }
            |> get
          in
          Exn.protect
            ~finally:(fun () -> Embedded.close embedded)
            ~f:(fun () ->
              observed
                { connection = Embedded.connection embedded
                ; session_id = Embedded.session_id embedded
                ; attachment = Embedded.attachment embedded
                ; replay_job_delivery = None
                })))
;;

let run
      ?audit
      ?(sequential = false)
      ?background
      ?replay_job_delivery
      ~env
      ~sources
      ~workspace_files
      ~calls
      ()
  =
  let batches =
    match sequential with
    | false -> [ calls ]
    | true -> List.map calls ~f:(fun call -> [ call ])
  in
  let expected_requests = List.length batches + 1 in
  let requests = ref 0 in
  let post_stream ~sw:_ ~inputs:_ =
    incr requests;
    match List.nth batches (!requests - 1) with
    | Some calls -> call_events calls
    | None -> Stdlib.Seq.empty
  in
  with_session
    ?audit
    ?replay_job_delivery
    ~env
    ~sources
    ~workspace_files
    ~post_stream
    (fun ~workspace embedded ->
       ignore
         (request
            embedded
            (Session_send_message
               { session_id = session_id embedded
               ; attachment_id = (attachment embedded).id
               ; content =
                   { kind = Plain_text
                   ; text = "Execute the evaluation calls."
                   ; attachments = []
                   }
               ; idempotency_key = P.Idempotency_key.of_string "evaluation:execute" |> get
               })
          : P.Method_result.t);
       Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
         let rec wait expected_requests ready =
           let current = snapshot embedded in
           Option.iter current.failure ~f:(fun error -> raise (Protocol_error error));
           let is_ready = ready current in
           match
             !requests >= expected_requests
             && Option.is_none current.session.active_operation
             && is_ready
           with
           | true ->
             (match !requests = expected_requests with
              | true -> current
              | false ->
                raise
                  (Scenario_failure
                     "evaluation unexpectedly requested an extra model turn"))
           | false ->
             Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
             wait expected_requests ready
         in
         let initial = wait expected_requests (fun _ -> true) in
         match background with
         | None ->
           (match List.is_empty initial.jobs with
            | true -> initial
            | false -> failwith "synchronous evaluation unexpectedly started jobs")
         | Some scenario ->
           scenario.after_ack ~workspace embedded initial;
           let settled = wait scenario.final_requests scenario.settled in
           Option.iter embedded.replay_job_delivery ~f:(fun replay ->
             List.iter settled.jobs ~f:replay);
           let final = wait scenario.final_requests scenario.settled in
           require
             (Jsonaf.exactly_equal
                (P.Snapshot.to_json settled |> Jsonaf.member_exn "canonical_history")
                (P.Snapshot.to_json final |> Jsonaf.member_exn "canonical_history"))
             "completion replay changed published history";
           final))
;;

let outcome (snapshot : P.Snapshot.t) call_id =
  List.find_map_exn snapshot.canonical_history.entries ~f:(fun entry ->
    match Agent_session.History_codec.of_protocol entry |> get |> History_entry.item with
    | Openai.Responses.Item.Function_call_output
        { call_id = actual; output = Text text; _ }
      when String.equal actual call_id ->
      Some (P.Invocation.outcome_of_json (Jsonaf.of_string text) |> get)
    | _ -> None)
;;
