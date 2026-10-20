open! Core
module P = Agent_protocol
module W = P.Developer_repl_wire
module D = P.Developer_common
module T = Chatml_repl_terminal

let protocol_error error = Error.create_s [%sexp (error : P.Error.t)]
let of_protocol = Result.map_error ~f:protocol_error

let route
      ~limits
      ~root_index
      ~dependency_root_indices
      ~source
      ~project
      ~entry
      ~environment
  =
  let open Result.Let_syntax in
  let request_limits = W.Limits.request limits in
  match source, project, entry, environment with
  | Some member, None, None, None when List.is_empty dependency_root_indices ->
    let%bind member =
      Source.Portable_path.create member
      |> Result.map_error ~f:(fun error ->
        P.Error.invalid_request (Error.to_string_hum error))
    in
    D.Route.local_source ~limits:request_limits ~root_index ~member
  | None, Some project, Some name, environment ->
    let%bind project =
      Source.Portable_path.create project
      |> Result.map_error ~f:(fun error ->
        P.Error.invalid_request (Error.to_string_hum error))
    in
    D.Route.local_project
      ~limits:request_limits
      ~root_index
      ~dependency_root_indices
      ~project
      ~selection:(Chatml_project.Selection.Entry { name; environment })
  | _ ->
    Error
      (P.Error.invalid_request
         "select --source MEMBER or --project PROJECT --entry NAME [--environment NAME]; \
          dependency roots require --project")
;;

let target
      ~local
      ~connect
      ~stdio_executable
      ~developer_policy
      ~prompt_file
      ~workspace
      ~data_root
      ~transient
      ~bearer_token_file
      ~expected_server
  =
  let open Result.Let_syntax in
  match local, connect with
  | true, None ->
    let%bind policy_file, prompt_file, workspace =
      match developer_policy, prompt_file, workspace with
      | Some policy, Some prompt, Some workspace -> Ok (policy, prompt, workspace)
      | _ ->
        Error
          (P.Error.invalid_request
             "--local requires --developer-policy, --prompt-file and --workspace")
    in
    let%bind storage =
      match transient, data_root with
      | true, None -> Ok T.Client_adapter.Local.Transient
      | false, Some path -> Ok (Durable path)
      | true, Some _ | false, None ->
        Error (P.Error.invalid_request "select exactly one of --transient or --data-root")
    in
    let%map local =
      T.Client_adapter.Local.create
        ~executable:stdio_executable
        ~policy_file
        ~prompt_file
        ~workspace
        ~storage
    in
    T.Client_adapter.Local local
  | false, Some endpoint ->
    let%bind expected_server =
      Option.value_map expected_server ~default:(Ok None) ~f:(fun value ->
        Result.map (P.Id.Server.of_string value) ~f:Option.some)
    in
    let%map profile =
      Agent_transport_client.Connection_profile.create
        ~home:(Sys.getenv "HOME")
        ~name:"chatml-repl"
        ~endpoint
        ~expected_server
        ~daemon_credential_file:bearer_token_file
    in
    T.Client_adapter.Remote profile
  | true, Some _ | false, None ->
    Error (P.Error.invalid_request "select exactly one of --local or --connect URI")
;;

let run
      ~local
      ~connect
      ~stdio_executable
      ~developer_policy
      ~prompt_file
      ~workspace
      ~data_root
      ~transient
      ~bearer_token_file
      ~expected_server
      ~source
      ~project
      ~entry
      ~environment
      ~root_index
      ~dependency_root_indices
      ~prompt_definition
      ~workspace_definition
  =
  Mirage_crypto_rng_unix.use_default ();
  Eio_main.run (fun env ->
    let open Result.Let_syntax in
    let%bind target =
      target
        ~local
        ~connect
        ~stdio_executable
        ~developer_policy
        ~prompt_file
        ~workspace
        ~data_root
        ~transient
        ~bearer_token_file
        ~expected_server
    in
    T.Client_adapter.with_connection
      ~env
      ~target
      ~notification_capacity:256
      ~f:(fun session ->
        let client = T.Client_adapter.client session in
        let limits =
          Agent_client.Developer_repl_connection.capabilities client
          |> W.Capabilities.limits
        in
        let%bind route =
          route
            ~limits
            ~root_index
            ~dependency_root_indices
            ~source
            ~project
            ~entry
            ~environment
        in
        let%bind prompt_definition, workspace_definition =
          match local, prompt_definition, workspace_definition with
          | true, None, None -> Ok ("embedded.prompt", "embedded.workspace")
          | _, Some prompt, Some workspace -> Ok (prompt, workspace)
          | _ ->
            Error
              (P.Error.invalid_request
                 "remote REPL requires --prompt-definition and --workspace-definition")
        in
        let%bind key =
          P.Id.Attachment.create ()
          |> P.Id.Attachment.to_string
          |> P.Idempotency_key.of_string
        in
        let%bind create =
          W.Create.create
            ~limits
            ~run_id:(P.Id.Developer_run.create ())
            ~key
            ~route
            ~target:D.Target.One_off_v1
            ~prompt:(Agent_server.Catalog_identity.prompt_definition prompt_definition)
            ~workspace:
              (Agent_server.Catalog_identity.workspace_definition workspace_definition)
            ~requested_execution:None
            ~requested_max_output_bytes:None
        in
        let%bind editor_limits =
          T.State.Limits.create
            ~max_source_bytes:(Int.min 65_536 (W.Limits.max_source_bytes limits))
            ~max_source_lines:4_096
            ~max_history_entries:128
            ~max_history_bytes:1_048_576
            ~max_notice_bytes:16_384
        in
        let selection =
          match source, project, entry with
          | Some member, None, None -> "source=" ^ member
          | None, Some project, Some entry ->
            Printf.sprintf
              "project=%s entry=%s environment=%s"
              project
              entry
              (Option.value environment ~default:"(project default)")
          | _ -> "invalid selection"
        in
        let context =
          Printf.sprintf
            "One_off_v1 root=%d %s\nprompt=%s workspace=%s\n%s"
            root_index
            selection
            prompt_definition
            workspace_definition
            (T.Client_adapter.description session)
        in
        let%bind initial = T.State.create ~limits:editor_limits ~context in
        let%bind renderer = T.Render.create ~max_display_bytes:32_768 in
        let%bind input = T.Controller.Input_queue.create ~capacity:256 in
        let paste = ref None in
        let offer = T.Controller.Input_queue.offer input in
        let add_paste text =
          match !paste with
          | None -> ()
          | Some buffer ->
            if
              String.length text
              > T.State.Limits.max_source_bytes editor_limits
                - Stdlib.Buffer.length buffer
            then (
              paste := None;
              T.Controller.Input_queue.fail
                input
                ~message:"Bracketed paste exceeds editor byte limit")
            else Stdlib.Buffer.add_string buffer text
        in
        let unicode uchar =
          let buffer = Stdlib.Buffer.create 4 in
          Stdlib.Buffer.add_utf_8_uchar buffer uchar;
          Stdlib.Buffer.contents buffer
        in
        let on_event =
          let open T.Controller.Input in
          function
          | `Resize -> offer Resize
          | `Paste `Start -> paste := Some (Stdlib.Buffer.create 256)
          | `Paste `End ->
            (match !paste with
             | None -> ()
             | Some buffer ->
               paste := None;
               offer (Paste (Stdlib.Buffer.contents buffer)))
          | `Mouse _ -> ()
          | `Key (key, mods) ->
            (match !paste with
             | Some _ ->
               (match key with
                | `ASCII char -> add_paste (String.of_char char)
                | `Uchar uchar -> add_paste (unicode uchar)
                | `Enter -> add_paste "\n"
                | `Tab -> add_paste "\t"
                | _ ->
                  paste := None;
                  T.Controller.Input_queue.fail
                    input
                    ~message:"Unsupported control in bracketed paste")
             | None ->
               (match key, mods with
                | `ASCII char, [ `Ctrl ] ->
                  (match Char.lowercase char with
                   | 's' -> offer Submit
                   | 'c' -> offer Cancel
                   | 'q' -> offer Quit
                   | _ -> ())
                | `ASCII char, [] -> offer (Text (String.of_char char))
                | `Uchar uchar, [] -> offer (Text (unicode uchar))
                | `Enter, _ -> offer Enter
                | `Tab, _ -> offer (Text "\t")
                | `Backspace, _ -> offer Backspace
                | `Delete, _ -> offer Delete
                | `Home, _ -> offer (Move Home)
                | `End, _ -> offer (Move End)
                | `Arrow `Left, _ -> offer (Move Left)
                | `Arrow `Right, _ -> offer (Move Right)
                | `Arrow `Up, _ -> offer History_previous
                | `Arrow `Down, _ -> offer History_next
                | _ -> ()))
        in
        Notty_eio.Term.run
          ~nosig:true
          ~mouse:false
          ~bpaste:true
          ~input:(Eio.Stdenv.stdin env)
          ~output:(Eio.Stdenv.stdout env)
          ~on_event
          (fun term ->
             let present state =
               let width, height = Notty_eio.Term.size term in
               let image, cursor = T.Render.image renderer state ~width ~height in
               Notty_eio.Term.present term ~image ~cursor
             in
             T.Controller.run ~session ~create ~initial ~input ~present)))
  |> of_protocol
;;

let command =
  let open Command.Let_syntax in
  Command.basic_or_error
    ~summary:"Interactive ChatML REPL over a configured Developer host"
    [%map_open
      let local = flag "--local" no_arg ~doc:" Launch a configured local stdio host"
      and connect = flag "--connect" (optional string) ~doc:"URI Connect to a daemon"
      and stdio_executable =
        flag
          "--stdio-executable"
          (optional_with_default "ochat-agent-stdio" string)
          ~doc:"PATH Trusted local host executable"
      and developer_policy =
        flag "--developer-policy" (optional string) ~doc:"FILE Local Developer policy"
      and prompt_file =
        flag "--prompt-file" (optional string) ~doc:"FILE Local ChatMD prompt"
      and workspace = flag "--workspace" (optional string) ~doc:"DIR Local workspace"
      and data_root =
        flag "--data-root" (optional string) ~doc:"DIR Durable local data root"
      and transient = flag "--transient" no_arg ~doc:" Use process-bound local storage"
      and bearer_token_file =
        flag "--bearer-token-file" (optional string) ~doc:"FILE Remote HTTP credential"
      and expected_server =
        flag "--expected-server" (optional string) ~doc:"ID Pin the remote server"
      and source =
        flag "--source" (optional string) ~doc:"MEMBER Source member in a configured root"
      and project =
        flag
          "--project"
          (optional string)
          ~doc:"PROJECT Project path in a configured root"
      and entry = flag "--entry" (optional string) ~doc:"NAME Project entry"
      and environment =
        flag "--environment" (optional string) ~doc:"NAME Project environment"
      and root_index =
        flag
          "--root-index"
          (optional_with_default 0 int)
          ~doc:"N Configured source root index"
      and dependency_root_indices =
        flag
          "--dependency-root-index"
          (listed int)
          ~doc:"N Additional configured project dependency root index (repeatable)"
      and prompt_definition =
        flag
          "--prompt-definition"
          (optional string)
          ~doc:"NAME Configured prompt definition"
      and workspace_definition =
        flag
          "--workspace-definition"
          (optional string)
          ~doc:"NAME Configured workspace definition"
      in
      fun () ->
        run
          ~local
          ~connect
          ~stdio_executable
          ~developer_policy
          ~prompt_file
          ~workspace
          ~data_root
          ~transient
          ~bearer_token_file
          ~expected_server
          ~source
          ~project
          ~entry
          ~environment
          ~root_index
          ~dependency_root_indices
          ~prompt_definition
          ~workspace_definition]
;;

let () = Command_unix.run command
