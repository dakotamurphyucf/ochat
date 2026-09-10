open Core
module F = Crash_recovery_fixture
module P = Agent_protocol
module Delta = Agent_session.Session_delta

let rec matches boundary = function
  | Delta.Batch deltas -> List.exists deltas ~f:(matches boundary)
  | Delivery_changed { status = Pending; _ } -> String.equal boundary "pending"
  | Delivery_committed ({ wake_disposition = Some Pending_wake; _ }, _) ->
    String.equal boundary "committed"
  | Delivery_wake_changed { wake_disposition = Some (Accepted_wake _); _ } ->
    String.equal boundary "accepted"
  | _ -> false
;;

let reached env boundary filename =
  let scan =
    F.read env filename
    |> Agent_store.Journal_segment.scan_contents ~max_payload_length:(64 * 1024 * 1024)
    |> F.store_ok
  in
  match List.last scan.entries with
  | Some entry when Agent_store.Frame.flags entry.frame = 0 ->
    let transaction =
      Agent_store.Frame.payload entry.frame
      |> Agent_store.Transaction.decode
      |> F.store_ok
    in
    let delta = Sexp.of_string transaction.delta |> Delta.t_of_sexp in
    (match matches boundary delta with
     | false -> ()
     | true ->
       Eio.Flow.copy_string
         ("notification-boundary " ^ boundary ^ "\n")
         (Eio.Stdenv.stdout env);
       Eio.Fiber.await_cancel ())
  | _ -> ()
;;

let call =
  let open Openai.Responses.Response_stream in
  [ Output_item_added
      { item =
          Function_call
            { name = "watch"
            ; arguments = ""
            ; call_id = "notification-watch"
            ; _type = "function_call"
            ; id = Some "notification-watch-item"
            ; status = None
            }
      ; output_index = 0
      ; type_ = "response.output_item.added"
      }
  ; Function_call_arguments_done
      { arguments = "null"
      ; item_id = "notification-watch-item"
      ; output_index = 0
      ; type_ = "response.function_call_arguments.done"
      }
  ]
;;

let run ?(standalone = false) env ~config_path ~boundary =
  let wrapped =
    Support.Crash_fault_io.wrap
      env
      ~matches:(fun path ->
        String.is_suffix path ~suffix:".log"
        && String.is_substring path ~substring:"/journal/")
      ~boundary:After_sync
      ~reached:(reached env boundary)
  in
  let config = Crash_side_effect_host.load_config wrapped config_path in
  let requests = ref 0 in
  let post_stream ~sw:_ ~inputs =
    Int.incr requests;
    let frames =
      List.count inputs ~f:(function
        | Openai.Responses.Item.Input_message
            { role = User; content = Text { text; _ } :: _; _ } ->
          String.is_prefix text ~prefix:"Ochat runtime notification."
        | _ -> false)
    in
    Eio.Flow.copy_string
      (sprintf "notification-provider %d frames=%d\n" !requests frames)
      (Eio.Stdenv.stdout env);
    match boundary, !requests with
    | "recover", 1 ->
      F.require
        (frames = if standalone then 1 else 2)
        "recovered provider received the wrong notification count";
      Stdlib.Seq.empty
    | "recover", _ -> F.fail "recovery started an extra provider call"
    | _, 1 -> Stdlib.List.to_seq call
    | _, 2 ->
      (match standalone with
       | false -> ()
       | true ->
         let release =
           Filename.concat (Filename.dirname config_path) "provider.release"
         in
         Eio.Time.with_timeout_exn (Eio.Stdenv.clock wrapped) 15. (fun () ->
           let rec wait () =
             match Eio.Path.is_file (F.path wrapped release) with
             | true -> ()
             | false ->
               Eio.Time.sleep (Eio.Stdenv.clock wrapped) 0.01;
               wait ()
           in
           wait ()));
      Stdlib.Seq.empty
    | _ -> F.fail "provider ran beyond selected crash boundary"
  in
  let options =
    { Agent_server.Daemon.default_options with
      qualify_chatml_extensions = true
    ; model_post_stream = Some post_stream
    }
  in
  Eio.Switch.run (fun sw ->
    let daemon =
      Agent_server.Daemon.start
        ~sw
        ~env:wrapped
        ~config
        ~tool_dir:(Filename.dirname config_path)
        ~home:(Sys.getenv_exn "HOME")
        ~process_start_identity:(Some "crash-notification-host")
        ~options
        ()
      |> F.protocol_ok
    in
    Eio.Fiber.fork_daemon ~sw (fun () ->
      Crash_side_effect_host.listener ~sw wrapped daemon config options;
      `Stop_daemon);
    Eio.Flow.copy_string "notification-host-ready\n" (Eio.Stdenv.stdout env);
    Eio.Fiber.await_cancel ())
;;
