open Core
module F = Crash_recovery_fixture
module P = Agent_protocol
module Delta = Agent_session.Session_delta

let rec accepted = function
  | Delta.Batch deltas -> List.exists deltas ~f:accepted
  | Ingress_changed registration -> not (List.is_empty registration.receipts)
  | _ -> false
;;

let reached env filename =
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
    (match Sexp.of_string transaction.delta |> Delta.t_of_sexp |> accepted with
     | false -> ()
     | true ->
       Eio.Flow.copy_string "ingress-receipt-saved\n" (Eio.Stdenv.stdout env);
       Eio.Fiber.await_cancel ())
  | _ -> ()
;;

let run env ~config_path ~recover =
  let wrapped =
    match recover with
    | true -> env
    | false ->
      Support.Crash_fault_io.wrap
        env
        ~matches:(fun path ->
          String.is_suffix path ~suffix:".log"
          && String.is_substring path ~substring:"/journal/")
        ~boundary:After_sync
        ~reached:(reached env)
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
      (sprintf "ingress-provider %d frames=%d\n" !requests frames)
      (Eio.Stdenv.stdout env);
    match recover, !requests with
    | false, 1 -> Stdlib.List.to_seq Crash_notification_host.call
    | false, 2 -> Stdlib.Seq.empty
    | true, 1 ->
      F.require (frames = 1) "recovered ingress did not deliver its notification";
      Stdlib.Seq.empty
    | _ -> F.fail "ingress started an unexpected model turn"
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
        ~process_start_identity:(Some "crash-ingress-host")
        ~options
        ()
      |> F.protocol_ok
    in
    Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Switch.run (fun sw ->
        Crash_side_effect_host.listener ~sw wrapped daemon config options);
      `Stop_daemon);
    Eio.Flow.copy_string "ingress-host-ready\n" (Eio.Stdenv.stdout env);
    Eio.Fiber.await_cancel ())
;;
