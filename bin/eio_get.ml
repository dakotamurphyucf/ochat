(** 
  This is a simple Eio-based HTTP client that connects to a server
  and streams events from the `/mcp` endpoint. It processes the events
  and prints them to stdout.
  It uses the Piaf library for HTTP requests and Eio for concurrency.
  It shows an example of how to stream SSE (Server-Sent Events) from
  a server and handle them in a structured way. Piaf determines if a connection should
    be made over HTTP or HTTPS based on the URI scheme.
  The client reads the events until it encounters a double newline,
  which indicates the end of an event. Each event is processed to extract
  the data, which is expected to be in JSON format. The client prints
  the data to stdout, and it handles errors gracefully.
  It also sets up logging for debugging purposes.
  To run this client, you need to provide the server's hostname as a command-line argument.
  Usage: `eio_get.exe HOST`
  where `HOST` is the server's hostname (e.g., `http://localhost:8080`).
  The client uses Eio's concurrency model to handle the request and response
  in a non-blocking way, allowing it to process incoming events as they arrive.
  It also demonstrates how to handle errors and clean up resources properly.
  The client is designed to be simple and illustrative, focusing on the
  mechanics of streaming events and processing them in a structured way.
  It can be extended or modified to suit more complex use cases, such as
  handling different types of events, integrating with other systems, or
  processing the events in a more sophisticated manner.
*)

open Core

module Result = struct
  include Result

  let ( let+ ) result f = map ~f result
  let ( let* ) result f = bind ~f result
end

let setup_log ?style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()
;;

let request ~env ~sw host =
  let open Piaf in
  let open Result in
  let module B = Eio.Buf_read in
  let* client =
    Client.create
      env
      ~sw
      ~config:
        { Config.default with
          follow_redirects = true
        ; allow_insecure = true
        ; flush_headers_immediately = true
        }
      (Uri.of_string host)
  in
  let+ response = Client.get client "/mcp" in
  let r, w = Eio_unix.pipe sw in
  Eio.Fiber.fork ~sw (fun () ->
    let body = Response.body response in
    let res =
      Body.iter
        ~f:(fun { buffer; off; len } ->
          Eio.Flow.write w [ Cstruct.of_bigarray ~off ~len buffer ])
        body
    in
    (match res with
     | Ok () -> ()
     | Error error -> Format.eprintf "error: %a@." Piaf.Error.pp_hum error);
    Eio.Flow.close w);
  let reader = Eio.Buf_read.of_flow r ~max_size:Core.Int.max_value in
  (* we want to get all the lines until we hit a double newline *)
  let parse_event =
    let rec run acc =
      let open B.Syntax in
      let* line = B.line in
      let* char = B.peek_char in
      match char with
      | Some '\n' ->
        let* () = B.skip 1 in
        B.return (line :: acc |> List.rev |> String.concat)
      | _ -> run ("\n" :: line :: acc)
    in
    run []
  in
  let events = B.seq parse_event ~stop:B.at_end_of_input reader in
  let on_event event =
    let data =
      String.concat
      @@ List.filter_map ~f:(fun line ->
        if
          String.is_prefix ~prefix:"data: " line
          && (not @@ String.is_prefix ~prefix:"data: [DONE]" line)
        then Some (String.chop_prefix_exn line ~prefix:"data: ")
        else None)
      @@ String.split_lines event
    in
    let choice =
      match String.is_empty data with
      | true -> None
      | false ->
        (match Jsonaf.parse data |> Result.bind ~f:(fun json -> Ok json) with
         | Ok json ->
           let event = Jsonaf.to_string_hum json in
           Some event
         | Error _ -> None)
    in
    match choice with
    | None -> ()
    | Some choice ->
      Printf.printf "%s\n" choice;
      Out_channel.flush stdout
  in
  match Seq.iter on_event events with
  | () ->
    Eio.Flow.close r;
    Client.shutdown client
  | exception Eio.Exn.Io (err, _) -> Printf.printf "%s" @@ Fmt.str "%a" Eio.Exn.pp_err err
  | exception ex ->
    Printf.printf "%s" @@ Fmt.str "Error processing response: %a" Eio.Exn.pp ex
;;

let () =
  setup_log (Some Logs.Debug);
  let host = ref None in
  Arg.parse [] (fun host_argument -> host := Some host_argument) "eio_get.exe HOST";
  let host =
    match !host with
    | None -> failwith "No hostname provided"
    | Some host -> host
  in
  Eio_main.run (fun env ->
    Eio.Switch.run (fun sw ->
      match request ~sw ~env host with
      | Ok () -> ()
      | Error e -> failwith (Piaf.Error.to_string e)))
;;
