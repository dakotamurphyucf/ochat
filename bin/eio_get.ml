(** Streaming HTTP client for the [/mcp] Server-Sent-Events endpoint.

    The executable opens a long-lived HTTP connection to [HOST]/mcp
    using {!Piaf.Client} and prints every JSON payload it receives –
    one line per event.

    {1 Behaviour}

    • Creates a {!Piaf.Client.t} bound to the supplied [HOST].
    • Issues a [GET /mcp] request with automatic redirect following and
      insecure-TLS allowance for local testing.
    • Treats the response body as a stream of Server-Sent Events
      (RFC 8955): events are separated by a blank line.
    • Extracts the [data:] fields, parses them with {!Jsonaf.parse},
      pretty-prints them, and writes the result to [stdout].

    The program shuts down when the server closes the stream or an
    I/O error is raised.

    {1 Example}

    Connecting to a server running on [localhost:8080]:

    {[  $ ochat eio-get http://localhost:8080 ]}

    {1 Limitations}

    • Only the first [data:] field of each event is considered.
    • Lines equal to [data: [DONE]] are ignored.
    • The program assumes that [data:] contains valid JSON.
*)

open Core

module Result = struct
  include Result

  let ( let+ ) result f = map ~f result
  let ( let* ) result f = bind ~f result
end

(** [setup_log ?style_renderer level] initialises {!Logs} so that log
    messages are printed to stderr and colours are enabled when
    supported.

    @param style_renderer Selects the colour renderer (see
           {!Fmt_tty.style_renderer}).
    @param level Minimum log level to report. *)
let setup_log ?style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()
;;

(** [request ~env ~sw host] connects to [host]/mcp and consumes the
    resulting SSE stream.

    The function returns [Ok ()] when the remote peer closes the
    connection normally.  Returns [Error e] if {!Piaf} reports an
    error while issuing the request or streaming the response.

    Implementation details:

    1. Build a {!Piaf.Client.t} with redirect following and relaxed
       TLS checks (suitable for local development).
    2. Perform a [GET] request to [/mcp].
    3. Copy the response body into an {!Eio_unix.pipe}, so that we can
       run {!Eio.Buf_read} parsers over it without blocking the client
       fibre.
    4. Parse events with a small hand-rolled parser that accumulates
       lines until a blank line is encountered.
    5. For each event, extract the [data:] lines, ignore "[DONE]",
       parse the JSON with {!Jsonaf.parse}, and print it.

    @param env Runtime environment supplied by {!Eio_main.run}.
    @param sw  Switch controlling the lifetime of the connection and
               helper fibre. *)
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
