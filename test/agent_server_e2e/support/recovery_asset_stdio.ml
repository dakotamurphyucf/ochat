open! Core
module P = Agent_protocol
module D = Agent_server.Dispatcher
module Wire = P.Recovery_asset_read

let ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp "stdio asset fixture failed", (error : P.Error.t)]
;;

let check env ~dispatcher ~actor request ~expected =
  let clock = Eio.Stdenv.clock env in
  Eio.Time.with_timeout_exn clock 10. (fun () ->
    Eio.Switch.run (fun sw ->
      let current = ref true in
      let actor =
        Operator_authorization.restrict actor ~is_current:(fun () -> !current)
      in
      let lines = Eio.Stream.create 0 in
      let next_read = ref None in
      let module Source = struct
        type t = unit

        let read_methods = []

        let single_read () buffer =
          let previous = !next_read in
          next_read := None;
          Option.iter previous ~f:(fun notify -> Eio.Promise.resolve notify ());
          match Eio.Stream.take lines with
          | None -> raise End_of_file
          | Some (line, notify) ->
            next_read := notify;
            let len = String.length line in
            assert (len <= Cstruct.length buffer);
            Cstruct.blit_from_string line 0 buffer 0 len;
            len
        ;;
      end
      in
      let input = Eio.Resource.T ((), Eio.Flow.Pi.source (module Source)) in
      let input_from_server, to_client = Eio_unix.pipe sw in
      let reader =
        Eio.Buf_read.of_flow input_from_server ~max_size:Wire.max_envelope_bytes
      in
      let pause = ref None in
      let module Sink = struct
        type t = unit

        let single_write () buffers =
          let blocked = !pause in
          pause := None;
          Option.iter blocked ~f:(fun (notify, released) ->
            Eio.Promise.resolve notify ();
            Eio.Promise.await released);
          Eio.Flow.single_write to_client buffers
        ;;

        let copy () ~src = Eio.Flow.Pi.simple_copy ~single_write () ~src
      end
      in
      let output = Eio.Resource.T ((), Eio.Flow.Pi.sink (module Sink)) in
      let server =
        Eio.Fiber.fork_promise ~sw (fun () ->
          Agent_transport_stdio.Server.run_authenticated
            ~sw
            ~dispatcher
            ~close_connection:(fun context ->
              D.close_connection_and_wait dispatcher context |> ok)
            ~actor
            ~connection_id:"original-asset-stdio"
            ~input
            ~output
            ~max_line_length:Wire.max_envelope_bytes
            ~outgoing_capacity:8
            ~max_attachments:1
            ~on_error:(fun error -> ignore (ok (Error error) : unit)))
      in
      let send number ?notify command =
        let id = P.Envelope.Request_id.of_json (`Number (Int.to_string number)) |> ok in
        let envelope =
          P.Envelope.request
            ~id
            ~method_:(P.Command.method_name command)
            ~params:(P.Command.params command)
            ()
        in
        let line = Jsonaf.to_string (P.Envelope.to_json envelope) ^ "\n" in
        Eio.Stream.add lines (Some (line, notify));
        id
      in
      let read id =
        match
          Eio.Buf_read.line reader |> Jsonaf.of_string |> P.Envelope.of_json |> ok
        with
        | Response response when P.Envelope.Request_id.compare id response.id = 0 ->
          response.outcome
        | _ -> failwith "unexpected stdio recovery envelope"
      in
      let implementation =
        P.Initialize.Implementation.create ~name:"recovery-stdio-fixture" ~version:"dev"
        |> ok
      in
      let initialization =
        P.Initialize.Request.create
          ~implementation
          ~protocol_min:P.Version.current
          ~protocol_max:P.Version.current
          ~features:[ Wire.feature ]
          ~event_encodings:[ Ndjson ]
          ~max_inbound_event_bytes:65536
          ()
        |> ok
      in
      let initialized = send 1 (Protocol_initialize initialization) in
      ignore (read initialized |> ok : Jsonaf.t);
      let asset = P.Command.Recovery_asset_read request in
      let first = send 2 asset in
      let chunk = read first |> ok |> Wire.Reply.of_json |> ok in
      Wire.Reply.validate chunk ~request |> ok;
      assert (String.equal chunk.bytes expected);
      (* Block the actual writer on an earlier ping. The next source read
         starts only after the asset has been dispatched and queued. *)
      let blocked, notify = Eio.Promise.create () in
      let released, release = Eio.Promise.create () in
      pause := Some (notify, released);
      let ping = send 3 (Protocol_ping { payload = None }) in
      Eio.Promise.await blocked;
      let queued, notify = Eio.Promise.create () in
      let held = send 4 ~notify asset in
      Eio.Promise.await queued;
      current := false;
      Eio.Promise.resolve release ();
      ignore (read ping |> ok : Jsonaf.t);
      (match read held with
       | Error error when P.Error.equal_code error.code Permission_denied -> ()
       | Error _ | Ok _ -> failwith "queued stdio recovery asset survived withdrawal");
      Eio.Stream.add lines None;
      Eio.Promise.await_exn server))
;;
