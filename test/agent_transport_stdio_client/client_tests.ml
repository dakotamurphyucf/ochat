open! Core
module P = Agent_protocol
module Child = Agent_transport_stdio.Client

let ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : P.Error.t)]
;;

let limits ?(grace = 0.05) ?(max_line_length = 65_536) ?(max_stderr_bytes = 128) () =
  Child.Limits.create
    ~max_line_length
    ~notification_capacity:4
    ~max_stderr_bytes
    ~shutdown_grace_seconds:grace
  |> ok
;;

let ping = P.Command.Protocol_ping { payload = None }

let%expect_test "stdio child keeps stderr separate and correlates a typed response" =
  Eio_main.run (fun env ->
    let id = P.Envelope.Request_id.of_json (`Number "1") |> ok in
    let request =
      P.Command.Protocol_ping { payload = Some (`String (String.make 256 'x')) }
    in
    let encoded_request_bytes =
      P.Envelope.request
        ~id
        ~method_:(P.Command.method_name request)
        ~params:(P.Command.params request)
        ()
      |> P.Envelope.to_json
      |> Jsonaf.to_string
      |> String.length
    in
    let response =
      P.Envelope.failure ~id (P.Error.invalid_request "controlled child failure")
      |> P.Envelope.to_json
      |> Jsonaf.to_string
    in
    let script =
      {|IFS= read -r request
i=0
while [ "$i" -lt 512 ]; do
  printf 'abcdefghijklmnop' >&2
  i=$((i+1))
done
printf '%s\n' "$1"|}
    in
    let outcome =
      Child.with_spawned
        ~env
        ~argv:[ "/bin/sh"; "-c"; script; "fixture"; response ]
        ~limits:
          (limits ~max_line_length:(encoded_request_bytes + 1) ~max_stderr_bytes:5_000 ())
        ~f:(fun child ->
          let response =
            Agent_client.Connection.request (Child.connection child) request
          in
          let code =
            match response with
            | Error error -> error.code
            | Ok _ -> failwith "unexpected successful response"
          in
          let status = Child.close_and_wait child |> ok in
          let stderr = Child.stderr_tail child in
          let full_stderr =
            String.concat (List.init 512 ~f:(fun _ -> "abcdefghijklmnop"))
          in
          print_s
            [%sexp
              { code : P.Error.code
              ; exited_zero =
                  ((match status with
                    | `Exited 0 -> true
                    | `Exited _ | `Signaled _ -> false)
                   : bool)
              ; stderr_bytes = (String.length stderr : int)
              ; stderr_tail_is_bounded =
                  (String.equal
                     stderr
                     (String.sub
                        full_stderr
                        ~pos:(String.length full_stderr - 5_000)
                        ~len:5_000)
                   : bool)
              }];
          Ok ())
    in
    ignore (ok outcome : unit));
  [%expect
    {|
    ((code Invalid_request) (exited_zero true) (stderr_bytes 5000)
     (stderr_tail_is_bounded true))
    |}]
;;

let%test_unit "partial write cancellation retires the shared channel" =
  Eio_main.run (fun _env ->
    Eio.Switch.run (fun sw ->
      let source_closed, source_closed_u = Eio.Promise.create () in
      let write_entered, write_entered_u = Eio.Promise.create () in
      let close_count = ref 0 in
      let flows_closed = ref false in
      let module Source = struct
        type t = unit

        let read_methods = []

        let single_read () _ =
          Eio.Promise.await source_closed;
          raise End_of_file
        ;;
      end
      in
      let first_write = ref true in
      let module Sink = struct
        type t = unit

        let single_write () _ =
          if !first_write
          then (
            first_write := false;
            Eio.Promise.resolve write_entered_u ();
            1)
          else Eio.Fiber.await_cancel ()
        ;;

        let copy () ~src = Eio.Flow.Pi.simple_copy ~single_write () ~src
      end
      in
      let source = Eio.Resource.T ((), Eio.Flow.Pi.source (module Source)) in
      let sink = Eio.Resource.T ((), Eio.Flow.Pi.sink (module Sink)) in
      let owner =
        Agent_transport_ndjson.Client.create
          ~sw
          ~transport_name:"fixture"
          ~input:source
          ~output:sink
          ~close_flows:(fun () ->
            if not !flows_closed
            then (
              flows_closed := true;
              Int.incr close_count;
              ignore (Eio.Promise.try_resolve source_closed_u () : bool)))
          ~max_line_length:4_096
          ~notification_capacity:2
        |> ok
      in
      let connection = Agent_transport_ndjson.Client.connection owner in
      let id = P.Envelope.Request_id.of_json (`Number "1") |> ok in
      let empty_ping = P.Command.Protocol_ping { payload = Some (`String "") } in
      let base_bytes =
        P.Envelope.request
          ~id
          ~method_:(P.Command.method_name empty_ping)
          ~params:(P.Command.params empty_ping)
          ()
        |> P.Envelope.to_json
        |> Jsonaf.to_string
        |> String.length
      in
      let exact_line_ping =
        P.Command.Protocol_ping
          { payload = Some (`String (String.make (4_096 - base_bytes) 'x')) }
      in
      let exact_line_bytes =
        P.Envelope.request
          ~id
          ~method_:(P.Command.method_name exact_line_ping)
          ~params:(P.Command.params exact_line_ping)
          ()
        |> P.Envelope.to_json
        |> Jsonaf.to_string
        |> String.length
      in
      assert (Int.equal exact_line_bytes 4_096);
      (match Agent_client.Connection.request connection exact_line_ping with
       | Error error -> assert (P.Error.equal_code error.code Invalid_request)
       | Ok _ -> failwith "request without newline headroom was written");
      assert (Option.is_none (Eio.Promise.peek write_entered));
      let cancelled =
        Eio.Fiber.first
          (fun () ->
             ignore (Agent_client.Connection.request connection ping);
             false)
          (fun () ->
             Eio.Promise.await write_entered;
             true)
      in
      assert cancelled;
      assert (Int.equal !close_count 1);
      Agent_transport_ndjson.Client.close_and_wait owner;
      assert (Int.equal !close_count 1);
      match Agent_client.Connection.request connection ping with
      | Error error -> assert (P.Error.equal_code error.code Interrupted)
      | Ok _ -> failwith "retired channel accepted a request"))
;;

let%test_unit "cancelled shared reader retires pending admission" =
  Eio_main.run (fun _env ->
    let ready, ready_u = Eio.Promise.create () in
    let retained = ref None in
    let writes = ref 0 in
    let module Source = struct
      type t = unit

      let read_methods = []
      let single_read () _ = Eio.Fiber.await_cancel ()
    end
    in
    let module Sink = struct
      type t = unit

      let single_write () _ =
        Int.incr writes;
        raise End_of_file
      ;;

      let copy () ~src = Eio.Flow.Pi.simple_copy ~single_write () ~src
    end
    in
    let source = Eio.Resource.T ((), Eio.Flow.Pi.source (module Source)) in
    let sink = Eio.Resource.T ((), Eio.Flow.Pi.sink (module Sink)) in
    let winner =
      Eio.Fiber.first
        (fun () ->
           (match
              Eio.Switch.run (fun sw ->
                let owner =
                  Agent_transport_ndjson.Client.create
                    ~sw
                    ~transport_name:"fixture"
                    ~input:source
                    ~output:sink
                    ~close_flows:(fun () -> ())
                    ~max_line_length:4_096
                    ~notification_capacity:2
                  |> ok
                in
                retained := Some owner;
                Eio.Promise.resolve ready_u ();
                Eio.Fiber.await_cancel ())
            with
            | exception Eio.Cancel.Cancelled _ -> ()
            | () -> failwith "cancelled reader owner returned normally");
           false)
        (fun () ->
           Eio.Promise.await ready;
           true)
    in
    assert winner;
    let owner = Option.value_exn !retained in
    let connection = Agent_transport_ndjson.Client.connection owner in
    (match Agent_client.Connection.request connection ping with
     | Error error -> assert (P.Error.equal_code error.code Interrupted)
     | Ok _ -> failwith "readerless channel accepted a request");
    assert (Int.equal !writes 0);
    Agent_transport_ndjson.Client.close_and_wait owner)
;;

let%test_unit "ignored TERM is killed and joined; caller exception stays primary" =
  Eio_main.run (fun env ->
    let argv =
      [ "/bin/sh"; "-c"; "trap '' TERM; printf ready >&2; while :; do :; done" ]
    in
    let status =
      Child.with_spawned ~env ~argv ~limits:(limits ()) ~f:(fun child ->
        Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 2. (fun () ->
          while not (String.is_substring (Child.stderr_tail child) ~substring:"ready") do
            Eio.Time.sleep (Eio.Stdenv.clock env) 0.001
          done);
        let first = ref None in
        let second = ref None in
        Eio.Fiber.both
          (fun () -> first := Some (Child.close_and_wait child))
          (fun () -> second := Some (Child.close_and_wait child));
        let first = Option.value_exn !first |> ok in
        let second = Option.value_exn !second |> ok in
        assert (
          match first, second with
          | `Exited a, `Exited b | `Signaled a, `Signaled b -> Int.equal a b
          | (`Exited _ | `Signaled _), _ -> false);
        Ok first)
      |> ok
    in
    (match status with
     | `Signaled signal -> assert (Int.equal signal Stdlib.Sys.sigkill)
     | `Exited _ -> failwith "TERM-ignoring child exited normally");
    let original = Failure "callback marker" in
    match
      Child.with_spawned ~env ~argv ~limits:(limits ()) ~f:(fun _ -> raise original)
    with
    | exception exn -> assert (phys_equal exn original)
    | Ok _ | Error _ -> failwith "callback exception was lost")
;;

let%test_unit "cancellation at callback and close stages joins one child" =
  Eio_main.run (fun env ->
    let clock = Eio.Stdenv.clock env in
    let argv =
      [ "/bin/sh"
      ; "-c"
      ; "trap 'printf term >&2; while :; do :; done' TERM; printf ready >&2; while :; do \
         :; done"
      ]
    in
    let await_tail child marker =
      Eio.Time.with_timeout_exn clock 2. (fun () ->
        while not (String.is_substring (Child.stderr_tail child) ~substring:marker) do
          Eio.Time.sleep clock 0.001
        done)
    in
    let assert_killed child =
      match Child.close_and_wait child with
      | Ok (`Signaled signal) -> assert (Int.equal signal Stdlib.Sys.sigkill)
      | Ok (`Exited status) -> failwithf "cancelled child exited %d" status ()
      | Error error -> raise_s [%sexp "cancelled child cleanup", (error : P.Error.t)]
    in
    List.iter [ `Callback; `Explicit_close; `Implicit_close ] ~f:(fun stage ->
      let retained = ref None in
      let cancelled = ref false in
      let started, started_u = Eio.Promise.create () in
      let close_started, close_started_u = Eio.Promise.create () in
      let winner =
        Eio.Fiber.first
          (fun () ->
             (match
                Child.with_spawned ~env ~argv ~limits:(limits ()) ~f:(fun child ->
                  retained := Some child;
                  await_tail child "ready";
                  Eio.Promise.resolve started_u ();
                  match stage with
                  | `Callback -> Eio.Fiber.await_cancel ()
                  | `Explicit_close ->
                    Eio.Fiber.both
                      (fun () ->
                         Eio.Promise.resolve close_started_u ();
                         ignore (Child.close_and_wait child))
                      (fun () ->
                         Eio.Promise.await close_started;
                         ignore (Child.close_and_wait child));
                    Ok ()
                  | `Implicit_close -> Ok ())
              with
              | exception Eio.Cancel.Cancelled _ -> cancelled := true
              | Ok _ | Error _ -> failwith "cancelled child returned normally");
             false)
          (fun () ->
             Eio.Promise.await started;
             (match stage with
              | `Callback -> ()
              | `Explicit_close | `Implicit_close ->
                let rec await_retained () =
                  match !retained with
                  | None ->
                    Eio.Time.sleep clock 0.001;
                    await_retained ()
                  | Some child -> await_tail child "term"
                in
                await_retained ());
             true)
      in
      assert winner;
      assert !cancelled;
      assert_killed (Option.value_exn !retained));
    let local_cancelled = ref false in
    Child.with_spawned ~env ~argv ~limits:(limits ()) ~f:(fun child ->
      await_tail child "ready";
      let won =
        Eio.Fiber.first
          (fun () ->
             (match Child.close_and_wait child with
              | Ok _ | Error _ -> failwith "close caller unexpectedly completed"
              | exception Eio.Cancel.Cancelled _ -> local_cancelled := true);
             false)
          (fun () ->
             await_tail child "term";
             true)
      in
      assert won;
      assert !local_cancelled;
      Eio.Time.with_timeout_exn clock 2. (fun () -> assert_killed child);
      Ok ())
    |> ok)
;;
