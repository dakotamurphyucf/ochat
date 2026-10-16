open! Core
module Remote = Developer_child_remote_content
module P = Agent_protocol

let ok = function
  | Ok value -> value
  | Error _ -> failwith "expected admitted remote fixture"
;;

let%expect_test "remote content uses exact loopback policy and finite body" =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run (fun sw ->
    let listener =
      Eio.Net.listen
        ~sw
        ~reuse_addr:true
        ~backlog:4
        (Eio.Stdenv.net env)
        (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
    in
    let port =
      match Eio.Net.listening_addr listener with
      | `Tcp (_, port) -> port
      | _ -> failwith "loopback server did not bind"
    in
    let requested = ref 0 in
    Eio.Fiber.fork_daemon ~sw (fun () ->
      while true do
        let flow, _ = Eio.Net.accept ~sw listener in
        Eio.Fiber.fork_daemon ~sw (fun () ->
          try
            Exn.protect
              ~finally:(fun () -> Eio.Flow.close flow)
              ~f:(fun () ->
                let reader = Eio.Buf_read.of_flow flow ~max_size:1024 in
                ignore (Eio.Buf_read.line reader : string);
                let rec headers () =
                  if String.is_empty (Eio.Buf_read.line reader) then () else headers ()
                in
                headers ();
                incr requested;
                Eio.Flow.copy_string
                  (if !requested = 5
                   then "HTTP/1.1 200 OK\rX"
                   else if !requested = 3
                   then
                     "HTTP/1.1 200 OK\r\n\
                      Set-Cookie: a=1\r\n\
                      Set-Cookie: b=2\r\n\
                      Connection: close\r\n\
                      \r\n\
                      bye"
                   else if !requested = 4
                   then
                     "HTTP/1.1 200 OK\r\n\
                      Content-Length: 8193\r\n\
                      Connection: close\r\n\
                      \r\n"
                     ^ String.make 8193 'x'
                   else
                     "HTTP/1.1 200 OK\r\n\
                      Content-Length: 5\r\n\
                      Connection: close\r\n\
                      \r\n\
                      hello")
                  flow;
                `Stop_daemon)
          with
          | End_of_file | Eio.Io _ -> `Stop_daemon)
      done);
    let origin = sprintf "http://127.0.0.1:%d" port in
    let policy =
      Remote.Policy.create
        ~allowed_origins:[ origin ]
        ~max_response_bytes:8
        ~max_wall_seconds:5.
      |> Or_error.ok_exn
    in
    let remote =
      Remote.create ~policy ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.mono_clock env)
      |> ok
    in
    let acquisitions = ref 0 in
    let check_owner () = Ok () in
    let fetch url max_bytes =
      Remote.read
        remote
        ~url
        ~max_bytes
        ~check_owner
        ~checkpoint:(fun () -> ())
        ~on_acquire:(fun () -> incr acquisitions)
    in
    let admitted = fetch (origin ^ "/asset?x=1") 8 |> ok in
    let before_denial = !acquisitions in
    let denied = fetch "http://127.0.0.1:1/asset" 8 in
    let denied_before_acquisition = Int.equal !acquisitions before_denial in
    let limited = fetch (origin ^ "/asset") 4 in
    let close_delimited = fetch (origin ^ "/short") 8 |> ok in
    let large_policy =
      Remote.Policy.create
        ~allowed_origins:[ origin ]
        ~max_response_bytes:8193
        ~max_wall_seconds:5.
      |> Or_error.ok_exn
    in
    let large_remote =
      Remote.create
        ~policy:large_policy
        ~net:(Eio.Stdenv.net env)
        ~clock:(Eio.Stdenv.mono_clock env)
      |> ok
    in
    let large =
      Remote.read
        large_remote
        ~url:(origin ^ "/large")
        ~max_bytes:8193
        ~check_owner
        ~checkpoint:(fun () -> ())
        ~on_acquire:(fun () -> ())
      |> ok
    in
    let code (error : P.Error.t) = error.code in
    let denied_code = Result.map_error denied ~f:code in
    let limited_code = Result.map_error limited ~f:code in
    let large_body = String.length large in
    (* This private copied unit exposes the bounded body parser without adding
       a production test hook. A check after the first consumed chunk must
       reject; a final remote-read check cannot satisfy this oracle. *)
    let body_reader = Eio.Buf_read.of_string (String.make 8193 'x') in
    let body_fields = String.Map.of_alist_exn [ "content-length", "8193" ] in
    let body_checkpoint_after_chunk =
      try
        ignore
          (Remote.body body_reader body_fields ~limit:8193 ~check:(fun () ->
             if Eio.Buf_read.consumed_bytes body_reader >= 4096
             then Remote.reject P.Error.Permission_denied "body owner revoked"));
        false
      with
      | Remote.Rejected error -> P.Error.equal_code error.code P.Error.Permission_denied
    in
    let malformed = fetch (origin ^ "/malformed") 8 in
    let malformed_code = Result.map_error malformed ~f:code in
    let exception Stop in
    let callback_identity =
      try
        ignore
          (Remote.read
             remote
             ~url:(origin ^ "/callback")
             ~max_bytes:8
             ~check_owner
             ~checkpoint:(fun () -> raise Stop)
             ~on_acquire:(fun () -> ()));
        false
      with
      | Stop -> true
    in
    let acquired_then_cancelled =
      let acquired = ref false in
      try
        ignore
          (Remote.read
             remote
             ~url:(origin ^ "/cancel")
             ~max_bytes:8
             ~check_owner
             ~checkpoint:(fun () -> if !acquired then raise Stop)
             ~on_acquire:(fun () -> acquired := true));
        false
      with
      | Stop -> !acquired
    in
    let observed_requests = !requested in
    let nonfinite_rejected =
      Result.is_error
        (Remote.Policy.create
           ~allowed_origins:[ origin ]
           ~max_response_bytes:8
           ~max_wall_seconds:Float.nan)
    in
    let nonloopback_rejected =
      Result.is_error
        (Remote.Policy.create
           ~allowed_origins:[ "http://example.com" ]
           ~max_response_bytes:8
           ~max_wall_seconds:5.)
    in
    print_s
      [%sexp
        (admitted : string)
      , (close_delimited : string)
      , (large_body : int)
      , (body_checkpoint_after_chunk : bool)
      , (malformed_code : (string, P.Error.code) result)
      , (callback_identity : bool)
      , (acquired_then_cancelled : bool)
      , (denied_before_acquisition : bool)
      , (denied_code : (string, P.Error.code) result)
      , (limited_code : (string, P.Error.code) result)
      , (observed_requests : int)
      , (nonfinite_rejected : bool)
      , (nonloopback_rejected : bool)];
    [%expect
      {|
      (hello bye 8193 true (Error Invalid_request) true true true
       (Error Permission_denied) (Error Resource_limit) 5 true true)
      |}])
;;
