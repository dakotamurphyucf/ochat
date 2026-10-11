open Core
open Support

let%expect_test "real pinned SHA1/SHA256 acquisition and offline cache" =
  with_workspace (fun env anchor _ _ _ store ->
    List.iter [ Host.Pin.Sha1; Sha256 ] ~f:(fun format ->
      let source, pin, oid, body = fixture env anchor format in
      let _, first =
        acquire store source pin ~limits:(limits ()) ~f:(fun repository ->
          ignore (expect_raw repository oid body);
          Ok ())
        |> admitted
      in
      assert (Host.Receipt.equal_disposition (Host.Receipt.disposition first) Fetched);
      assert (Host.Receipt.payload_bytes_reserved first = (4 * 2048) + (8 * 16_384));
      let route = native source in
      Eio.Path.rmtree source;
      let _, second =
        Host.with_cached_repository
          store
          ~repository:route
          ~pin
          ~limits:(limits ~payload:0 ())
          ~checkpoint:ignore
          ~f:(fun repository ->
            ignore (expect_raw repository oid body);
            Ok ())
        |> admitted
      in
      assert (Host.Receipt.equal_disposition (Host.Receipt.disposition second) Cached);
      assert (Host.Receipt.payload_bytes_reserved second = 0);
      print_s
        [%sexp
          (format : Host.Pin.object_format)
        , (Host.Receipt.disposition first : Host.Receipt.disposition)
        , (Host.Receipt.disposition second : Host.Receipt.disposition)
        , (body : string)]));
  [%expect
    {|
    (Sha1 Fetched Cached "let meaning = 42\n")
    (Sha256 Fetched Cached "let meaning = 42\n")
    |}]
;;

let%test_unit "callback rejection cannot publish; stale and escaped handles reject" =
  with_workspace (fun env anchor cache _ _ store ->
    let source, pin, oid, body = fixture env anchor Host.Pin.Sha1 in
    let escaped = ref None in
    let result =
      acquire store source pin ~limits:(limits ()) ~f:(fun repository ->
        escaped := Some repository;
        ignore (expect_raw repository oid body);
        Error ())
    in
    assert (
      match result with
      | Error (`Callback ()) -> true
      | Ok _ | Error (`Host _) -> false);
    assert (List.is_empty (Eio.Path.read_dir Eio.Path.(cache / "cache")));
    let repository = Option.value_exn !escaped in
    (match Host.Repository.checkpoint repository with
     | Error error -> assert (Host.Error.equal_code (Host.Error.code error) Closed)
     | Ok () -> failwith "escaped checkpoint admitted");
    (match
       Host.Repository.raw_object repository oid ~max_output_bytes:4096 ~checkpoint:ignore
     with
     | Error error -> assert (Host.Error.equal_code (Host.Error.code error) Closed)
     | Ok _ -> failwith "escaped repository admitted");
    let grant =
      Host.Grant.local ~repository:(native source) ~max_input_bytes:4096
      |> Or_error.ok_exn
    in
    let earlier = Host.Request.prepare store ~grant ~pin ~limits:(limits ()) |> ok in
    ignore
      (acquire store source pin ~limits:(limits ()) ~f:(fun repository ->
         ignore (expect_raw repository oid body);
         Ok ())
       |> admitted);
    expect_code
      (Host.with_acquisition earlier ~checkpoint:ignore ~f:(fun _ -> Ok ()))
      Stale_request)
;;

let%test_unit "literal reservations, bounded query poison and owner reentrancy" =
  with_workspace (fun env anchor cache _ _ store ->
    let source, pin, oid, body = fixture env anchor Host.Pin.Sha1 in
    let grant =
      Host.Grant.local ~repository:(native source) ~max_input_bytes:4096
      |> Or_error.ok_exn
    in
    let below =
      Host.Request.prepare store ~grant ~pin ~limits:(limits ~payload:139_263 ())
    in
    assert (
      match below with
      | Error error -> Host.Error.equal_code (Host.Error.code error) Limit_exceeded
      | Ok _ -> false);
    assert (List.is_empty (Eio.Path.read_dir Eio.Path.(cache / "stages")));
    let result =
      acquire store source pin ~limits:(limits ()) ~f:(fun repository ->
        expect_code
          (Host.with_cached_repository
             store
             ~repository:(native source)
             ~pin
             ~limits:(limits ())
             ~checkpoint:ignore
             ~f:(fun _ -> Ok ()))
          Busy;
        (match
           Host.Repository.raw_object
             repository
             oid
             ~max_output_bytes:1
             ~checkpoint:ignore
         with
         | Error _ -> ()
         | Ok _ -> failwith "oversize raw query admitted");
        (match
           Host.Repository.raw_object
             repository
             oid
             ~max_output_bytes:4096
             ~checkpoint:ignore
         with
         | Error _ -> ()
         | Ok _ -> failwith "poisoned query admitted");
        Ok ())
    in
    expect_code result Process_failed;
    assert (List.is_empty (Eio.Path.read_dir Eio.Path.(cache / "cache")));
    ignore
      (acquire store source pin ~limits:(limits ()) ~f:(fun repository ->
         ignore (expect_raw repository oid body);
         Ok ())
       |> admitted))
;;

exception Query_checkpoint

let%test_unit "abandoned concurrent query joins before cleanup and cannot publish" =
  with_workspace (fun env anchor cache _ _ store ->
    let source, pin, oid, body = fixture env anchor Host.Pin.Sha1 in
    let started, signal_started = Eio.Promise.create () in
    let joined, signal_joined = Eio.Promise.create () in
    Eio.Switch.run (fun outer_switch ->
      let result =
        acquire store source pin ~limits:(limits ()) ~f:(fun repository ->
          Eio.Fiber.fork ~sw:outer_switch (fun () ->
            Fun.protect
              ~finally:(fun () -> Eio.Promise.resolve signal_joined ())
              (fun () ->
                 try
                   ignore
                     (Host.Repository.raw_object
                        repository
                        oid
                        ~max_output_bytes:4096
                        ~checkpoint:(fun () ->
                          Eio.Promise.resolve signal_started ();
                          Eio.Fiber.await_cancel ()));
                   failwith "abandoned query succeeded"
                 with
                 | Eio.Cancel.Cancelled _ -> ()));
          Eio.Promise.await started;
          Ok ())
      in
      expect_code result Process_failed;
      Eio.Promise.await joined);
    assert (List.is_empty (Eio.Path.read_dir Eio.Path.(cache / "cache")));
    assert (List.is_empty (Eio.Path.read_dir Eio.Path.(cache / "stages")));
    ignore
      (acquire store source pin ~limits:(limits ()) ~f:(fun repository ->
         ignore (expect_raw repository oid body);
         Ok ())
       |> admitted))
;;

let%test_unit "query checkpoint identity survives owned-fiber cleanup" =
  with_workspace (fun env anchor cache _ _ store ->
    let source, pin, oid, body = fixture env anchor Host.Pin.Sha1 in
    let observed =
      try
        ignore
          (acquire store source pin ~limits:(limits ()) ~f:(fun repository ->
             ignore
               (Host.Repository.raw_object
                  repository
                  oid
                  ~max_output_bytes:4096
                  ~checkpoint:(fun () -> raise Query_checkpoint));
             Ok ()));
        false
      with
      | Query_checkpoint -> true
    in
    assert observed;
    assert (List.is_empty (Eio.Path.read_dir Eio.Path.(cache / "cache")));
    assert (List.is_empty (Eio.Path.read_dir Eio.Path.(cache / "stages")));
    ignore
      (acquire store source pin ~limits:(limits ()) ~f:(fun repository ->
         ignore (expect_raw repository oid body);
         Ok ())
       |> admitted))
;;

let%expect_test
    "HTTPS uses explicit private authorization and trusted TLS; cache stays offline"
  =
  with_workspace ~trust_fixture_certificate:true (fun env anchor _ _ _ store ->
    let source, pin, oid, body = fixture env anchor Host.Pin.Sha1 in
    let authorization = "Bearer fixture-only-private-token" in
    let requests = ref [] in
    let route =
      Eio.Switch.run (fun server_switch ->
        let endpoint =
          Https_fixture.start
            (Eio.Stdenv.net env)
            ~sw:server_switch
            ~handle:(fun request ->
              let module R = Https_fixture.Request in
              assert (Option.is_none (R.header request "Git-Protocol"));
              let method_ = R.method_ request in
              let target = R.target request in
              assert (
                Option.equal
                  String.equal
                  (R.header request "authorization")
                  (Some authorization));
              requests := (method_, target) :: !requests;
              let response, content_type =
                match method_, target with
                | "GET", "/repo/info/refs?service=git-upload-pack" ->
                  let advertisement =
                    run_git
                      env
                      source
                      ~arguments:
                        [ "upload-pack"; "--stateless-rpc"; "--advertise-refs"; "." ]
                      ~stdin:""
                  in
                  ( "001e# service=git-upload-pack\n0000" ^ advertisement
                  , "application/x-git-upload-pack-advertisement" )
                | "POST", "/repo/git-upload-pack" ->
                  ( run_git
                      env
                      source
                      ~arguments:[ "upload-pack"; "--stateless-rpc"; "." ]
                      ~stdin:(R.body request)
                  , "application/x-git-upload-pack-result" )
                | _ -> failwith "unexpected smart Git route"
              in
              { Https_fixture.Response.status = "200 OK"
              ; headers = [ "Content-Type", content_type ]
              ; body = response
              })
        in
        let route = endpoint ^ "/repo" in
        let grant =
          Host.Grant.https
            ~repository:route
            ~authorization:(Some ("Authorization: " ^ authorization))
            ~max_input_bytes:4096
          |> Or_error.ok_exn
        in
        let request = Host.Request.prepare store ~grant ~pin ~limits:(limits ()) |> ok in
        ignore
          (Host.with_acquisition request ~checkpoint:ignore ~f:(fun repository ->
             ignore (expect_raw repository oid body);
             Ok ())
           |> admitted);
        route)
    in
    assert (List.exists !requests ~f:(fun (method_, _) -> String.equal method_ "GET"));
    assert (List.exists !requests ~f:(fun (method_, _) -> String.equal method_ "POST"));
    let _, receipt =
      Host.with_cached_repository
        store
        ~repository:route
        ~pin
        ~limits:(limits ~payload:0 ())
        ~checkpoint:ignore
        ~f:(fun repository ->
          ignore (expect_raw repository oid body);
          Ok ())
      |> admitted
    in
    print_s
      [%sexp
        (List.rev !requests : (string * string) list)
      , (Host.Receipt.disposition receipt : Host.Receipt.disposition)]);
  [%expect
    {|
    (((GET /repo/info/refs?service=git-upload-pack) (POST /repo/git-upload-pack)
      (POST /repo/git-upload-pack))
     Cached)
    |}]
;;

let%test_unit
    "HTTPS redirects and certificate failures never publish or disclose authorization"
  =
  List.iter [ false; true ] ~f:(fun trust_fixture_certificate ->
    with_workspace ~trust_fixture_certificate (fun env anchor cache _ _ store ->
      let _, pin, _, _ = fixture env anchor Host.Pin.Sha1 in
      let headers_received = ref 0 in
      let result =
        Eio.Switch.run (fun server_switch ->
          let endpoint =
            Https_fixture.start (Eio.Stdenv.net env) ~sw:server_switch ~handle:(fun _ ->
              incr headers_received;
              { Https_fixture.Response.status = "302 Found"
              ; headers = [ "Location", "https://unadmitted.invalid/other" ]
              ; body = ""
              })
          in
          let grant =
            Host.Grant.https
              ~repository:(endpoint ^ "/repo")
              ~authorization:(Some "Authorization: Bearer private-test-only")
              ~max_input_bytes:4096
            |> Or_error.ok_exn
          in
          let request =
            Host.Request.prepare store ~grant ~pin ~limits:(limits ()) |> ok
          in
          Host.with_acquisition request ~checkpoint:ignore ~f:(fun _ -> Ok ()))
      in
      expect_code result Process_failed;
      assert (!headers_received = if trust_fixture_certificate then 1 else 0);
      assert (List.is_empty (Eio.Path.read_dir Eio.Path.(cache / "cache")));
      assert (List.is_empty (Eio.Path.read_dir Eio.Path.(cache / "stages")))))
;;

let%test_unit "literal Git file caps reject metadata and packs; cleanup permits reuse" =
  with_workspace (fun env anchor cache _ _ store ->
    let source, pin, oid, body = fixture env anchor Host.Pin.Sha1 in
    List.iter
      [ limits ~metadata:1 (); limits ~pack:1 () ]
      ~f:(fun limits ->
        let result =
          acquire store source pin ~limits ~f:(fun _ ->
            failwith "capped Git reached callback")
        in
        (match result with
         | Error (`Host _) -> ()
         | Error (`Callback _) | Ok _ ->
           failwith "literal native file cap was not enforced");
        assert (List.is_empty (Eio.Path.read_dir Eio.Path.(cache / "cache")));
        assert (List.is_empty (Eio.Path.read_dir Eio.Path.(cache / "stages"))));
    ignore
      (acquire store source pin ~limits:(limits ()) ~f:(fun repository ->
         ignore (expect_raw repository oid body);
         Ok ())
       |> admitted))
;;

let%test_unit "aggregate raw output allowance is shared across queries" =
  with_workspace (fun env anchor cache _ _ store ->
    let source, pin, oid, body = fixture env anchor Host.Pin.Sha1 in
    ignore
      (acquire store source pin ~limits:(limits ()) ~f:(fun repository ->
         ignore (expect_raw repository oid body);
         Ok ())
       |> admitted);
    let expected = Host.Object_id.to_string oid ^ " blob 17\n" ^ body ^ "\n" in
    let one = String.length expected in
    let successful = ref 0 in
    let result =
      Host.with_cached_repository
        store
        ~repository:(native source)
        ~pin
        ~limits:(limits ~payload:0 ~output:((2 * one) - 1) ())
        ~checkpoint:ignore
        ~f:(fun repository ->
          List.iter [ (); () ] ~f:(fun () ->
            match
              Host.Repository.raw_object
                repository
                oid
                ~max_output_bytes:4096
                ~checkpoint:ignore
            with
            | Ok raw ->
              assert (String.equal raw expected);
              incr successful
            | Error _ -> ());
          Ok ())
    in
    assert (!successful = 1);
    expect_code result Process_failed;
    assert (List.length (Eio.Path.read_dir Eio.Path.(cache / "cache")) = 1))
;;

let%test_unit
    "malformed cached configuration is rejected before invoking query checkpoint"
  =
  with_workspace (fun env anchor cache _ _ store ->
    let source, pin, oid, body = fixture env anchor Host.Pin.Sha1 in
    ignore
      (acquire store source pin ~limits:(limits ()) ~f:(fun repository ->
         ignore (expect_raw repository oid body);
         Ok ())
       |> admitted);
    let key = Eio.Path.read_dir Eio.Path.(cache / "cache") |> List.hd_exn in
    let config = Eio.Path.(cache / "cache" / key / "config") in
    let original = Eio.Path.load config in
    Eio.Path.save
      ~create:(`Or_truncate 0o600)
      config
      (original ^ "[include]\n\tpath = /unadmitted/config\n");
    let invoked = ref false in
    let result =
      Host.with_cached_repository
        store
        ~repository:(native source)
        ~pin
        ~limits:(limits ~payload:0 ())
        ~checkpoint:ignore
        ~f:(fun _ ->
          invoked := true;
          Ok ())
    in
    expect_code result Cache_corrupt;
    assert (not !invoked);
    Eio.Path.save ~create:(`Or_truncate 0o600) config original;
    ignore
      (Host.with_cached_repository
         store
         ~repository:(native source)
         ~pin
         ~limits:(limits ~payload:0 ())
         ~checkpoint:ignore
         ~f:(fun repository ->
           ignore (expect_raw repository oid body);
           Ok ())
       |> admitted))
;;

exception Owner_checkpoint

let%test_unit "whole owner interruption sweep joins descriptors without GC or publication"
  =
  with_workspace (fun env anchor _ _ deployment _ ->
    let source, pin, oid, body = fixture env anchor Host.Pin.Sha1 in
    let serial = ref 0 in
    let run checkpoint =
      incr serial;
      let directory = Eio.Path.(anchor / ("sweep-" ^ Int.to_string !serial)) in
      Eio.Path.mkdir ~perm:0o700 directory;
      let outcome =
        try
          Eio.Path.with_open_dir directory (fun root ->
            Eio.Switch.run (fun sw ->
              let store =
                Host.Store.open_root
                  root
                  ~sw
                  ~deployment
                  ~clock:(Eio.Stdenv.mono_clock env)
                  ~limits:store_limits
                  ~checkpoint
                |> ok
              in
              ignore
                (acquire
                   ~checkpoint
                   store
                   source
                   pin
                   ~limits:(limits ())
                   ~f:(fun repository ->
                     ignore
                       (Host.Repository.raw_object
                          repository
                          oid
                          ~max_output_bytes:4096
                          ~checkpoint
                        |> ok);
                     Ok ())
                 |> admitted)));
          `Completed
        with
        | Owner_checkpoint -> `Interrupted
      in
      (match outcome with
       | `Completed -> ()
       | `Interrupted ->
         List.iter [ "cache"; "stages" ] ~f:(fun name ->
           let child = Eio.Path.(directory / name) in
           if Eio.Path.is_directory child
           then assert (List.is_empty (Eio.Path.read_dir child))));
      outcome
    in
    let observed = ref 0 in
    ignore (run (fun () -> incr observed));
    assert (!observed > 16);
    let descriptors () =
      Eio.Path.read_dir Eio.Path.(Eio.Stdenv.fs env / "/dev/fd") |> List.length
    in
    let before = descriptors () in
    let interrupted = ref 0 in
    for stop = 1 to !observed do
      let calls = ref 0 in
      let outcome =
        run (fun () ->
          incr calls;
          if !calls = stop then raise Owner_checkpoint)
      in
      (match outcome with
       | `Interrupted -> incr interrupted
       | `Completed ->
         (* Joined process polling can differ by scheduling. A position that this
            run never reaches has no injected failure; it must fully succeed. *)
         assert (!calls < stop));
      assert (descriptors () = before)
    done;
    assert (!interrupted > 16);
    ignore (run (fun () -> ())))
;;

let%test_unit "store switch close rejects admission and waits for its active bracket" =
  with_workspace (fun env anchor _ _ deployment _ ->
    let source, pin, _, _ = fixture env anchor Host.Pin.Sha1 in
    let grant =
      Host.Grant.local ~repository:(native source) ~max_input_bytes:4096
      |> Or_error.ok_exn
    in
    let root = Eio.Path.(anchor / "concurrent-store") in
    Eio.Path.mkdir ~perm:0o700 root;
    Eio.Path.with_open_dir root (fun root ->
      Eio.Switch.run (fun test_switch ->
        let admitted_store, signal_store = Eio.Promise.create () in
        let inside, signal_inside = Eio.Promise.create () in
        let close_requested, request_close = Eio.Promise.create () in
        let released, signal_released = Eio.Promise.create () in
        let continue_callback, signal_continue = Eio.Promise.create () in
        let finished, signal_finished = Eio.Promise.create () in
        Eio.Fiber.fork ~sw:test_switch (fun () ->
          Eio.Switch.run (fun store_switch ->
            let store =
              Host.Store.open_root
                root
                ~sw:store_switch
                ~deployment
                ~clock:(Eio.Stdenv.mono_clock env)
                ~limits:store_limits
                ~checkpoint:ignore
              |> ok
            in
            Eio.Promise.resolve signal_store store;
            Eio.Promise.await close_requested);
          Eio.Promise.resolve signal_released ());
        let store = Eio.Promise.await admitted_store in
        Eio.Fiber.fork ~sw:test_switch (fun () ->
          let result =
            acquire store source pin ~limits:(limits ()) ~f:(fun _ ->
              Eio.Promise.resolve signal_inside ();
              Eio.Promise.await continue_callback;
              Error ())
          in
          assert (
            match result with
            | Error (`Callback ()) -> true
            | Error (`Host _) | Ok _ -> false);
          Eio.Promise.resolve signal_finished ());
        Eio.Promise.await inside;
        Eio.Promise.resolve request_close ();
        let rec await_closing () =
          match Host.Request.prepare store ~grant ~pin ~limits:(limits ()) with
          | Error error when Host.Error.equal_code (Host.Error.code error) Closed -> ()
          | Error error when Host.Error.equal_code (Host.Error.code error) Busy ->
            Eio.Fiber.yield ();
            await_closing ()
          | Error _ | Ok _ -> failwith "owner did not reject closing admission"
        in
        await_closing ();
        assert (Option.is_none (Eio.Promise.peek released));
        Eio.Promise.resolve signal_continue ();
        Eio.Promise.await finished;
        Eio.Promise.await released)))
;;

let%test_unit "unknown helper inventory cannot acquire deployment authority" =
  with_workspace (fun env anchor _ _ _ _ ->
    let helper = Eio.Path.(anchor / "helpers") in
    Eio.Path.save
      ~create:(`Exclusive 0o600)
      Eio.Path.(helper / "git-http-fetch")
      "not admitted";
    Eio.Path.with_open_dir helper (fun root ->
      let result =
        Host.Deployment.admit_git_2_56_0
          root
          ~native_helper_directory:(native helper)
          ~native_git_executable:(Filename.concat (native helper) "git")
          ~native_ca_bundle:None
          ~clock:(Eio.Stdenv.mono_clock env)
          ~probe_limits:(process_limits ())
          ~max_deployment_bytes:4096
          ~checkpoint:ignore
      in
      match result with
      | Error _ -> ()
      | Ok _ -> failwith "extra helper authority admitted"))
;;

let%test_unit "unexpected v2 advertisement rejects before any URI or fetch request" =
  with_workspace ~trust_fixture_certificate:true (fun env anchor cache _ _ store ->
    let _, pin, _, _ = fixture env anchor Host.Pin.Sha1 in
    let packet bytes = sprintf "%04x%s" (String.length bytes + 4) bytes in
    let pack_header = "PACK\000\000\000\002\000\000\000\000" in
    let empty_pack =
      pack_header ^ Digestif.SHA1.(to_raw_string (digest_string pack_header))
    in
    let fetch_requests = ref 0 in
    let uri_requests = ref 0 in
    let result =
      Eio.Switch.run (fun server_switch ->
        let endpoint = ref "" in
        let address =
          Https_fixture.start
            (Eio.Stdenv.net env)
            ~sw:server_switch
            ~handle:(fun request ->
              let module R = Https_fixture.Request in
              let body, content_type =
                match R.method_ request, R.target request with
                | "GET", "/repo/info/refs?service=git-upload-pack" ->
                  ( packet "version 2\n"
                    ^ packet "ls-refs=unborn\n"
                    ^ packet "fetch=shallow packfile-uris\n"
                    ^ packet "object-format=sha1\n"
                    ^ "0000"
                  , "application/x-git-upload-pack-advertisement" )
                | "POST", "/repo/git-upload-pack"
                  when String.is_substring (R.body request) ~substring:"command=ls-refs"
                  ->
                  ( packet (Host.Pin.commit pin ^ " refs/heads/main\n") ^ "0000"
                  , "application/x-git-upload-pack-result" )
                | "POST", "/repo/git-upload-pack"
                  when String.is_substring (R.body request) ~substring:"command=fetch" ->
                  incr fetch_requests;
                  assert (
                    not (String.is_substring (R.body request) ~substring:"packfile-uris "));
                  ( packet "packfile-uris\n"
                    ^ packet (String.make 40 '0' ^ " " ^ !endpoint ^ "/uri-pack\n")
                    ^ "0001"
                    ^ packet "packfile\n"
                    ^ packet ("\001" ^ empty_pack)
                    ^ "0000"
                  , "application/x-git-upload-pack-result" )
                | "GET", "/uri-pack" ->
                  incr uri_requests;
                  empty_pack, "application/octet-stream"
                | _ -> failwith "unexpected hostile Git fixture route"
              in
              { Https_fixture.Response.status = "200 OK"
              ; headers = [ "Content-Type", content_type ]
              ; body
              })
        in
        endpoint := address;
        let grant =
          Host.Grant.https
            ~repository:(address ^ "/repo")
            ~authorization:None
            ~max_input_bytes:4096
          |> Or_error.ok_exn
        in
        let request =
          Host.Request.prepare store ~grant ~pin ~limits:(limits ~wall_time:1. ()) |> ok
        in
        Host.with_acquisition request ~checkpoint:ignore ~f:(fun _ ->
          failwith "unsolicited URI reached verification"))
    in
    assert (!fetch_requests = 0);
    assert (!uri_requests = 0);
    (match result with
     | Error (`Host error) ->
       assert (
         List.mem
           [ Host.Error.Process_failed; Timed_out ]
           (Host.Error.code error)
           ~equal:Host.Error.equal_code)
     | Error (`Callback _) | Ok _ -> failwith "unexpected v2 response admitted");
    assert (List.is_empty (Eio.Path.read_dir Eio.Path.(cache / "cache")));
    assert (List.is_empty (Eio.Path.read_dir Eio.Path.(cache / "stages"))))
;;

let%test_unit "stock Git protocol v0 transfers SHA256 over real HTTPS" =
  with_workspace ~trust_fixture_certificate:true (fun env anchor _ _ _ _ ->
    let source, pin, oid, body = fixture env anchor Host.Pin.Sha256 in
    let client = Eio.Path.(anchor / "v0-sha256-client") in
    Eio.Path.mkdir ~perm:0o700 client;
    ignore
      (run_git
         env
         client
         ~arguments:[ "init"; "--bare"; "--quiet"; "--object-format=sha256" ]
         ~stdin:"");
    let requested_v0 = ref 0 in
    Eio.Switch.run (fun server_switch ->
      let endpoint =
        Https_fixture.start (Eio.Stdenv.net env) ~sw:server_switch ~handle:(fun request ->
          assert (Option.is_none (Https_fixture.Request.header request "Git-Protocol"));
          incr requested_v0;
          smart_git_response env source request)
      in
      let route = endpoint ^ "/repo" in
      ignore
        (run_git
           env
           client
           ~arguments:
             [ "-c"
             ; "protocol.version=0"
             ; "-c"
             ; "http.sslVerify=true"
             ; "-c"
             ; "http.sslCAInfo=" ^ native Eio.Path.(anchor / "test-ca.pem")
             ; "-c"
             ; "fetch.fsckObjects=true"
             ; "-c"
             ; "pack.writeReverseIndex=false"
             ; "remote-https"
             ; route
             ; route
             ]
           ~stdin:
             ("capabilities\n\
               option depth 1\n\
               option progress false\n\
               option object-format true\n\
               fetch "
              ^ Host.Pin.commit pin
              ^ " "
              ^ Host.Pin.commit pin
              ^ "\n\n\n")));
    assert (!requested_v0 >= 2);
    let raw =
      run_git
        env
        client
        ~arguments:[ "cat-file"; "--batch" ]
        ~stdin:(Host.Object_id.to_string oid ^ "\n")
    in
    assert (String.equal raw (Host.Object_id.to_string oid ^ " blob 17\n" ^ body ^ "\n")))
;;

let%test_unit "qualified host profile captures full SHA256 pin over HTTPS" =
  with_workspace ~trust_fixture_certificate:true (fun env anchor _ _ _ store ->
    let source, pin, oid, body = fixture env anchor Host.Pin.Sha256 in
    let requests = ref 0 in
    let route =
      Eio.Switch.run (fun server_switch ->
        let endpoint =
          Https_fixture.start
            (Eio.Stdenv.net env)
            ~sw:server_switch
            ~handle:(fun request ->
              assert (Option.is_none (Https_fixture.Request.header request "Git-Protocol"));
              incr requests;
              smart_git_response env source request)
        in
        let route = endpoint ^ "/repo" in
        let grant =
          Host.Grant.https ~repository:route ~authorization:None ~max_input_bytes:4096
          |> Or_error.ok_exn
        in
        let request = Host.Request.prepare store ~grant ~pin ~limits:(limits ()) |> ok in
        ignore
          (Host.with_acquisition request ~checkpoint:ignore ~f:(fun repository ->
             ignore (expect_raw repository oid body);
             Ok ())
           |> admitted);
        route)
    in
    assert (!requests >= 2);
    ignore
      (Host.with_cached_repository
         store
         ~repository:route
         ~pin
         ~limits:(limits ~payload:0 ())
         ~checkpoint:ignore
         ~f:(fun repository ->
           ignore (expect_raw repository oid body);
           Ok ())
       |> admitted))
;;

let%test_unit "private authorization is absent from argv and non-fetch environments" =
  let module Profile = Git_host_profile in
  let deployment =
    Profile.Deployment.inputs
      ~native_helper_directory:"/host/helpers"
      ~native_git_executable:"/host/helpers/git"
      ~native_ca_bundle:None
      ~max_bytes:4096
    |> function
    | Ok deployment -> deployment
    | Error error -> raise_s [%sexp (error : Profile.Error.t)]
  in
  let private_marker = "Authorization: Bearer fixture-private-marker" in
  let grant =
    Profile.Grant.https
      ~repository:"https://host.invalid/repo"
      ~authorization:(Some private_marker)
      ~max_input_bytes:4096
    |> Or_error.ok_exn
  in
  let pin =
    Host.Pin.create ~object_format:Sha1 ~commit:(String.make 40 'a') |> Or_error.ok_exn
  in
  let id = Profile.Object_id.of_pin pin in
  List.iter
    [ Profile.Init Sha1; Raw_object id; Fetch (grant, pin) ]
    ~f:(fun command ->
      let arguments = Profile.arguments deployment ~workspace:"/store/active" command in
      assert (
        not
          (List.exists arguments ~f:(fun text ->
             String.is_substring text ~substring:private_marker)));
      let environment =
        Profile.environment deployment ~workspace:"/store/active" command
      in
      match command with
      | Fetch _ ->
        assert (
          Option.equal
            String.equal
            (List.Assoc.find environment "GIT_CONFIG_VALUE_0" ~equal:String.equal)
            (Some private_marker))
      | Version | Init _ | Raw_object _ ->
        assert (
          not
            (List.exists environment ~f:(fun (_, value) ->
               String.is_substring value ~substring:private_marker))))
;;

let%test_unit "copy boundaries preserve exact bytes and reject growth after inventory" =
  with_workspace (fun _ anchor _ _ _ _ ->
    let module Storage = Git_host_storage in
    let storage_ok = function
      | Ok value -> value
      | Error error -> raise_s [%sexp (error : Git_host_native.Code.t)]
    in
    List.iter [ 0; 1; 4095; 4096; 4097; 8192 ] ~f:(fun bytes ->
      let source_path = Eio.Path.(anchor / ("copy-source-" ^ Int.to_string bytes)) in
      let target_path = Eio.Path.(anchor / ("copy-target-" ^ Int.to_string bytes)) in
      Eio.Path.mkdir ~perm:0o700 source_path;
      Eio.Path.mkdir ~perm:0o700 target_path;
      Eio.Path.mkdir ~perm:0o700 Eio.Path.(source_path / "nested");
      let payload = String.init bytes ~f:(fun i -> Char.of_int_exn (i * 17 % 256)) in
      Eio.Path.save
        ~create:(`Exclusive 0o600)
        Eio.Path.(source_path / "nested" / "content")
        payload;
      Eio.Path.with_open_dir source_path (fun source ->
        Eio.Path.with_open_dir target_path (fun target ->
          let input = Storage.borrow_root source ~checkpoint:ignore |> storage_ok in
          let output = Storage.borrow_root target ~checkpoint:ignore |> storage_ok in
          Fun.protect
            ~finally:(fun () ->
              Storage.close output;
              Storage.close input)
            (fun () ->
               let inventory =
                 Storage.inventory
                   input
                   ~max_entries:2
                   ~max_depth:1
                   ~max_payload_bytes:bytes
                   ~max_work_steps:128
                   ~checkpoint:ignore
                 |> storage_ok
               in
               assert (Storage.Inventory.payload_bytes inventory = bytes);
               Storage.copy_inventory
                 input
                 ~destination:output
                 inventory
                 ~checkpoint:ignore
               |> storage_ok;
               assert (
                 String.equal
                   (Eio.Path.load Eio.Path.(target_path / "nested" / "content"))
                   payload)))));
    let source_path = Eio.Path.(anchor / "changed-source") in
    let target_path = Eio.Path.(anchor / "changed-target") in
    Eio.Path.mkdir ~perm:0o700 source_path;
    Eio.Path.mkdir ~perm:0o700 target_path;
    let member = Eio.Path.(source_path / "content") in
    Eio.Path.save ~create:(`Exclusive 0o600) member (String.make 4096 'x');
    Eio.Path.with_open_dir source_path (fun source ->
      Eio.Path.with_open_dir target_path (fun target ->
        let input = Storage.borrow_root source ~checkpoint:ignore |> storage_ok in
        let output = Storage.borrow_root target ~checkpoint:ignore |> storage_ok in
        Fun.protect
          ~finally:(fun () ->
            Storage.close output;
            Storage.close input)
          (fun () ->
             let inventory =
               Storage.inventory
                 input
                 ~max_entries:1
                 ~max_depth:0
                 ~max_payload_bytes:4096
                 ~max_work_steps:128
                 ~checkpoint:ignore
               |> storage_ok
             in
             Eio.Path.save ~create:(`Or_truncate 0o600) member (String.make 4097 'x');
             match
               Storage.copy_inventory
                 input
                 ~destination:output
                 inventory
                 ~checkpoint:ignore
             with
             | Error code -> assert (Git_host_native.Code.equal code Corrupt)
             | Ok () -> failwith "changed source length copied"))))
;;

let%test_unit "managed cache root itself consumes an entry allowance" =
  with_workspace (fun env anchor _ _ deployment _ ->
    let path = Eio.Path.(anchor / "entry-boundary-store") in
    Eio.Path.mkdir ~perm:0o700 path;
    Eio.Path.mkdir ~perm:0o700 Eio.Path.(path / "cache");
    Eio.Path.mkdir ~perm:0o700 Eio.Path.(path / "stages");
    Eio.Path.mkdir ~perm:0o700 Eio.Path.(path / "cache" / String.make 64 'a');
    let limits =
      Host.Store_limits.create
        ~max_cache_repositories:1
        ~max_cache_payload_bytes:0
        ~max_managed_entries:3
        ~max_path_bytes:4096
        ~max_work_steps:1024
      |> Or_error.ok_exn
    in
    Eio.Path.with_open_dir path (fun root ->
      Eio.Switch.run (fun sw ->
        match
          Host.Store.open_root
            root
            ~sw
            ~deployment
            ~clock:(Eio.Stdenv.mono_clock env)
            ~limits
            ~checkpoint:ignore
        with
        | Error error ->
          assert (Host.Error.equal_code (Host.Error.code error) Limit_exceeded)
        | Ok _ -> failwith "fourth managed entry admitted with allowance three")))
;;

let%test_unit "oversized live grants and cached routes reject before hashing" =
  with_workspace (fun _ _ cache _ _ store ->
    let pin =
      Host.Pin.create ~object_format:Sha1 ~commit:(String.make 40 'a') |> Or_error.ok_exn
    in
    let route = "https://host.invalid/" ^ String.make 65_536 'x' in
    let grant =
      Host.Grant.https ~repository:route ~authorization:None ~max_input_bytes:100_000
      |> Or_error.ok_exn
    in
    (match Host.Request.prepare store ~grant ~pin ~limits:(limits ()) with
     | Error error ->
       assert (Host.Error.equal_code (Host.Error.code error) Limit_exceeded)
     | Ok _ -> failwith "oversized grant admitted");
    let polls = ref 0 in
    expect_code
      (Host.with_cached_repository
         store
         ~repository:route
         ~pin
         ~limits:(limits ())
         ~checkpoint:(fun () -> incr polls)
         ~f:(fun _ -> failwith "oversized cached route admitted"))
      Limit_exceeded;
    assert (!polls = 0);
    assert (List.is_empty (Eio.Path.read_dir Eio.Path.(cache / "stages"))))
;;

let%test_unit "finite v0 pack, index and shallow boundary witnesses" =
  with_workspace ~trust_fixture_certificate:true (fun env anchor cache _ _ store ->
    let source, pin, oid, body = fixture env anchor Host.Pin.Sha1 in
    let run ~pack ~shallow ~cap ~callback =
      let model = Adversarial_fixture.create ~pin ~pack ~shallow in
      let called = ref false in
      let indexed_sizes = ref Int.Set.empty in
      let observe () =
        let directory =
          Eio.Path.(cache / "stages" / "active" / "repo" / "objects" / "pack")
        in
        if Eio.Path.is_directory directory
        then
          List.iter (Eio.Path.read_dir directory) ~f:(fun name ->
            if String.is_suffix name ~suffix:".idx"
            then
              indexed_sizes
              := Set.add
                   !indexed_sizes
                   (Optint.Int63.to_int
                      (Eio.Path.stat ~follow:false Eio.Path.(directory / name)).size))
      in
      let result =
        Eio.Switch.run (fun sw ->
          let endpoint =
            Https_fixture.start
              (Eio.Stdenv.net env)
              ~sw
              ~handle:
                (Adversarial_fixture.response
                   model
                   ~stock:(smart_git_response env source))
          in
          let grant =
            Host.Grant.https
              ~repository:(endpoint ^ "/repo")
              ~authorization:None
              ~max_input_bytes:4096
            |> Or_error.ok_exn
          in
          let request =
            Host.Request.prepare store ~grant ~pin ~limits:(limits ~pack:cap ()) |> ok
          in
          Host.with_acquisition request ~checkpoint:observe ~f:(fun repo ->
            called := true;
            callback repo;
            Error ()))
      in
      assert (List.is_empty (Eio.Path.read_dir Eio.Path.(cache / "cache")));
      assert (List.is_empty (Eio.Path.read_dir Eio.Path.(cache / "stages")));
      result, !called, model, !indexed_sizes
    in
    let semantic_rejected = function
      | Error (`Callback ()) -> ()
      | Error (`Host error) ->
        raise_s [%sexp "native stage did not reach callback", (error : Host.Error.t)]
      | Ok _ -> failwith "fixture callback rejection ignored"
    in
    let stock, called, model, _ =
      run ~pack:Stock ~shallow:Stock ~cap:2048 ~callback:(fun repo ->
        ignore (expect_raw repo oid body))
    in
    semantic_rejected stock;
    assert called;
    assert (Adversarial_fixture.supplied_pack model);
    (* Empty SHA1 index: 8 header + 1024 fanout + two 20-byte hashes = 1072.
       Its handbuilt pack is only 32 bytes; the index is the dominant file. *)
    List.iter [ 1071; 1072 ] ~f:(fun cap ->
      let result, called, model, indexed_sizes =
        run ~pack:Empty ~shallow:Stock ~cap ~callback:(fun repo ->
          let missing =
            Host.Repository.raw_object
              repo
              (Host.Object_id.create ~object_format:Sha1 (Host.Pin.commit pin)
               |> Or_error.ok_exn)
              ~max_output_bytes:4096
              ~checkpoint:ignore
            |> ok
          in
          assert (String.equal missing (Host.Pin.commit pin ^ " missing\n")))
      in
      assert (Adversarial_fixture.supplied_pack model);
      assert (not called);
      (match result with
       | Error (`Host _) -> ()
       | Error (`Callback ()) | Ok _ ->
         failwith "empty pack unexpectedly supplied requested commit");
      assert (Bool.equal (Set.mem indexed_sizes 1072) (cap = 1072)));
    let malformed, called, model, _ =
      run ~pack:Malformed ~shallow:Stock ~cap:2048 ~callback:(fun _ ->
        failwith "malformed pack reached semantic callback")
    in
    assert (Adversarial_fixture.supplied_pack model);
    assert (not called);
    (match malformed with
     | Error (`Host _) -> ()
     | _ -> failwith "malformed pack admitted");
    let stock, called, _, _ =
      run ~pack:Stock ~shallow:Stock ~cap:2048 ~callback:(fun repo ->
        ignore (expect_raw repo oid body))
    in
    semantic_rejected stock;
    assert called;
    let generous, called, model, _ =
      run ~pack:Stock ~shallow:Excessive ~cap:4096 ~callback:(fun repo ->
        ignore (expect_raw repo oid body))
    in
    semantic_rejected generous;
    assert called;
    assert (Adversarial_fixture.supplied_shallow model);
    assert (Adversarial_fixture.supplied_pack model);
    (* 65 independent shallow lines of 41 bytes exceed 2048, while the
       stock pack/index/control above fit that same native cap. *)
    let excessive, called, model, _ =
      run ~pack:Stock ~shallow:Excessive ~cap:2048 ~callback:(fun _ ->
        failwith "excessive shallow reached callback")
    in
    assert (Adversarial_fixture.supplied_shallow model);
    assert (Adversarial_fixture.supplied_pack model);
    assert (not called);
    match excessive with
    | Error (`Host _) -> ()
    | _ -> failwith "excessive shallow admitted")
;;

let%test_unit "exact acquisition reservation and raw output boundaries" =
  with_workspace (fun env anchor cache _ _ store ->
    let source, pin, oid, body = fixture env anchor Host.Pin.Sha1 in
    let grant =
      Host.Grant.local ~repository:(native source) ~max_input_bytes:4096
      |> Or_error.ok_exn
    in
    List.iter [ 139_263; 139_264; 139_265 ] ~f:(fun payload ->
      match Host.Request.prepare store ~grant ~pin ~limits:(limits ~payload ()) with
      | Error error ->
        assert (payload = 139_263);
        assert (Host.Error.equal_code (Host.Error.code error) Limit_exceeded)
      | Ok request ->
        assert (payload >= 139_264);
        let called = ref false in
        let result =
          Host.with_acquisition request ~checkpoint:ignore ~f:(fun repo ->
            called := true;
            ignore (expect_raw repo oid body);
            Error ())
        in
        assert !called;
        (match result with
         | Error (`Callback ()) -> ()
         | _ -> failwith "reservation control failed"));
    let raw_bytes =
      String.length (Host.Object_id.to_string oid ^ " blob 17\n" ^ body ^ "\n")
    in
    List.iter
      [ raw_bytes - 1; raw_bytes; raw_bytes + 1 ]
      ~f:(fun maximum ->
        let checked = ref false in
        let result =
          acquire store source pin ~limits:(limits ()) ~f:(fun repo ->
            checked := true;
            match
              Host.Repository.raw_object
                repo
                oid
                ~max_output_bytes:maximum
                ~checkpoint:ignore
            with
            | Ok raw ->
              assert (maximum >= raw_bytes);
              assert (String.length raw = raw_bytes);
              Error ()
            | Error error ->
              assert (maximum < raw_bytes);
              assert (Host.Error.equal_code (Host.Error.code error) Limit_exceeded);
              Ok ())
        in
        assert !checked;
        if maximum >= raw_bytes
        then (
          match result with
          | Error (`Callback ()) -> ()
          | _ -> failwith "exact raw control failed")
        else (
          match result with
          | Error (`Host _) -> ()
          | _ -> failwith "raw cap did not poison"));
    assert (List.is_empty (Eio.Path.read_dir Eio.Path.(cache / "cache"))))
;;

let%test_unit "store scans are admitted before large entry allowances" =
  with_workspace (fun env anchor _ _ deployment _ ->
    let path = Eio.Path.(anchor / "small-work-store") in
    Eio.Path.mkdir ~perm:0o700 path;
    Eio.Path.mkdir ~perm:0o700 Eio.Path.(path / "cache");
    Eio.Path.mkdir ~perm:0o700 Eio.Path.(path / "stages");
    List.iter
      (List.init 64 ~f:(fun i -> sprintf "%064x" i))
      ~f:(fun name -> Eio.Path.mkdir ~perm:0o700 Eio.Path.(path / "cache" / name));
    let store_limits =
      Host.Store_limits.create
        ~max_cache_repositories:Int.max_value
        ~max_cache_payload_bytes:Int.max_value
        ~max_managed_entries:Int.max_value
        ~max_path_bytes:4096
        ~max_work_steps:32
      |> Or_error.ok_exn
    in
    let polls = ref 0 in
    Eio.Path.with_open_dir path (fun root ->
      Eio.Switch.run (fun sw ->
        match
          Host.Store.open_root
            root
            ~sw
            ~deployment
            ~clock:(Eio.Stdenv.mono_clock env)
            ~limits:store_limits
            ~checkpoint:(fun () -> incr polls)
        with
        | Error error ->
          assert (Host.Error.equal_code (Host.Error.code error) Limit_exceeded)
        | Ok _ -> failwith "unadmitted cache scan succeeded"));
    assert (!polls <= 12))
;;

let%test_unit "workspace entries are reserved before setup for fetched and cached paths" =
  with_workspace (fun env anchor _ _ deployment _ ->
    let source, pin, oid, body = fixture env anchor Host.Pin.Sha1 in
    let path = Eio.Path.(anchor / "workspace-entry-store") in
    Eio.Path.mkdir ~perm:0o700 path;
    let with_store maximum f =
      let limits =
        Host.Store_limits.create
          ~max_cache_repositories:8
          ~max_cache_payload_bytes:1_048_576
          ~max_managed_entries:maximum
          ~max_path_bytes:4096
          ~max_work_steps:100_000
        |> Or_error.ok_exn
      in
      Eio.Path.with_open_dir path (fun root ->
        Eio.Switch.run (fun sw ->
          let store =
            Host.Store.open_root
              root
              ~sw
              ~deployment
              ~clock:(Eio.Stdenv.mono_clock env)
              ~limits
              ~checkpoint:ignore
            |> ok
          in
          f store))
    in
    let assert_no_workspace () =
      assert (List.is_empty (Eio.Path.read_dir Eio.Path.(path / "stages")))
    in
    with_store 66 (fun store ->
      let grant =
        Host.Grant.local ~repository:(native source) ~max_input_bytes:4096
        |> Or_error.ok_exn
      in
      let request = Host.Request.prepare store ~grant ~pin ~limits:(limits ()) |> ok in
      expect_code
        (Host.with_acquisition request ~checkpoint:assert_no_workspace ~f:(fun _ ->
           failwith "insufficient fetched entry capacity admitted"))
        Limit_exceeded);
    with_store 67 (fun store ->
      ignore
        (acquire store source pin ~limits:(limits ()) ~f:(fun repo ->
           ignore (expect_raw repo oid body);
           Ok ())
         |> admitted));
    let rec count path =
      List.fold (Eio.Path.read_dir path) ~init:0 ~f:(fun total name ->
        let member = Eio.Path.(path / name) in
        let children =
          match (Eio.Path.stat ~follow:false member).kind with
          | `Directory -> count member
          | `Regular_file -> 0
          | _ -> failwith "unexpected managed fixture member"
        in
        total + 1 + children)
    in
    let entries = count path in
    (* Five free entries admit the stored inventory but cannot admit the six
       workspace entries. Both cached entrypoints must reject before creation. *)
    with_store (entries + 5) (fun store ->
      expect_code
        (Host.with_cached_repository
           store
           ~repository:(native source)
           ~pin
           ~limits:(limits ())
           ~checkpoint:assert_no_workspace
           ~f:(fun _ -> failwith "insufficient cached entry capacity admitted"))
        Limit_exceeded;
      let grant =
        Host.Grant.local ~repository:(native source) ~max_input_bytes:4096
        |> Or_error.ok_exn
      in
      let request = Host.Request.prepare store ~grant ~pin ~limits:(limits ()) |> ok in
      expect_code
        (Host.with_acquisition request ~checkpoint:assert_no_workspace ~f:(fun _ ->
           failwith "cached acquisition bypassed entry capacity"))
        Limit_exceeded);
    assert_no_workspace ())
;;

let%expect_test "pure semantic checkpoints share the acquisition deadline" =
  with_workspace (fun env anchor _ _ deployment _ ->
    let source, pin, _, _ = fixture env anchor Host.Pin.Sha1 in
    let path = Eio.Path.(anchor / "deadline-store") in
    Eio.Path.mkdir ~perm:0o700 path;
    Eio.Path.with_open_dir path (fun root ->
      Eio.Switch.run (fun sw ->
        let clock = Eio_mock.Clock.Mono.make () in
        let store =
          Host.Store.open_root
            root
            ~sw
            ~deployment
            ~clock
            ~limits:store_limits
            ~checkpoint:ignore
          |> ok
        in
        let observed = ref None in
        let result =
          acquire store source pin ~limits:(limits ()) ~f:(fun repo ->
            assert (Result.is_ok (Host.Repository.checkpoint repo));
            Eio_mock.Clock.Mono.set_time clock (Mtime.of_uint64_ns 20_000_000_000L);
            (match Host.Repository.checkpoint repo with
             | Error error -> observed := Some (Host.Error.code error)
             | Ok () -> failwith "expired semantic checkpoint admitted");
            (* Catching the checkpoint failure cannot authorize publication. *)
            Ok ())
        in
        assert (Option.equal Host.Error.equal_code !observed (Some Timed_out));
        expect_code result Timed_out;
        let published =
          not (List.is_empty (Eio.Path.read_dir Eio.Path.(path / "cache")))
        in
        assert (not published);
        print_s
          [%sexp { checkpoint = (!observed : Host.Error.code option); published : bool }])));
  [%expect
    {|
    +mock time is now 20
    ((checkpoint (Timed_out)) (published false))
    |}]
;;
