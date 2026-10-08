open! Core
module H = Inference_host.Provider_profiles
module D = Openai.Responses_driver
module R = Inference.Request
module RT = Inference_runtime
module P = History_entry.Payload.Presence

let ok result = Result.map_error result ~f:(fun _ -> "fixture") |> Result.ok_or_failwith
let limits = Document_schema.Limits.default

let binding name =
  R.Auth_binding.create ~method_:"api_key" ~credential_reference:name ~limits |> ok
;;

let caps support =
  D.Capability.create
    ~baseline:[ Text_input, Supported; Setting "temperature", support ]
    ~models:[]
  |> ok
;;

let setting value =
  D.Setting.create ~name:"temperature" ~value ~provenance:Profile_default |> ok
;;

let profile
      ?(support = D.Capability.Supported)
      ?(defaults = [])
      ?(account = "account")
      ?(endpoint = "http://127.0.0.1:9/v1/responses")
      name
      revision
  =
  D.Profile.create
    ~id:name
    ~account:(Some account)
    ~endpoint
    ~capabilities:(caps support)
    ~defaults
  |> ok
  |> H.Profile.create ~revision ~binding:(binding name)
  |> ok
;;

let driver env = D.create ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.clock env) () |> ok

let registry env ~authorize ~credentials =
  H.create
    (driver env)
    ~authorize
    ~credentials
    ~status:(fun _ -> Available)
    ~limits:RT.Limits.default
;;

let allow ~principal:_ ~profile:_ ~account:_ ~binding:_ = true
let add host profile = H.add host profile ~owner:"runtime-host" ~generation:0L |> ok

let capture host name =
  H.capture host ~principal:"user" ~profile:name ~model:"model" ~settings:[] |> ok
;;

let prepare host target =
  H.resolve host ~principal:"user" target
  |> ok
  |> fun context ->
  RT.Context.prepare
    context
    ~preparation_id:"prepared"
    (R.create ~target ~history:[] ~tools:[] ~assets:[] ~limits |> ok)
  |> ok
;;

let run sw prepared =
  let scope =
    Transcript.Scope.create
      ~source:(Transcript.Source_id.of_string "test" |> ok)
      ~attempt:(Transcript.Attempt_id.of_string "1" |> ok)
      ~relation:Root
    |> ok
  in
  let accounting_id = Inference.Observation.Observation_id.of_string "test" |> ok in
  let attempt = RT.Prepared.start prepared ~scope ~accounting_id |> ok in
  RT.Attempt.run attempt ~sw ~on_event:ignore ~on_observation:ignore
  |> ok
  |> fun receipt ->
  let terminal = RT.Receipt.terminal receipt in
  print_s
    [%sexp
      (Inference.Event.Terminal.delivery terminal : Inference.Event.Terminal.delivery)
    , (Inference.Event.Terminal.outcome terminal : Inference.Event.Terminal.outcome)]
;;

let%expect_test "restore isolates defaults, binding presence and unknown members" =
  Eio_main.run (fun env ->
    let host =
      registry env ~authorize:allow ~credentials:(fun ~sw:_ _ -> Error D.Auth.Missing)
    in
    add host (profile ~defaults:[ setting (Value (`Number "0.2")) ] "one" "r1");
    add host (profile ~defaults:[ setting Null ] "two" "r1");
    let one = capture host "one"
    and two = capture host "two" in
    H.edit_profile host (profile ~defaults:[ setting (Value (`Number "0.9")) ] "one" "r2")
    |> ok;
    let restored = R.Target.of_json (R.Target.to_json one) ~limits |> ok in
    print_s [%sexp (Result.is_ok (H.resolve host ~principal:"user" restored) : bool)];
    List.iter
      [ restored; two; capture host "one" ]
      ~f:(fun target ->
        print_s
          [%sexp
            (List.map (R.Target.settings target) ~f:R.Setting.value : Jsonaf.t P.t list)]);
    List.iter [ P.Absent; P.Null ] ~f:(fun binding ->
      let target = R.Target.with_auth_binding restored ~binding ~limits |> ok in
      print_s
        [%sexp
          (Result.map (H.resolve host ~principal:"user" target) ~f:(fun _ -> ())
           : (unit, H.Error.t) Result.t)]);
    let raw =
      `Object
        [ "method", `String "api_key"
        ; "credential_reference", `String "one"
        ; "future", `Number "1.00"
        ]
    in
    let target =
      R.Target.with_auth_binding
        restored
        ~binding:(Value (R.Auth_binding.of_json raw ~limits |> ok))
        ~limits
      |> ok
    in
    let changed = R.Target.with_model target ~model:"new" ~limits |> ok in
    print_s
      [%sexp
        (P.equal
           R.Auth_binding.equal
           (R.Target.auth_binding target)
           (R.Target.auth_binding changed)
         : bool)]);
  [%expect
    {|
    true
    ((Value (Number 0.2)))
    (Null)
    ((Value (Number 0.9)))
    (Error Binding_unavailable)
    (Error Binding_unavailable)
    true
    |}]
;;

let%expect_test "dispatch revocation and stale capabilities never access credentials" =
  Eio_main.run (fun env ->
    Eio.Switch.run (fun sw ->
      let allowed = ref true
      and lookups = ref 0 in
      let host =
        registry
          env
          ~authorize:(fun ~principal:_ ~profile:_ ~account:_ ~binding:_ -> !allowed)
          ~credentials:(fun ~sw:_ _ ->
            incr lookups;
            D.Auth.bearer "secret")
      in
      add host (profile ~defaults:[ setting (Value (`Number "0.2")) ] "one" "r1");
      let target = capture host "one" in
      let prepared = prepare host target in
      allowed := false;
      run sw prepared;
      allowed := true;
      H.edit_profile host (profile ~support:Unsupported "one" "r2") |> ok;
      run sw prepared;
      let context = H.resolve host ~principal:"user" target |> ok in
      print_s
        [%sexp
          (Result.map
             (RT.Context.prepare
                context
                ~preparation_id:"again"
                (R.create ~target ~history:[] ~tools:[] ~assets:[] ~limits |> ok))
             ~f:(fun _ -> ())
           : (unit, RT.Preparation_error.t) Result.t)];
      print_s [%sexp (!lookups : int)]));
  [%expect
    {|
    (Definitely_not_submitted (Failed (Authentication Denied)))
    (Definitely_not_submitted (Failed (Authentication Profile_changed)))
    (Error Unsupported_input)
    0
    |}]
;;

let%expect_test "fresh generation, lookup race and disabled status are host owned" =
  Eio_main.run (fun env ->
    Eio.Switch.run (fun sw ->
      let host_ref = ref None in
      let host =
        registry env ~authorize:allow ~credentials:(fun ~sw:_ identity ->
          print_s [%sexp (identity : H.Credential_identity.t)];
          H.reauthorize
            (Option.value_exn !host_ref)
            ~profile:"one"
            ~owner:"new-owner"
            ~generation:2L
          |> ok;
          D.Auth.bearer "never-public-secret")
      in
      host_ref := Some host;
      add host (profile "one" "r1");
      let target = capture host "one" in
      let context = H.resolve host ~principal:"user" target |> ok in
      let prepare () =
        RT.Context.prepare
          context
          ~preparation_id:"prepared"
          (R.create ~target ~history:[] ~tools:[] ~assets:[] ~limits |> ok)
        |> ok
      in
      let prepared = prepare () in
      H.reauthorize host ~profile:"one" ~owner:"renewed-owner" ~generation:1L |> ok;
      run sw prepared;
      run sw (prepare ());
      H.disable host ~profile:"one" |> ok;
      print_s
        [%sexp
          (H.status host ~principal:"user" ~profile:"one"
           : (H.Status.t, H.Error.t) Result.t)];
      print_s
        [%sexp
          (H.edit_profile host (profile ~account:"different" "one" "r2")
           : (unit, H.Error.t) Result.t)]));
  [%expect
    {|
    (Definitely_not_submitted (Failed (Authentication Denied)))
    ((profile one) (account (account)) (method_ api_key)
     (credential_reference one) (owner renewed-owner) (generation 1))
    (Definitely_not_submitted (Failed (Authentication Denied)))
    (Ok
     ((identity
       ((profile one) (account (account)) (method_ api_key)
        (credential_reference one) (owner new-owner) (generation 2)))
      (availability Disabled)))
    (Error Incompatible_identity)
    |}]
;;

let%expect_test "two profiles dispatch separate bearer leases and captured settings" =
  Eio_main.run (fun env ->
    Eio.Switch.run (fun sw ->
      let socket =
        Eio.Net.listen
          ~sw
          ~reuse_addr:true
          ~backlog:4
          (Eio.Stdenv.net env)
          (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
      in
      let port =
        match Eio.Net.listening_addr socket with
        | `Tcp (_, port) -> port
        | _ -> assert false
      in
      let endpoint = sprintf "http://127.0.0.1:%d/v1/responses" port in
      let observations = ref [] in
      Eio.Fiber.fork_daemon ~sw (fun () ->
        while true do
          let flow, _ = Eio.Net.accept ~sw socket in
          let reader = Eio.Buf_read.of_flow flow ~max_size:1_000_000 in
          ignore (Eio.Buf_read.line reader : string);
          let rec headers length bearer =
            let line = Eio.Buf_read.line reader in
            if String.is_empty line
            then length, bearer
            else (
              match String.lsplit2 line ~on:':' with
              | Some (key, value) when String.Caseless.equal key "content-length" ->
                headers (Int.of_string (String.strip value)) bearer
              | Some (key, value) when String.Caseless.equal key "authorization" ->
                headers length (String.strip value)
              | None | Some _ -> headers length bearer)
          in
          let length, bearer = headers 0 "" in
          let body = Eio.Buf_read.take length reader |> Jsonaf.of_string in
          let selected =
            if String.equal bearer "Bearer one-private"
            then "one"
            else if String.equal bearer "Bearer two-private"
            then "two"
            else "wrong"
          in
          observations
          := !observations
             @ [ ( selected
                 , match Document_schema.Json.field body ~name:"temperature" with
                   | Absent -> P.Absent
                   | Null -> P.Null
                   | Value v -> P.Value v )
               ];
          let body =
            "data: \
             {\"type\":\"response.completed\",\"sequence_number\":0,\"response\":{\"object\":\"response\",\"id\":\"r\",\"status\":\"completed\",\"output\":[]}}\n\n"
          in
          Eio.Flow.copy_string
            (sprintf
               "HTTP/1.1 200 OK\r\n\
                Content-Type: text/event-stream\r\n\
                Content-Length: %d\r\n\
                Connection: close\r\n\
                \r\n\
                %s"
               (String.length body)
               body)
            flow;
          Eio.Flow.close flow
        done);
      let host =
        registry env ~authorize:allow ~credentials:(fun ~sw:_ identity ->
          D.Auth.bearer (H.Credential_identity.profile identity ^ "-private"))
      in
      add
        host
        (profile ~endpoint ~defaults:[ setting (Value (`Number "0.2")) ] "one" "r1");
      add host (profile ~endpoint ~defaults:[ setting Null ] "two" "r1");
      List.iter [ "one"; "two" ] ~f:(fun name ->
        run sw (prepare host (capture host name)));
      print_s [%sexp (!observations : (string * Jsonaf.t P.t) list)]));
  [%expect
    {|
    (Response_started Completed)
    (Response_started Completed)
    ((one (Value (Number 0.2))) (two Null))
    |}]
;;

let%expect_test "binding validation, denied status and removed IDs" =
  List.iter
    [ `Null
    ; `Object [ "method", `String "api_key" ]
    ; `Object [ "method", `String ""; "credential_reference", `String "one" ]
    ]
    ~f:(fun json ->
      print_s [%sexp (Result.is_error (R.Auth_binding.of_json json ~limits) : bool)]);
  Eio_main.run (fun env ->
    let callbacks = ref 0 in
    let host =
      H.create
        (driver env)
        ~authorize:(fun ~principal:_ ~profile:_ ~account:_ ~binding:_ -> false)
        ~credentials:(fun ~sw:_ _ ->
          incr callbacks;
          Error D.Auth.Missing)
        ~status:(fun _ ->
          incr callbacks;
          Available)
        ~limits:RT.Limits.default
    in
    add host (profile "one" "r1");
    print_s
      [%sexp
        (H.status host ~principal:"user" ~profile:"one"
         : (H.Status.t, H.Error.t) Result.t)];
    print_s [%sexp (!callbacks : int)];
    H.remove host ~profile:"one" |> ok;
    print_s
      [%sexp
        (H.add host (profile "one" "r1") ~owner:"runtime-host" ~generation:1L
         : (unit, H.Error.t) Result.t)]);
  [%expect
    {|
    true
    true
    true
    (Error Denied)
    0
    (Error Invalid_profile)
    |}]
;;

let%expect_test "lease guard runs after connection and before bearer headers" =
  Eio_main.run (fun env ->
    Eio.Switch.run (fun sw ->
      let socket =
        Eio.Net.listen
          ~sw
          ~reuse_addr:true
          ~backlog:1
          (Eio.Stdenv.net env)
          (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
      in
      let port =
        match Eio.Net.listening_addr socket with
        | `Tcp (_, port) -> port
        | _ -> assert false
      in
      let seen, resolver = Eio.Promise.create () in
      Eio.Fiber.fork ~sw (fun () ->
        let flow, _ = Eio.Net.accept ~sw socket in
        let bytes = Cstruct.create 1 in
        let received =
          try
            ignore (Eio.Flow.single_read flow bytes : int);
            true
          with
          | End_of_file -> false
        in
        Eio.Promise.resolve resolver received;
        Eio.Flow.close flow);
      let base =
        D.Profile.create
          ~id:"fixed"
          ~account:(Some "account")
          ~endpoint:(sprintf "http://127.0.0.1:%d/v1/responses" port)
          ~capabilities:(caps Supported)
          ~defaults:[]
        |> ok
      in
      let prepared =
        D.Prepared.create base ~model:"model" ~history:[] ~tools:[] ~settings:[] |> ok
      in
      let auth ~sw:_ _ =
        D.Auth.bearer "never-written"
        |> ok
        |> D.Auth.with_identity ~owner:"host" ~generation:0L ~check_current:(fun () ->
          Error D.Auth.Profile_changed)
      in
      print_s
        [%sexp
          (Result.map
             (D.run (driver env) ~auth ~prepared ~on_event:ignore)
             ~f:(fun _ -> ())
           : (unit, D.Auth.error) Result.t)];
      print_s [%sexp (Eio.Promise.await seen : bool)]));
  [%expect
    {|
    (Error Profile_changed)
    false
    |}]
;;

let%expect_test "registry wrapping preserves independent source revocation" =
  Eio_main.run (fun env ->
    Eio.Switch.run (fun sw ->
      let socket =
        Eio.Net.listen
          ~sw
          ~reuse_addr:true
          ~backlog:1
          (Eio.Stdenv.net env)
          (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
      in
      let port =
        match Eio.Net.listening_addr socket with
        | `Tcp (_, port) -> port
        | _ -> assert false
      in
      let seen, resolver = Eio.Promise.create () in
      Eio.Fiber.fork ~sw (fun () ->
        let flow, _ = Eio.Net.accept ~sw socket in
        let received =
          try
            ignore (Eio.Flow.single_read flow (Cstruct.create 1) : int);
            true
          with
          | End_of_file -> false
        in
        Eio.Promise.resolve resolver received;
        Eio.Flow.close flow);
      let source_valid = ref true
      and checks = ref 0 in
      let lease =
        D.Auth.bearer "source-secret"
        |> ok
        |> D.Auth.with_identity
             ~owner:"runtime-host"
             ~generation:0L
             ~check_current:(fun () ->
               incr checks;
               if !source_valid then Ok () else Error D.Auth.Denied)
        |> ok
      in
      let host = registry env ~authorize:allow ~credentials:(fun ~sw:_ _ -> Ok lease) in
      add
        host
        (profile ~endpoint:(sprintf "http://127.0.0.1:%d/v1/responses" port) "one" "r1");
      let prepared = prepare host (capture host "one") in
      source_valid := false;
      run sw prepared;
      print_s [%sexp (Eio.Promise.await seen : bool), (!checks : int)];
      print_s
        [%sexp
          (Result.is_error
             (D.Auth.with_identity
                lease
                ~owner:"different-owner"
                ~generation:0L
                ~check_current:(fun () -> Ok ()))
           : bool)]));
  [%expect
    {|
    (Definitely_not_submitted (Failed (Authentication Denied)))
    (false 1)
    true
    |}]
;;

let%expect_test "resolver retains recovery categories" =
  Eio_main.run (fun env ->
    let authorized = ref true
    and availability = ref H.Status.Available in
    let host =
      H.create
        (driver env)
        ~authorize:(fun ~principal:_ ~profile:_ ~account:_ ~binding:_ -> !authorized)
        ~credentials:(fun ~sw:_ _ -> Error D.Auth.Missing)
        ~status:(fun _ -> !availability)
        ~limits:RT.Limits.default
    in
    add host (profile "one" "r1");
    let target = capture host "one" in
    let report target =
      print_s
        [%sexp
          (Result.map (H.resolver host ~principal:"user" target) ~f:(fun _ -> ())
           : (unit, RT.Preparation_error.t) Result.t)]
    in
    report (R.Target.with_auth_binding target ~binding:Absent ~limits |> ok);
    availability := Reauthorization_required;
    report target;
    availability := Available;
    authorized := false;
    report target;
    authorized := true;
    report
      (R.Target.with_auth_binding target ~binding:(Value (binding "different")) ~limits
       |> ok);
    H.disable host ~profile:"one" |> ok;
    report target;
    H.remove host ~profile:"one" |> ok;
    report target);
  [%expect
    {|
    (Error Target_unavailable)
    (Error Reauthorization_required)
    (Error Target_denied)
    (Error Target_mismatch)
    (Error Target_unavailable)
    (Error Target_unavailable)
    |}]
;;

let%expect_test "disable and reenroll cancel old plan; same context prepares new epoch" =
  Eio_main.run (fun env ->
    Eio.Switch.run (fun sw ->
      let socket =
        Eio.Net.listen
          ~sw
          ~reuse_addr:true
          ~backlog:4
          (Eio.Stdenv.net env)
          (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
      in
      let port =
        match Eio.Net.listening_addr socket with
        | `Tcp (_, port) -> port
        | _ -> assert false
      in
      let endpoint = sprintf "http://127.0.0.1:%d/v1/responses" port in
      let connections = ref 0 in
      let bearer_bytes = ref 0 in
      Eio.Fiber.fork_daemon ~sw (fun () ->
        while true do
          let flow, _ = Eio.Net.accept ~sw socket in
          incr connections;
          let reader = Eio.Buf_read.of_flow flow ~max_size:1_000_000 in
          ignore (Eio.Buf_read.line reader : string);
          let rec headers length =
            let line = Eio.Buf_read.line reader in
            if String.is_empty line
            then length
            else (
              match String.lsplit2 line ~on:':' with
              | Some (key, value) when String.Caseless.equal key "content-length" ->
                headers (Int.of_string (String.strip value))
              | Some (key, value) when String.Caseless.equal key "authorization" ->
                bearer_bytes := !bearer_bytes + String.length (String.strip value);
                headers length
              | _ -> headers length)
          in
          ignore (Eio.Buf_read.take (headers 0) reader : string);
          let body =
            "data: \
             {\"type\":\"response.completed\",\"sequence_number\":0,\"response\":{\"object\":\"response\",\"id\":\"r\",\"status\":\"completed\",\"output\":[]}}\n\n"
          in
          Eio.Flow.copy_string
            (sprintf
               "HTTP/1.1 200 OK\r\n\
                Content-Type: text/event-stream\r\n\
                Content-Length: %d\r\n\
                Connection: close\r\n\
                \r\n\
                %s"
               (String.length body)
               body)
            flow;
          Eio.Flow.close flow
        done);
      let lookups = ref 0 in
      let host =
        registry env ~authorize:allow ~credentials:(fun ~sw:_ identity ->
          incr lookups;
          assert (Int64.equal (H.Credential_identity.generation identity) 1L);
          D.Auth.bearer "reenrolled-key")
      in
      add host (profile ~endpoint "one" "r1");
      let target = capture host "one" in
      let context = H.resolve host ~principal:"user" target |> ok in
      let prepare () =
        RT.Context.prepare
          context
          ~preparation_id:"prepared"
          (R.create ~target ~history:[] ~tools:[] ~assets:[] ~limits |> ok)
        |> ok
      in
      let old_plan = prepare () in
      H.disable host ~profile:"one" |> ok;
      print_s
        [%sexp
          (Result.map
             (RT.Context.prepare
                context
                ~preparation_id:"disabled"
                (R.create ~target ~history:[] ~tools:[] ~assets:[] ~limits |> ok))
             ~f:(fun _ -> ())
           : (unit, RT.Preparation_error.t) Result.t)];
      H.reauthorize host ~profile:"one" ~owner:"runtime-host" ~generation:1L |> ok;
      let new_plan = prepare () in
      run sw old_plan;
      print_s [%sexp (!lookups : int), (!connections : int), (!bearer_bytes : int)];
      run sw new_plan;
      run sw old_plan;
      print_s [%sexp (!lookups : int), (!connections : int), (!bearer_bytes > 0 : bool)]));
  [%expect
    {|
    (Error Target_unavailable)
    (Definitely_not_submitted (Failed (Authentication Denied)))
    (0 0 0)
    (Response_started Completed)
    (Definitely_not_submitted (Failed (Authentication Denied)))
    (1 1 true)
    |}]
;;
