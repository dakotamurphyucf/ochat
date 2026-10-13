open Core
open Support
module Authoring = Agent_server.Authoring_source_host
module Captured = Chatml_source
module Prepared = Chatml_prepared_program
module Path = Source.Portable_path
module P = Agent_protocol

let actor id =
  P.Principal.create
    ~id:(P.Id.Principal.of_string id |> ok)
    ~authentication_kind:"local.authoring_test"
    ~scopes:P.Scope.Set.empty
    ~attributes:[]
  |> ok
  |> Operator_authorization.trusted_local
;;

let authoring_ok = function
  | Ok value -> value
  | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
;;

let with_source_context f =
  with_workspace (fun env root_path data ->
    Eio.Path.save
      ~create:(`Or_truncate 0o600)
      Eio.Path.(root_path / "chatml.project.json")
      {|{"schema_version":1,"namespace":"authoring","entries":{"main":{"source":"main.chatml","target":"one_off_v1"}}}|};
    Eio.Path.save
      ~create:(`Or_truncate 0o600)
      Eio.Path.(root_path / "main.chatml")
      "let main = 1\n";
    Eio.Switch.run (fun sw ->
      let configuration =
        Configuration.{ external_roots = [ Eio.Path.native_exn root_path ]; git = None }
      in
      let sources = open_host ~configuration env data sw in
      let allowed = actor "pri_authoring_allowed" in
      let denied = actor "pri_authoring_denied" in
      let policy =
        Authoring.Read_policy.create ~authorize:(fun actor ~root_index ->
          Int.equal root_index 0
          && P.Id.Principal.equal
               (Operator_authorization.principal actor).id
               (Operator_authorization.principal allowed).id)
      in
      let limits =
        Authoring.Limits.create
          ~max_contexts:2
          ~max_context_requests:8
          ~max_manifest_entries:8
          ~max_browse_entries:32
          ~max_read_bytes:1024
          ~max_context_read_bytes:2048
        |> Or_error.ok_exn
      in
      let host = Authoring.create sources ~sw ~read_policy:policy ~limits in
      let root = Authoring.roots host ~actor:allowed ~checkpoint:ignore |> authoring_ok in
      [%test_eq: int] (List.length root) 1;
      [%test_eq: int]
        (List.length
           (Authoring.roots host ~actor:denied ~checkpoint:ignore |> authoring_ok))
        0;
      let root = List.hd_exn root in
      let project = Path.create "chatml.project.json" |> Or_error.ok_exn in
      let contract =
        Chatml.Chatml_compiler_contract.for_target One_off_v1 ~checkpoint:ignore
      in
      let context =
        Authoring.Context.open_root
          host
          ~actor:allowed
          ~root
          ~dependency_roots:[]
          ~project
          ~selection:(Entry { name = "main"; environment = None })
          ~contract
        |> authoring_ok
      in
      Exn.protect
        ~finally:(fun () ->
          Authoring.Context.close_and_wait context;
          Authoring.close_and_wait host;
          Host.close_and_wait sources)
        ~f:(fun () -> f env root context allowed denied sources host data root_path)))
;;

let%test_unit "authoring context captures a local project and reads only its grant" =
  with_source_context (fun env _ context allowed denied _ _ _ _ ->
    let capture =
      Authoring.Context.capture
        context
        ~actor:allowed
        ~clock:env#mono_clock
        ~checkpoint:ignore
      |> authoring_ok
    in
    let graph = Authoring.Capture.prepared capture in
    assert (
      Captured.Digest.equal
        (Authoring.Capture.fingerprint capture)
        (Prepared.fingerprint graph));
    assert (
      List.exists (Authoring.Capture.sources capture) ~f:(fun source ->
        String.equal (Captured.contents source) "let main = 1\n"));
    let entries =
      Authoring.Context.list_directory
        context
        ~actor:allowed
        ~directory:None
        ~checkpoint:ignore
      |> authoring_ok
    in
    assert (
      List.exists entries ~f:(fun entry ->
        String.equal (Path.to_string (Authoring.Entry.member entry)) "chatml.project.json"));
    let main = Path.create "main.chatml" |> Or_error.ok_exn in
    [%test_eq: string]
      (Authoring.Context.read_member
         context
         ~actor:allowed
         ~member:main
         ~checkpoint:ignore
       |> authoring_ok)
      "let main = 1\n";
    match
      Authoring.Context.read_member context ~actor:denied ~member:main ~checkpoint:ignore
    with
    | Error Authoring.Error.Unauthorized -> ()
    | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
    | Ok _ -> failwith "foreign actor read admitted")
;;

let%test_unit "same-context reentry rejects Busy before a second source borrow" =
  with_source_context (fun env _ context allowed _ _ _ _ _ ->
    let checked = ref false in
    let checkpoint () =
      if not !checked
      then (
        checked := true;
        let member = Path.create "main.chatml" |> Or_error.ok_exn in
        match
          Authoring.Context.read_member context ~actor:allowed ~member ~checkpoint:ignore
        with
        | Error Authoring.Error.Busy -> ()
        | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
        | Ok _ -> failwith "reentrant read admitted")
    in
    ignore
      (Authoring.Context.capture context ~actor:allowed ~clock:env#mono_clock ~checkpoint
       |> authoring_ok
       : Authoring.Capture.t);
    assert !checked)
;;

let%test_unit "concurrent read rejects Busy and close joins cancelled capture" =
  with_source_context (fun env _ context allowed _ _ _ _ _ ->
    Eio.Switch.run (fun sw ->
      let entered, resolve_entered = Eio.Promise.create () in
      let entered_once = ref false in
      let worker =
        Eio.Fiber.fork_promise ~sw (fun () ->
          try
            ignore
              (Authoring.Context.capture
                 context
                 ~actor:allowed
                 ~clock:env#mono_clock
                 ~checkpoint:(fun () ->
                   if not !entered_once
                   then (
                     entered_once := true;
                     Eio.Promise.resolve resolve_entered ();
                     Eio.Fiber.await_cancel ()))
               : (Authoring.Capture.t, Authoring.Error.t) Result.t);
            false
          with
          | Eio.Cancel.Cancelled _ -> true)
      in
      Eio.Promise.await entered;
      let member = Path.create "main.chatml" |> Or_error.ok_exn in
      (match
         Authoring.Context.read_member context ~actor:allowed ~member ~checkpoint:ignore
       with
       | Error Authoring.Error.Busy -> ()
       | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
       | Ok _ -> failwith "concurrent read admitted");
      Authoring.Context.close_and_wait context;
      assert (Eio.Promise.await_exn worker);
      match
        Authoring.Context.read_member context ~actor:allowed ~member ~checkpoint:ignore
      with
      | Error Authoring.Error.Closed -> ()
      | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
      | Ok _ -> failwith "closed context admitted read"))
;;

let%test_unit "reentrant close rejects self-join and leaves operation usable" =
  with_source_context (fun env _ context allowed _ _ _ _ _ ->
    let checked = ref false in
    let checkpoint () =
      if not !checked
      then (
        checked := true;
        (match Authoring.Context.close_and_wait context with
         | exception Invalid_argument _ -> ()
         | () -> failwith "reentrant context close admitted");
        let member = Path.create "main.chatml" |> Or_error.ok_exn in
        match
          Authoring.Context.read_member context ~actor:allowed ~member ~checkpoint:ignore
        with
        | Error Authoring.Error.Busy -> ()
        | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
        | Ok _ -> failwith "reentrant read admitted")
    in
    ignore
      (Authoring.Context.capture context ~actor:allowed ~clock:env#mono_clock ~checkpoint
       |> authoring_ok
       : Authoring.Capture.t);
    assert !checked)
;;

let%test_unit "host switch release closes retained contexts" =
  with_source_context (fun _ _ _ allowed _ sources _ _ _ ->
    let retained =
      Eio.Switch.run (fun sw ->
        let limits =
          Authoring.Limits.create
            ~max_contexts:1
            ~max_context_requests:1
            ~max_manifest_entries:8
            ~max_browse_entries:8
            ~max_read_bytes:1024
            ~max_context_read_bytes:1024
          |> Or_error.ok_exn
        in
        let policy =
          Authoring.Read_policy.create ~authorize:(fun _ ~root_index ->
            Int.equal root_index 0)
        in
        let host = Authoring.create sources ~sw ~read_policy:policy ~limits in
        let root =
          Authoring.roots host ~actor:allowed ~checkpoint:ignore
          |> authoring_ok
          |> List.hd_exn
        in
        let project = Path.create "chatml.project.json" |> Or_error.ok_exn in
        let contract =
          Chatml.Chatml_compiler_contract.for_target One_off_v1 ~checkpoint:ignore
        in
        Authoring.Context.open_root
          host
          ~actor:allowed
          ~root
          ~dependency_roots:[]
          ~project
          ~selection:(Entry { name = "main"; environment = None })
          ~contract
        |> authoring_ok)
    in
    let member = Path.create "main.chatml" |> Or_error.ok_exn in
    match
      Authoring.Context.read_member retained ~actor:allowed ~member ~checkpoint:ignore
    with
    | Error Authoring.Error.Closed -> ()
    | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
    | Ok _ -> failwith "switch-released context admitted read")
;;

let%test_unit "root enumeration rechecks grant before publication" =
  with_source_context (fun _ _ _ allowed _ sources _ _ _ ->
    Eio.Switch.run (fun sw ->
      let calls = ref 0 in
      let policy =
        Authoring.Read_policy.create ~authorize:(fun _ ~root_index:_ ->
          Int.incr calls;
          Int.equal !calls 1)
      in
      let limits =
        Authoring.Limits.create
          ~max_contexts:1
          ~max_context_requests:1
          ~max_manifest_entries:8
          ~max_browse_entries:8
          ~max_read_bytes:1024
          ~max_context_read_bytes:1024
        |> Or_error.ok_exn
      in
      let host = Authoring.create sources ~sw ~read_policy:policy ~limits in
      match Authoring.roots host ~actor:allowed ~checkpoint:ignore with
      | Error Authoring.Error.Unauthorized -> ()
      | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
      | Ok _ -> failwith "revoked root selection published"))
;;

let%test_unit "operator policy exception preserves identity" =
  with_source_context (fun _ _ _ allowed _ sources _ _ _ ->
    Eio.Switch.run (fun sw ->
      let exception Policy_raised in
      let policy =
        Authoring.Read_policy.create ~authorize:(fun _ ~root_index:_ ->
          raise Policy_raised)
      in
      let limits =
        Authoring.Limits.create
          ~max_contexts:1
          ~max_context_requests:1
          ~max_manifest_entries:8
          ~max_browse_entries:8
          ~max_read_bytes:1024
          ~max_context_read_bytes:1024
        |> Or_error.ok_exn
      in
      let host = Authoring.create sources ~sw ~read_policy:policy ~limits in
      match Authoring.roots host ~actor:allowed ~checkpoint:ignore with
      | exception Policy_raised -> ()
      | exception exn -> raise exn
      | Ok _ -> failwith "policy exception swallowed"
      | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)))
;;

let%test_unit "active context cannot close its host" =
  with_source_context (fun env _ context allowed _ _ host _ _ ->
    let checked = ref false in
    let checkpoint () =
      if not !checked
      then (
        checked := true;
        match Authoring.close_and_wait host with
        | exception Invalid_argument _ -> ()
        | () -> failwith "host self-join admitted")
    in
    ignore
      (Authoring.Context.capture context ~actor:allowed ~clock:env#mono_clock ~checkpoint
       |> authoring_ok
       : Authoring.Capture.t);
    assert !checked)
;;

let%test_unit "host close cancels and joins a yielding root enumeration" =
  with_source_context (fun _ _ _ allowed _ sources _ _ _ ->
    Eio.Switch.run (fun sw ->
      let limits =
        Authoring.Limits.create
          ~max_contexts:1
          ~max_context_requests:1
          ~max_manifest_entries:8
          ~max_browse_entries:8
          ~max_read_bytes:1024
          ~max_context_read_bytes:1024
        |> Or_error.ok_exn
      in
      let policy =
        Authoring.Read_policy.create ~authorize:(fun _ ~root_index:_ -> true)
      in
      let host = Authoring.create sources ~sw ~read_policy:policy ~limits in
      let entered, resolve_entered = Eio.Promise.create () in
      let worker =
        Eio.Fiber.fork_promise ~sw (fun () ->
          try
            ignore
              (Authoring.roots host ~actor:allowed ~checkpoint:(fun () ->
                 Eio.Promise.resolve resolve_entered ();
                 Eio.Fiber.await_cancel ())
               : (Authoring.Root.t list, Authoring.Error.t) Result.t);
            false
          with
          | Eio.Cancel.Cancelled _ -> true)
      in
      Eio.Promise.await entered;
      (match Authoring.roots host ~actor:allowed ~checkpoint:ignore with
       | Error Authoring.Error.Busy -> ()
       | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
       | Ok _ -> failwith "concurrent root enumeration admitted");
      Authoring.close_and_wait host;
      assert (Eio.Promise.await_exn worker);
      match Authoring.roots host ~actor:allowed ~checkpoint:ignore with
      | Error Authoring.Error.Closed -> ()
      | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
      | Ok _ -> failwith "closed host admitted root enumeration"))
;;

let%test_unit "root enumeration cannot self-close its host" =
  with_source_context (fun _ _ _ allowed _ sources _ _ _ ->
    Eio.Switch.run (fun sw ->
      let limits =
        Authoring.Limits.create
          ~max_contexts:1
          ~max_context_requests:1
          ~max_manifest_entries:8
          ~max_browse_entries:8
          ~max_read_bytes:1024
          ~max_context_read_bytes:1024
        |> Or_error.ok_exn
      in
      let policy =
        Authoring.Read_policy.create ~authorize:(fun _ ~root_index:_ -> true)
      in
      let host = Authoring.create sources ~sw ~read_policy:policy ~limits in
      let checked = ref false in
      let checkpoint () =
        if not !checked
        then (
          checked := true;
          match Authoring.close_and_wait host with
          | exception Invalid_argument _ -> ()
          | () -> failwith "root enumeration self-join admitted")
      in
      let roots = Authoring.roots host ~actor:allowed ~checkpoint |> authoring_ok in
      assert !checked;
      [%test_eq: int] (List.length roots) 1))
;;

let%test_unit "nested source contexts cannot hide an outer self-join" =
  with_source_context (fun env _ outer allowed _ _ _ data root_path ->
    Eio.Switch.run (fun sw ->
      let configuration =
        Configuration.{ external_roots = [ Eio.Path.native_exn root_path ]; git = None }
      in
      let sources = open_host ~configuration env data sw in
      let limits =
        Authoring.Limits.create
          ~max_contexts:1
          ~max_context_requests:2
          ~max_manifest_entries:8
          ~max_browse_entries:8
          ~max_read_bytes:1024
          ~max_context_read_bytes:1024
        |> Or_error.ok_exn
      in
      let policy =
        Authoring.Read_policy.create ~authorize:(fun _ ~root_index ->
          Int.equal root_index 0)
      in
      let host = Authoring.create sources ~sw ~read_policy:policy ~limits in
      let root =
        Authoring.roots host ~actor:allowed ~checkpoint:ignore
        |> authoring_ok
        |> List.hd_exn
      in
      let project = Path.create "chatml.project.json" |> Or_error.ok_exn in
      let contract =
        Chatml.Chatml_compiler_contract.for_target One_off_v1 ~checkpoint:ignore
      in
      let inner =
        Authoring.Context.open_root
          host
          ~actor:allowed
          ~root
          ~dependency_roots:[]
          ~project
          ~selection:(Entry { name = "main"; environment = None })
          ~contract
        |> authoring_ok
      in
      Exn.protect
        ~finally:(fun () ->
          Authoring.Context.close_and_wait inner;
          Authoring.close_and_wait host;
          Host.close_and_wait sources)
        ~f:(fun () ->
          let checked = ref false in
          let outer_checkpoint () =
            if not !checked
            then (
              checked := true;
              let inner_checkpoint () =
                match Authoring.Context.close_and_wait outer with
                | exception Invalid_argument _ -> ()
                | () -> failwith "nested call closed its active ancestor"
              in
              ignore
                (Authoring.Context.capture
                   inner
                   ~actor:allowed
                   ~clock:env#mono_clock
                   ~checkpoint:inner_checkpoint
                 |> authoring_ok
                 : Authoring.Capture.t))
          in
          ignore
            (Authoring.Context.capture
               outer
               ~actor:allowed
               ~clock:env#mono_clock
               ~checkpoint:outer_checkpoint
             |> authoring_ok
             : Authoring.Capture.t);
          assert !checked)))
;;

let%test_unit "owner switch release joins externally switched source work" =
  with_source_context (fun env _ _ allowed _ sources _ _ _ ->
    List.iter [ `Roots; `Capture ] ~f:(fun mode ->
      Eio.Switch.run (fun caller_sw ->
        let worker =
          Eio.Switch.run (fun owner_sw ->
            let limits =
              Authoring.Limits.create
                ~max_contexts:1
                ~max_context_requests:1
                ~max_manifest_entries:8
                ~max_browse_entries:8
                ~max_read_bytes:1024
                ~max_context_read_bytes:1024
              |> Or_error.ok_exn
            in
            let policy =
              Authoring.Read_policy.create ~authorize:(fun _ ~root_index ->
                Int.equal root_index 0)
            in
            let host =
              Authoring.create sources ~sw:owner_sw ~read_policy:policy ~limits
            in
            let context =
              match mode with
              | `Roots -> None
              | `Capture ->
                let root =
                  Authoring.roots host ~actor:allowed ~checkpoint:ignore
                  |> authoring_ok
                  |> List.hd_exn
                in
                let project = Path.create "chatml.project.json" |> Or_error.ok_exn in
                let contract =
                  Chatml.Chatml_compiler_contract.for_target One_off_v1 ~checkpoint:ignore
                in
                Some
                  (Authoring.Context.open_root
                     host
                     ~actor:allowed
                     ~root
                     ~dependency_roots:[]
                     ~project
                     ~selection:(Entry { name = "main"; environment = None })
                     ~contract
                   |> authoring_ok)
            in
            let entered, resolve_entered = Eio.Promise.create () in
            let worker =
              Eio.Fiber.fork_promise ~sw:caller_sw (fun () ->
                try
                  let checkpoint () =
                    Eio.Promise.resolve resolve_entered ();
                    Eio.Fiber.await_cancel ()
                  in
                  (match mode, context with
                   | `Roots, None ->
                     ignore
                       (Authoring.roots host ~actor:allowed ~checkpoint
                        : (Authoring.Root.t list, Authoring.Error.t) Result.t)
                   | `Capture, Some context ->
                     ignore
                       (Authoring.Context.capture
                          context
                          ~actor:allowed
                          ~clock:env#mono_clock
                          ~checkpoint
                        : (Authoring.Capture.t, Authoring.Error.t) Result.t)
                   | _ -> failwith "invalid lifecycle case");
                  false
                with
                | Eio.Cancel.Cancelled _ -> true)
            in
            Eio.Promise.await entered;
            worker)
        in
        assert (Eio.Promise.await_exn worker))))
;;

let%test_unit "grant identity, actor lifetime and read budgets fail closed" =
  with_source_context (fun _ foreign_root _ allowed _ sources _ _ root_path ->
    Eio.Path.save ~create:(`Or_truncate 0o600) Eio.Path.(root_path / "tiny.chatml") "abc";
    Eio.Switch.run (fun sw ->
      let limits =
        Authoring.Limits.create
          ~max_contexts:2
          ~max_context_requests:2
          ~max_manifest_entries:8
          ~max_browse_entries:8
          ~max_read_bytes:4
          ~max_context_read_bytes:3
        |> Or_error.ok_exn
      in
      let policy =
        Authoring.Read_policy.create ~authorize:(fun _ ~root_index ->
          Int.equal root_index 0)
      in
      let host = Authoring.create sources ~sw ~read_policy:policy ~limits in
      let root =
        Authoring.roots host ~actor:allowed ~checkpoint:ignore
        |> authoring_ok
        |> List.hd_exn
      in
      let project = Path.create "chatml.project.json" |> Or_error.ok_exn in
      let contract =
        Chatml.Chatml_compiler_contract.for_target One_off_v1 ~checkpoint:ignore
      in
      let open_context root actor =
        Authoring.Context.open_root
          host
          ~actor
          ~root
          ~dependency_roots:[]
          ~project
          ~selection:(Entry { name = "main"; environment = None })
          ~contract
      in
      (match open_context foreign_root allowed with
       | Error Authoring.Error.Foreign_root -> ()
       | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
       | Ok _ -> failwith "foreign root admitted");
      let current = ref true in
      let original =
        Operator_authorization.guarded
          ~principal:(Operator_authorization.principal allowed)
          ~is_current:(fun () -> !current)
      in
      let stale_root =
        Authoring.roots host ~actor:original ~checkpoint:ignore
        |> authoring_ok
        |> List.hd_exn
      in
      current := false;
      (match open_context stale_root allowed with
       | Error Authoring.Error.Expired_actor -> ()
       | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
       | Ok _ -> failwith "stale original proof admitted");
      let large = open_context root allowed |> authoring_ok in
      let aggregate = open_context root allowed |> authoring_ok in
      let main = Path.create "main.chatml" |> Or_error.ok_exn in
      let tiny = Path.create "tiny.chatml" |> Or_error.ok_exn in
      let expect_too_large context member =
        match
          Authoring.Context.read_member context ~actor:allowed ~member ~checkpoint:ignore
        with
        | Error (Authoring.Error.Local error)
          when Chatml_local_source.Error.equal_code
                 (Chatml_local_source.Error.code error)
                 Chatml_local_source.Error.Too_large -> ()
        | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
        | Ok _ -> failwith "oversized read admitted"
      in
      expect_too_large large main;
      expect_too_large large main;
      (match
         Authoring.Context.read_member
           large
           ~actor:allowed
           ~member:tiny
           ~checkpoint:ignore
       with
       | Error Authoring.Error.Result_limit -> ()
       | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
       | Ok _ -> failwith "request cap bypassed");
      [%test_eq: string]
        (Authoring.Context.read_member
           aggregate
           ~actor:allowed
           ~member:tiny
           ~checkpoint:ignore
         |> authoring_ok)
        "abc";
      (match
         Authoring.Context.read_member
           aggregate
           ~actor:allowed
           ~member:tiny
           ~checkpoint:ignore
       with
       | Error Authoring.Error.Result_limit -> ()
       | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
       | Ok _ -> failwith "aggregate read cap bypassed");
      Authoring.close_and_wait host))
;;
