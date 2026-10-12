open Core
module H = Shell_runtime.Chatml_extension
module L = Chatml.Chatml_lang
module S = Chatml_source
module Snapshot = Session.Snapshot

let body =
  {|let initial_state = Counter.seed
let match_command ev state =
  Task.bind(Match.yes(Array.get(Shell.argv(ev),0) ++ ":" ++ to_string(state)),
    fun ignored -> if state == 2 then Task.fail("rejected") else Task.pure(state+1))|}
;;

let compiled env =
  let contract =
    H.compiler_contract
      Shell_matcher
      ~entrypoint:"match_command"
      ~checkpoint:Support.checkpoint
    |> Support.ok
  in
  let input =
    Support.input
      contract
      [ "entry.chatml", body, [ "Counter", "counter.chatml" ]
      ; "counter.chatml", "let seed = 0", []
      ]
  in
  H.compile_prepared
    input
    ~script:(Support.script body)
    ~entrypoint:"match_command"
    ~limits:Support.compiler_limits
    ~max_dependency_depth:16
    ~env
    ()
  |> Result.map_error ~f:(fun errors ->
    List.map errors ~f:Chatmd_shell_spec.Diagnostic.to_string |> String.concat ~sep:"; ")
  |> Result.ok_or_failwith
;;

let instantiate env compiled lifecycle =
  match H.instantiate ~env ~lifecycle compiled with
  | Ok instance -> instance
  | Error error -> failwith (Chatmd_shell_spec.Diagnostic.to_string error)
;;

let call instance = H.call instance ~context:Support.context ~event:Support.event

let%test_unit "manifest matcher admission agrees with supported interceptor phases" =
  let module Shell = Chatmd_shell_spec.Shell_spec in
  let module Spec = Chatmd_shell_spec.Chatmd_script_spec in
  let module MC = Chatmd_shell_spec.Manifest_compiler in
  let matcher = Support.script "let initial_state = 0" in
  List.iter [ Shell.Before; After ] ~f:(fun phase ->
    let hook =
      { matcher with
        id = "hook"
      ; kind =
          (match phase with
           | Before -> Spec.Shell_before_interceptor
           | After -> Shell_after_interceptor)
      }
    in
    let base =
      Chatmd_shell_spec.Manifest_defaults.legacy_runtime ~source:matcher.source_ref
    in
    let runtime =
      { base with
        interceptors =
          Some
            { Shell.merge = Replace
            ; values =
                [ { Shell.id = "guarded"
                  ; phase
                  ; extension =
                      Chatml_extension { script = hook.id; lifecycle = Invocation }
                  ; matcher =
                      Some
                        (All
                           [ Any_command
                           ; Not
                               (Any
                                  [ Chatml_match
                                      { script = matcher.id
                                      ; function_ = "selective"
                                      ; failure = No_match_failure
                                      }
                                  ])
                           ])
                  ; failure = Error_failure
                  }
                ]
            }
      }
    in
    let compile scripts =
      MC.compile_with_material
        { runtimes = [ runtime ]
        ; tools = []
        ; scripts
        ; legacy_tools = []
        ; moderator_runtime = None
        ; platform = Macos
        ; supported_features = Chatmd_shell_spec.Feature.phase3
        }
    in
    match phase with
    | After ->
      (match compile [ matcher; hook ] with
       | Error diagnostics ->
         assert (
           List.exists diagnostics ~f:(fun diagnostic ->
             String.equal
               diagnostic.Chatmd_shell_spec.Diagnostic.code
               "shell.after_interceptor_match"))
       | Ok _ -> failwith "unsupported after predicate bypassed manifest validation")
    | Before ->
      let manifest, material = compile [ matcher; hook ] |> Support.ok in
      let active = MC.effective_scripts material ~manifest |> Support.ok in
      [%test_eq: string list]
        (List.map active ~f:(fun script -> script.Spec.id)
         |> List.sort ~compare:String.compare)
        (List.sort [ matcher.id; hook.id ] ~compare:String.compare);
      let effective =
        MC.effective_runtimes material ~manifest |> Support.ok |> List.hd_exn
      in
      let references = Chatmd_shell_spec.Script_reference.of_runtime effective in
      assert (
        List.exists references ~f:(fun reference ->
          String.equal reference.script matcher.id
          && Option.equal String.equal reference.entrypoint (Some "selective")));
      (match compile [ hook ] with
       | Error diagnostics ->
         assert (
           List.exists diagnostics ~f:(fun diagnostic ->
             String.equal
               diagnostic.Chatmd_shell_spec.Diagnostic.code
               "shell.unknown_script"))
       | Ok _ -> failwith "interceptor predicate escaped manifest script admission"))
;;

let%expect_test "prepared matcher names select independently compiled captured roles" =
  Eio_main.run (fun env ->
    let body =
      {|let initial_state = Counter.seed
let match_command ev state = Task.bind(Match.yes("default"), fun ignored -> Task.pure(state))
let selective ev state = Task.bind(Match.no("selected"), fun ignored -> Task.pure(state))|}
    in
    let script = Support.script body in
    let compile script entrypoint =
      let contract =
        H.compiler_contract Shell_matcher ~entrypoint ~checkpoint:Support.checkpoint
        |> Support.ok
      in
      H.compile_prepared
        (Support.input
           contract
           [ "entry.chatml", body, [ "Counter", "counter.chatml" ]
           ; "counter.chatml", "let seed = 0", []
           ])
        ~script
        ~entrypoint
        ~limits:Support.compiler_limits
        ~max_dependency_depth:16
        ~env
        ()
      |> Support.ok
    in
    let default = compile script "match_command" in
    let selected = compile script "selective" in
    let attach alternatives =
      H.with_prepared_entrypoints default ~alternatives ~checkpoint:Support.checkpoint
    in
    List.iter
      [ [ default ]
      ; [ selected; selected ]
      ; [ compile { script with id = "foreign" } "selective" ]
      ]
      ~f:(fun alternatives -> assert (Result.is_error (attach alternatives)));
    let choices = attach [ selected ] |> Support.ok in
    assert (
      Result.is_error
        (H.with_prepared_entrypoints
           choices
           ~alternatives:[]
           ~checkpoint:Support.checkpoint));
    assert (Result.is_error (H.with_entrypoint choices "missing"));
    let selected = H.with_entrypoint choices "selective" |> Support.ok in
    let restored_default = H.with_entrypoint selected "match_command" |> Support.ok in
    assert (String.equal (H.source_revision restored_default) (H.source_revision default));
    assert (not (String.equal (H.source_revision selected) (H.source_revision default)));
    List.iter [ choices; selected; restored_default ] ~f:(fun compiled ->
      print_s
        [%sexp (call (instantiate env compiled Invocation) |> Support.ok : H.action)]));
  [%expect
    {|
    (Match true default)
    (Match false selected)
    (Match true default)
  |}]
;;

let%expect_test
    "captured original hook receives event/state and commits under persistent control"
  =
  Eio_main.run (fun env ->
    let compiled = compiled env in
    [%test_eq: string]
      (H.source_sha256 compiled)
      (S.Digest.of_contents body ~checkpoint:Support.checkpoint |> S.Digest.to_hex);
    assert (phys_equal compiled (H.with_entrypoint compiled "match_command" |> Support.ok));
    assert (Result.is_error (H.with_entrypoint compiled "different"));
    let persistent = instantiate env compiled Session in
    print_s [%sexp (call persistent |> Support.ok : H.action)];
    print_s [%sexp (call persistent |> Support.ok : H.action)];
    let before = H.snapshot persistent |> Support.ok in
    assert (Result.is_error (call persistent));
    let after = H.snapshot persistent |> Support.ok in
    assert (Int.equal (Snapshot.compare before after) 0);
    let fresh = instantiate env compiled Runtime in
    print_s [%sexp (call fresh |> Support.ok : H.action)];
    H.restore fresh before |> Support.ok;
    assert (Result.is_error (call fresh));
    let invocation = instantiate env compiled Invocation in
    for _ = 1 to 2 do
      [%test_eq: H.action]
        (call invocation |> Support.ok)
        (H.Match (true, "actual-event:0"))
    done);
  [%expect
    {|
    (Match true actual-event:0)
    (Match true actual-event:1)
    (Match true actual-event:0)
  |}]
;;

let%test_unit "shell native role and body constraints fail closed before initialization" =
  Eio_main.run (fun env ->
    assert (
      Result.is_error
        (H.compiler_contract
           Moderator
           ~entrypoint:"on_event"
           ~checkpoint:Support.checkpoint));
    let contract =
      H.compiler_contract
        Shell_matcher
        ~entrypoint:"match_command"
        ~checkpoint:Support.checkpoint
      |> Support.ok
    in
    let input =
      Support.input
        contract
        [ "entry.chatml", body, [ "Counter", "counter.chatml" ]
        ; "counter.chatml", "let seed = 0", []
        ]
    in
    let script = Support.script body in
    let forged = { script with source_sha256 = String.make 64 '0' } in
    assert (
      Result.is_error
        (H.compile_prepared
           input
           ~script:forged
           ~entrypoint:"match_command"
           ~limits:Support.compiler_limits
           ~max_dependency_depth:16
           ~env
           ()));
    let edited = Support.script (body ^ " ") in
    assert (
      Result.is_error
        (H.compile_prepared
           input
           ~script:edited
           ~entrypoint:"match_command"
           ~limits:Support.compiler_limits
           ~max_dependency_depth:16
           ~env
           ())))
;;

exception Persist_interrupted of int ref

let%test_unit
    "prospective prepared state remains unchanged when persistence raises or cancels"
  =
  Eio_main.run (fun env ->
    let instance = instantiate env (compiled env) Session in
    let before = H.snapshot instance |> Support.ok in
    let identity = ref 19 in
    H.set_snapshot_handler instance (fun _ -> raise (Persist_interrupted identity));
    (match call instance with
     | _ -> assert false
     | exception Persist_interrupted actual -> assert (phys_equal identity actual));
    assert (Int.equal (Snapshot.compare before (H.snapshot instance |> Support.ok)) 0);
    H.set_snapshot_handler instance (fun _ ->
      raise (Eio.Cancel.Cancelled (Persist_interrupted identity)));
    (match call instance with
     | _ -> assert false
     | exception Eio.Cancel.Cancelled (Persist_interrupted actual) ->
       assert (phys_equal identity actual));
    assert (Int.equal (Snapshot.compare before (H.snapshot instance |> Support.ok)) 0);
    let persisted = ref None in
    (match
       Eio.Cancel.sub (fun cancel ->
         H.set_snapshot_handler instance (fun snapshot ->
           Eio.Cancel.cancel cancel (Persist_interrupted identity);
           (* The admitted write may yield under its narrow cancellation protection.
              Installation must follow without another cancellation checkpoint;
              joining the outer execution scope may then report cancellation. *)
           Eio.Fiber.yield ();
           persisted := Some snapshot;
           Ok ());
         ignore (call instance : (H.action, Chatmd_shell_spec.Diagnostic.t) Result.t))
     with
     | _ -> assert false
     | exception Eio.Cancel.Cancelled (Persist_interrupted actual) ->
       assert (phys_equal identity actual));
    assert (
      Int.equal
        (Snapshot.compare
           (Option.value_exn !persisted)
           (H.snapshot instance |> Support.ok))
        0);
    H.set_snapshot_handler instance (fun _ -> Ok ());
    [%test_eq: H.action] (call instance |> Support.ok) (H.Match (true, "actual-event:1")))
;;

let array_body =
  {|let initial_state = {cell = [0]}
let match_command ev state =
  let old = Array.get(state.cell,0) in
  Array.set(state.cell,0,old+1);
  Task.bind(Match.yes(to_string(old)), fun ignored ->
    if Array.get(Shell.argv(ev),0) == "actual-event" then Task.fail("rejected") else Task.pure(state))|}
;;

let compiled_array env body =
  let contract =
    H.compiler_contract
      Shell_matcher
      ~entrypoint:"match_command"
      ~checkpoint:Support.checkpoint
    |> Support.ok
  in
  let input = Support.input contract [ "entry.chatml", body, [] ] in
  H.compile_prepared
    input
    ~script:(Support.script body)
    ~entrypoint:"match_command"
    ~limits:Support.compiler_limits
    ~max_dependency_depth:16
    ~env
    ()
  |> Result.map_error ~f:(fun errors ->
    List.map errors ~f:Chatmd_shell_spec.Diagnostic.to_string |> String.concat ~sep:"; ")
  |> Result.ok_or_failwith
;;

let cell instance =
  match H.snapshot instance |> Support.ok with
  | Snapshot.Record fields ->
    (match List.Assoc.find fields ~equal:String.equal "state" with
     | Some (Record [ ("cell", Array [ Int value ]) ]) -> value
     | _ -> assert false)
  | _ -> assert false
;;

let commit_event =
  match Support.event with
  | L.VRecord fields ->
    L.VRecord (Map.set fields ~key:"argv" ~data:(L.VArray [| L.VString "commit" |]))
  | _ -> assert false
;;

let%test_unit "Legacy and Prepared persistence errors isolate nested mutable state" =
  Eio_main.run (fun env ->
    let constructors =
      [ (fun () -> H.compile ~script:(Support.script array_body) |> Support.ok)
      ; (fun () -> compiled_array env array_body)
      ]
    in
    List.iter constructors ~f:(fun compile ->
      let instance = instantiate env (compile ()) Session in
      H.set_snapshot_handler instance (fun _ -> Error "persistence rejected");
      assert (
        Result.is_error (H.call instance ~context:Support.context ~event:commit_event));
      [%test_eq: int] (cell instance) 0;
      H.set_snapshot_handler instance (fun _ -> Ok ());
      H.call instance ~context:Support.context ~event:commit_event |> Support.ok |> ignore;
      [%test_eq: int] (cell instance) 1))
;;

let%test_unit "prepared data state copy keeps nested array mutation transactional" =
  Eio_main.run (fun env ->
    let instance = instantiate env (compiled_array env array_body) Session in
    [%test_eq: int] (cell instance) 0;
    assert (Result.is_error (call instance));
    [%test_eq: int] (cell instance) 0;
    H.call instance ~context:Support.context ~event:commit_event |> Support.ok |> ignore;
    [%test_eq: int] (cell instance) 1;
    assert (Result.is_error (call instance));
    [%test_eq: int] (cell instance) 1)
;;

exception Hook_cancelled of int ref

let%test_unit
    "actual prepared hook computation responds to owner cancellation and retains state"
  =
  Eio_main.run (fun env ->
    let body =
      {|let initial_state = {cell = [0]}
let rec spin n = spin(n+1)
let match_command ev state = Array.set(state.cell,0,1); spin(0)|}
    in
    let limits = { H.default_limits with fuel = 1_000_000 } in
    let instance =
      H.instantiate ~env ~limits ~lifecycle:Session (compiled_array env body)
      |> Support.ok
    in
    let ready, resolver = Eio.Promise.create () in
    let identity = ref 23 in
    (match
       Eio.Cancel.sub (fun cancel ->
         Eio.Fiber.both
           (fun () ->
              Eio.Promise.await ready;
              Eio.Fiber.yield ();
              Eio.Cancel.cancel cancel (Hook_cancelled identity))
           (fun () ->
              Eio.Promise.resolve resolver ();
              ignore
                (H.call instance ~context:Support.context ~event:L.VUnit
                 : (H.action, Chatmd_shell_spec.Diagnostic.t) Result.t)))
     with
     | _ -> assert false
     | exception Eio.Cancel.Cancelled (Hook_cancelled actual) ->
       assert (phys_equal identity actual));
    [%test_eq: int] (cell instance) 0)
;;

let%test_unit "minimal prepared shell hook is usable under default compiler limits" =
  Eio_main.run (fun env ->
    let body = "let initial_state = 0 let match_command event state = Task.pure(state)" in
    let c =
      H.compiler_contract
        Shell_matcher
        ~entrypoint:"match_command"
        ~checkpoint:Support.checkpoint
      |> Support.ok
    in
    let input = Support.input c [ "entry.chatml", body, [] ] in
    match
      H.compile_prepared
        input
        ~script:(Support.script body)
        ~entrypoint:"match_command"
        ~limits:Chatml_compilation.default_limits
        ~max_dependency_depth:16
        ~env
        ()
    with
    | Ok _ -> ()
    | Error diagnostics ->
      let allocated = ref 0 in
      let canonical =
        Support.Contract.input_bytes c ~checkpoint:Support.checkpoint ~allocate:(fun n ->
          allocated := !allocated + n)
      in
      failwith
        (sprintf
           "Default native admission rejected canonical=%d conservative=%d: %s"
           canonical
           !allocated
           (List.map diagnostics ~f:Chatmd_shell_spec.Diagnostic.to_string
            |> String.concat ~sep:"; ")))
;;

let%expect_test "shell registry snapshots follow the complete prepared graph revision" =
  Eio_main.run (fun env ->
    let module Spec = Chatmd_shell_spec.Chatmd_script_spec in
    let module Shell = Chatmd_shell_spec.Shell_spec in
    let module Registry = Shell_runtime.Registry in
    let module MC = Chatmd_shell_spec.Manifest_compiler in
    let body =
      {|let initial_state = Counter.seed
let review ev state =
  Task.bind(Review.deny(to_string(state)), fun ignored -> Task.pure(state+1))|}
    in
    let script =
      { (Support.script body) with id = "reviewer"; kind = Spec.Shell_reviewer }
    in
    let contract =
      H.compiler_contract
        Shell_reviewer
        ~entrypoint:"review"
        ~checkpoint:Support.checkpoint
      |> Support.ok
    in
    let compile seed script =
      H.compile_prepared
        (Support.input
           contract
           [ "entry.chatml", body, [ "Counter", "counter.chatml" ]
           ; "counter.chatml", "let seed = " ^ Int.to_string seed, []
           ])
        ~script
        ~entrypoint:"review"
        ~limits:Support.compiler_limits
        ~max_dependency_depth:16
        ~env
        ()
    in
    let first = compile 0 script |> Support.ok in
    let changed = compile 10 script |> Support.ok in
    [%test_eq: string] (H.source_sha256 first) (H.source_sha256 changed);
    assert (not (String.equal (H.source_revision first) (H.source_revision changed)));
    assert (H.matches_script first script);
    assert (not (H.matches_script first { script with id = "other" }));
    assert (
      not
        (H.matches_script
           first
           { script with limits = { script.limits with fuel = script.limits.fuel - 1 } }));
    let selected =
      { script with
        project_selection =
          Some
            (Chatmd_shell_spec.Script_project.create
               ~project:"./chatml.project.json"
               ~environment:None
             |> Or_error.ok_exn)
      }
    in
    assert (not (H.matches_script first selected));
    (match H.compile ~script:selected with
     | Error [ diagnostic ] ->
       [%test_eq: string] diagnostic.code "shell.chatml_missing_program"
     | Ok _ | Error _ -> assert false);
    let root = Eio.Path.(Eio.Stdenv.cwd env / "native-shell-snapshot") in
    Eio.Path.mkdir ~perm:0o700 root;
    Exn.protect
      ~f:(fun () ->
        let runtime =
          Chatmd_shell_spec.Manifest_defaults.legacy_runtime ~source:script.source_ref
        in
        let runtime =
          { runtime with
            policy = Some { Shell.default = Set Ask; rules = []; merge = Replace }
          ; reviewers =
              Some
                { Shell.values =
                    [ Chatml_reviewer
                        { id = "review"
                        ; script = script.id
                        ; lifecycle = Session
                        ; failure = Error_failure
                        }
                    ]
                ; merge = Replace
                }
          }
        in
        let manifest, material =
          MC.compile_with_material
            { runtimes = [ runtime ]
            ; tools = []
            ; scripts = [ script ]
            ; legacy_tools = []
            ; moderator_runtime = None
            ; platform = Macos
            ; supported_features = Chatmd_shell_spec.Feature.phase3
            }
          |> Support.ok
        in
        let host =
          Shell_runtime.Host.
            { env
            ; workspace = root
            ; tool_dir = root
            ; prompt_dir = root
            ; session_dir = root
            ; cache_dir = root
            ; home = root
            ; source_dirs = String.Map.singleton script.source_ref.file root
            ; process_environment = [| "PATH=/usr/bin:/bin" |]
            ; session_id = "snapshot-registry"
            }
        in
        let prepare compiled =
          Registry.prepare_with_compiler
            ~compile_extension:(fun _ -> Ok compiled)
            ~admin_policy:Shell_runtime.Admin_policy.permissive
            ~manifest
            ~material
        in
        let compile_calls = ref 0 in
        let mismatched = { manifest with sha256 = String.make 64 '0' } in
        assert (
          Result.is_error
            (Registry.prepare_with_compiler_bounded
               material
               ~compile_extension:(fun _ ->
                 incr compile_calls;
                 Ok first)
               ~admin_policy:Shell_runtime.Admin_policy.permissive
               ~manifest:mismatched
               ~max_manifest_bytes:100000
               ~checkpoint:Support.checkpoint));
        assert (!compile_calls = 0);
        (match prepare (compile 0 { script with id = "different" } |> Support.ok) with
         | Error [ error ] ->
           [%test_eq: string] error.code "shell.chatml_compiled_mismatch"
         | Ok _ | Error _ -> assert false);
        let saved = ref [] in
        Eio.Switch.run (fun sw ->
          let prepared = prepare first |> Support.ok in
          let complete = Registry.prepared_manifest prepared in
          let oldgrant =
            Shell_runtime.Manifest_authorizer.authorize
              Shell_runtime.Manifest_authorizer.assume_authorized
              manifest
            |> Support.ok
          in
          assert (
            Result.is_error
              (Shell_runtime.Manifest_authorizer.authorize
                 Shell_runtime.Manifest_authorizer.deny
                 complete));
          let preflight_calls = ref 0 in
          (match
             Registry.instantiate_prepared_after
               ~sw
               ~host
               ~grant:oldgrant
               ~approval_provider:Shell_runtime.Approval_broker.None_available
               ~preflight:(fun _ ->
                 incr preflight_calls;
                 Ok ())
               prepared
           with
           | Error [ error ] ->
             [%test_eq: string] "shell.manifest_grant_mismatch" error.code
           | Error _ | Ok _ -> failwith "raw grant authorized a prepared graph");
          assert (!preflight_calls = 0);
          assert (List.is_empty !saved);
          let instantiate compiled snapshots =
            let prepared = prepare compiled |> Support.ok in
            let grant =
              Shell_runtime.Manifest_authorizer.authorize
                Shell_runtime.Manifest_authorizer.assume_authorized
                (Registry.prepared_manifest prepared)
              |> Support.ok
            in
            Registry.instantiate_prepared
              ~sw
              ~host
              ~grant
              ~approval_provider:Shell_runtime.Approval_broker.None_available
              ~extension_snapshots:snapshots
              ~persist_extension_snapshots:(fun snapshots ->
                saved := snapshots;
                Ok ())
              prepared
          in
          let review registry =
            let runtime = Registry.runtimes registry |> List.hd_exn in
            match
              Shell_access.Executor.run
                (Shell_runtime.Runtime.executor_config runtime)
                { request =
                    Shell_access.Request.command
                      (Shell_access.Command.create "/bin/echo" [])
                ; input = Empty
                ; rationale = None
                ; origin = Host "snapshot-test"
                }
            with
            | Error (Denied reason) -> reason
            | Error error -> failwith (Shell_access.Executor.error_to_string error)
            | Ok _ -> failwith "reviewer should deny before executing a process"
          in
          [%test_eq: string] (review (instantiate first [] |> Support.ok)) "0";
          let snapshot = List.hd_exn !saved in
          [%test_eq: string] snapshot.source_sha256 (H.source_revision first);
          [%test_eq: string] (review (instantiate first [ snapshot ] |> Support.ok)) "1";
          [%test_eq: string]
            (review (instantiate changed [ snapshot ] |> Support.ok))
            "10";
          (match instantiate first [ snapshot; snapshot ] with
           | Error [ error ] ->
             [%test_eq: string] error.code "shell.extension_snapshot_duplicate"
           | Ok _ | Error _ -> assert false);
          print_endline
            "same body; saved graph resumes at 1; changed dependency starts at 10"))
      ~finally:(fun () -> Eio.Cancel.protect (fun () -> Eio.Path.rmtree root)));
  [%expect {| same body; saved graph resumes at 1; changed dependency starts at 10 |}]
;;

let%test_unit "prepared native activation is one-shot and direct access is revocable" =
  Eio_main.run (fun env ->
    let compiled = compiled env in
    let pending = H.prepare_instance ~env ~lifecycle:Session compiled |> Support.ok in
    assert (Result.is_error (call pending));
    assert (Result.is_error (H.snapshot pending));
    H.activate pending |> Support.ok;
    H.activate pending |> Support.ok;
    [%test_eq: H.action] (call pending |> Support.ok) (H.Match (true, "actual-event:0"));
    H.close pending;
    H.close pending;
    assert (Result.is_error (H.activate pending));
    assert (Result.is_error (call pending));
    assert (Result.is_error (H.snapshot pending)))
;;
