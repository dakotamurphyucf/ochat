open Core
module H = Shell_runtime.Chatml_extension
module Registry = Shell_runtime.Registry
module Spec = Chatmd_shell_spec.Chatmd_script_spec
module Shell = Chatmd_shell_spec.Shell_spec
module Tool = Chatmd_shell_spec.Shell_tool_spec
module MC = Chatmd_shell_spec.Manifest_compiler

let ok = function
  | Ok value -> value
  | Error _ -> failwith "unexpected activation fixture rejection"
;;

let with_root env f =
  let root = Eio.Path.(Eio.Stdenv.cwd env / "shell-activation-fixture") in
  Eio.Path.mkdir ~perm:0o700 root;
  Exn.protect
    ~f:(fun () -> f root)
    ~finally:(fun () -> Eio.Cancel.protect (fun () -> Eio.Path.rmtree root))
;;

let source_ref body =
  let position = Chatmd_shell_spec.Source_ref.{ offset = 0; line = 1; column = 0 } in
  Chatmd_shell_spec.Source_ref.create
    ~file:"fixture.chatmd"
    ~source_dir:"."
    ~prompt_dir:"."
    ~namespace:None
    ~start_pos:position
    ~end_pos:position
    ~source:body
;;

let fixture ?(initial_state = "0") env root =
  let body =
    sprintf
      {|let initial_state = %s
let review ev state = Task.bind(if state == 0 then Review.defer() else Review.deny(to_string(state)), fun ignored -> Task.pure(state+1))|}
      initial_state
  in
  let source = source_ref body in
  let script =
    Spec.
      { id = "reviewer"
      ; language = "chatml"
      ; kind = Shell_reviewer
      ; source = Inline body
      ; source_ref = source
      ; source_sha256 = Chatmd_shell_spec.Source_ref.digest body
      ; limits = default_limits
      }
  in
  let base = Chatmd_shell_spec.Manifest_defaults.legacy_runtime ~source in
  let runtime =
    { base with
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
              ; Chatml_reviewer
                  { id = "same-session"
                  ; script = script.id
                  ; lifecycle = Session
                  ; failure = Error_failure
                  }
              ]
          ; merge = Replace
          }
    }
  in
  let tool =
    Tool.parse_compact_attributes
      ~source
      [ "name", "probe"
      ; "type", "shell"
      ; "mode", "fixed"
      ; "runtime", Shell.Runtime_id.to_string runtime.id
      ; "command", "/bin/echo"
      ]
    |> ok
  in
  let manifest, material =
    MC.compile_with_material
      { runtimes = [ runtime ]
      ; tools = [ tool ]
      ; scripts = [ script ]
      ; legacy_tools = []
      ; moderator_runtime = None
      ; platform = Macos
      ; supported_features = Chatmd_shell_spec.Feature.phase3
      }
    |> ok
  in
  let prepared =
    match Registry.prepare ~manifest ~material with
    | Ok prepared -> prepared
    | Error errors -> raise_s [%sexp (errors : Registry.error list)]
  in
  let grant =
    Shell_runtime.Manifest_authorizer.authorize
      Shell_runtime.Manifest_authorizer.assume_authorized
      manifest
    |> ok
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
      ; source_dirs = String.Map.singleton "fixture.chatmd" root
      ; process_environment = [| "PATH=/usr/bin:/bin" |]
      ; session_id = "activation"
      }
  in
  prepared, grant, host, tool
;;

let output = function
  | Openai.Responses.Tool_output.Output.Text text -> Jsonaf.of_string text
  | _ -> failwith "expected native tool text"
;;

let assert_inactive function_ =
  let json = function_.Ochat_function.run "not even valid json" |> output in
  let error = Jsonaf.member_exn "error" json in
  [%test_eq: string]
    (Jsonaf.member_exn "code" error |> Jsonaf.string_exn)
    "shell.tool_inactive"
;;

let raw_review config =
  Shell_access.Executor.run
    config
    { request = Shell_access.Request.command (Shell_access.Command.create "/bin/echo" [])
    ; input = Empty
    ; rationale = None
    ; origin = Host "activation-test"
    }
;;

let assert_denied config =
  match raw_review config with
  | Error (Denied reason) ->
    assert (String.is_substring reason ~substring:"shell registry")
  | Ok _ | Error _ -> assert false
;;

let function_and_config staged tool =
  let function_ = Chat_response.Shell_tool.prepare staged tool |> ok in
  let runtime = Registry.Staged.runtime staged tool.Tool.runtime |> Option.value_exn in
  let config = Shell_runtime.Runtime.executor_config runtime in
  assert_inactive function_;
  assert_denied config;
  function_, config
;;

let%expect_test
    "real native functions activate only after preflight and expire with their owner"
  =
  Eio_main.run (fun env ->
    with_root env (fun root ->
      let prepared, grant, host, tool = fixture env root in
      let escaped = ref None in
      let snapshots = ref [] in
      Eio.Switch.run (fun sw ->
        let registry, function_ =
          Registry.instantiate_prepared_after
            ~sw
            ~host
            ~grant
            ~approval_provider:Shell_runtime.Approval_broker.None_available
            ~persist_extension_snapshots:(fun values ->
              snapshots := values;
              Ok ())
            ~preflight:(fun staged ->
              let function_, config = function_and_config staged tool in
              escaped := Some (staged, function_, config);
              Ok function_)
            prepared
          |> ok
        in
        let _, _, config = Option.value_exn !escaped in
        (match raw_review config with
         | Error (Denied "1") -> ()
         | Ok _ | Error _ -> assert false);
        let saved = List.hd_exn !snapshots in
        ignore (Chat_response.Shell_tool.create registry tool |> ok : Ochat_function.t);
        assert (
          Result.is_ok
            (Registry.Staged.check
               (let staged, _, _ = Option.value_exn !escaped in
                staged)));
        let restored =
          Registry.instantiate_prepared_after
            ~sw
            ~host
            ~grant
            ~approval_provider:Shell_runtime.Approval_broker.None_available
            ~extension_snapshots:[ saved ]
            ~preflight:(fun _ -> Ok ())
            prepared
          |> ok
          |> fst
        in
        let runtime = Registry.runtime restored tool.runtime |> Option.value_exn in
        (match raw_review (Shell_runtime.Runtime.executor_config runtime) with
         | Error (Denied "2") -> ()
         | Ok _ | Error _ -> assert false);
        ignore (function_ : Ochat_function.t));
      let staged, function_, config = Option.value_exn !escaped in
      assert (Result.is_error (Registry.Staged.check staged));
      assert_inactive function_;
      assert_denied config;
      print_endline
        "preflight: gated; shared reviewers: state 1; restored: state 2; closed: denied"));
  [%expect
    {| preflight: gated; shared reviewers: state 1; restored: state 2; closed: denied |}]
;;

exception Preflight_interrupted of int ref

let%test_unit
    "preflight failures revoke escaped native functions without running initializers"
  =
  Eio_main.run (fun env ->
    with_root env (fun root ->
      (* This initializer compiles but fails when evaluated. Each preflight
           outcome below must therefore occur before activation. *)
      let prepared, grant, host, tool = fixture ~initial_state:"1 / 0" env root in
      List.iter [ `Returned; `Raised; `Cancelled ] ~f:(fun mode ->
        let escaped = ref None in
        let identity = ref 0 in
        let interrupted = Preflight_interrupted identity in
        let calls = ref 0 in
        let rejected =
          Registry.
            { code = "fixture.preflight"
            ; message = "later script rejected"
            ; runtime_id = None
            }
        in
        let run sw cancel =
          Registry.instantiate_prepared_after
            ~sw
            ~host
            ~grant
            ~approval_provider:Shell_runtime.Approval_broker.None_available
            ~preflight:(fun staged ->
              Int.incr calls;
              let function_, config = function_and_config staged tool in
              escaped := Some (staged, function_, config);
              match mode with
              | `Returned -> Error [ rejected ]
              | `Raised -> raise interrupted
              | `Cancelled ->
                cancel ();
                Ok ())
            prepared
        in
        (match mode with
         | `Returned ->
           Eio.Switch.run (fun sw ->
             match run sw (fun () -> assert false) with
             | Error [ error ] ->
               assert (phys_equal rejected error);
               let staged, function_, config = Option.value_exn !escaped in
               assert (Result.is_error (Registry.Staged.check staged));
               assert_inactive function_;
               assert_denied config
             | Ok _ | Error _ -> assert false)
         | `Raised ->
           (try
              Eio.Switch.run (fun sw ->
                ignore (run sw (fun () -> assert false) : (_, _) Result.t));
              assert false
            with
            | Preflight_interrupted actual as exn ->
              assert (phys_equal identity actual);
              assert (phys_equal interrupted exn))
         | `Cancelled ->
           (try
              Eio.Cancel.sub (fun cancel ->
                Eio.Switch.run (fun sw ->
                  ignore
                    (run sw (fun () ->
                       Eio.Cancel.cancel cancel (Failure "cancel preflight"))
                     : (_, _) Result.t)));
              assert false
            with
            | Eio.Cancel.Cancelled _ -> ()));
        [%test_eq: int] !calls 1;
        let staged, function_, config = Option.value_exn !escaped in
        assert (Result.is_error (Registry.Staged.check staged));
        assert_inactive function_;
        assert_denied config);
      (* Establish independently that the sentinel really fails activation
           once a successful preflight permits the initializer to run. *)
      Eio.Switch.run (fun sw ->
        let escaped = ref None in
        let result =
          Registry.instantiate_prepared_after
            ~sw
            ~host
            ~grant
            ~approval_provider:Shell_runtime.Approval_broker.None_available
            ~preflight:(fun staged ->
              escaped := Some (function_and_config staged tool);
              Ok ())
            prepared
        in
        assert (Result.is_error result);
        let function_, config = Option.value_exn !escaped in
        assert_inactive function_;
        assert_denied config)))
;;
