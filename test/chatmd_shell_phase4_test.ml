open! Core
module CM = Prompt.Chat_markdown
module D = Chatmd_shell_spec.Diagnostic
module F = Chatmd_shell_spec.Feature
module H = Shell_runtime.Hook_protocol
module M = Chatmd_shell_spec.Manifest
module MC = Chatmd_shell_spec.Manifest_compiler
module S = Chatmd_shell_spec.Shell_spec

let parse source =
  Eio_main.run (fun env ->
    CM.parse_chat_inputs ~source:"phase4.chatmd" ~dir:(Eio.Stdenv.cwd env) source)
;;

let runtimes source =
  List.filter_map (parse source) ~f:(function
    | CM.Shell_runtime runtime -> Some runtime
    | _ -> None)
;;

let compile source =
  MC.compile
    { runtimes = runtimes source
    ; tools = []
    ; scripts = []
    ; legacy_tools = []
    ; moderator_runtime = None
    ; platform = S.Macos
    ; supported_features = F.phase4
    }
;;

let compile_exn source =
  match compile source with
  | Ok manifest -> manifest
  | Error diagnostics ->
    List.map diagnostics ~f:D.to_string |> String.concat ~sep:"\n" |> failwith
;;

let%expect_test "shell-hook-json-v1 canonical requests and strict responses" =
  let request =
    H.
      { version = V1
      ; request_id = "request"
      ; hook_id = "review"
      ; kind = Reviewer
      ; payload = `Object [ "z", `Number "1"; "a", `String "value" ]
      }
  in
  print_endline (H.encode_request request);
  let cases =
    [ {|{"version":1,"request_id":"request","decision":"approve_once"}|}
    ; {|{"version":1,"request_id":"request","decision":"approve_once","extra":true}|}
    ; {|{"version":1,"version":1,"request_id":"request","decision":"approve_once"}|}
    ; {|{"version":1,"request_id":"request","action":"continue"}|}
    ]
  in
  List.iter cases ~f:(fun source ->
    match H.decode_response ~kind:Reviewer source with
    | Ok response -> print_s [%sexp (response.action : H.action)]
    | Error message -> printf "error:%s\n" message);
  [%expect
    {|
    {"hook_id":"review","kind":"reviewer","payload":{"a":"value","z":1},"request_id":"request","version":1}
    Approve_once
    error:unknown field: extra
    error:duplicate field: version
    error:missing field: decision
    |}]
;;

let phase4_source =
  {|<shell_access id="worker" extends="builtin:yolo@1"/>
    <shell_access id="main" extends="builtin:yolo@1">
      <reviewers>
        <reviewer id="review" kind="executable" executable="/bin/echo"
          runtime="worker" protocol="shell-hook-json-v1" timeout="2s" failure="deny"/>
      </reviewers>
      <interceptors>
        <interceptor id="before" phase="before" executable="/bin/echo"
          runtime="worker" protocol="shell-hook-json-v1" max_input="64KiB"
          max_output="16KiB" failure="deny"/>
      </interceptors>
      <effect_analysis>
        <analyzer id="effects" kind="executable" executable="/bin/echo"
          runtime="worker" protocol="shell-hook-json-v1" failure="unknown"/>
      </effect_analysis>
      <audit format="stderr">
        <filter executable="/bin/echo" runtime="worker" protocol="shell-hook-json-v1"/>
      </audit>
    </shell_access>
    <shell_access id="external" extends="builtin:yolo@1">
      <capabilities sandbox="preferred"/>
      <backends merge="replace" accept_declared_confinement="true">
        <external_backend id="wrapper" executable="/usr/bin/env" confinement="declared">
          <arg value="--"/>
          <cwd_value/>
          <command_argv/>
        </external_backend>
      </backends>
    </shell_access>|}
;;

let%expect_test "executable declarations and external templates enter the manifest" =
  let manifest = compile_exn phase4_source in
  List.iter manifest.M.payload.dependencies ~f:(fun dependency ->
    printf
      "%s -> %s (%s)\n"
      dependency.from_id
      dependency.to_id
      (Sexp.to_string ([%sexp_of: M.edge_kind] dependency.kind)));
  print_s [%sexp (manifest.payload.required_features : string list)];
  [%expect
    {|
    main -> worker (Worker_runtime)
    (shell.backend.direct.v1 shell.executable_hooks.v1 shell.external_backend.v1)
    |}]
;;

let%expect_test "worker runtime cycles include the complete dependency path" =
  let source =
    {|<shell_access id="a" extends="builtin:yolo@1">
        <reviewers><reviewer id="a-hook" kind="executable" executable="/bin/echo"
          runtime="b" protocol="shell-hook-json-v1" failure="deny"/></reviewers>
      </shell_access>
      <shell_access id="b" extends="builtin:yolo@1">
        <reviewers><reviewer id="b-hook" kind="executable" executable="/bin/echo"
          runtime="a" protocol="shell-hook-json-v1" failure="deny"/></reviewers>
      </shell_access>|}
  in
  (match compile source with
   | Ok _ -> print_endline "unexpected success"
   | Error diagnostics ->
     List.iter diagnostics ~f:(fun diagnostic ->
       printf "%s:%s\n" diagnostic.D.code diagnostic.message));
  [%expect
    {|
    shell.runtime_dependency_cycle:runtime dependency cycle: a -> b -> a
    |}]
;;

let%expect_test "external templates require one command argv and reject verified claims" =
  let source atoms confinement =
    sprintf
      {|<shell_access id="external" extends="builtin:yolo@1">
          <capabilities sandbox="preferred"/>
          <backends merge="replace" accept_declared_confinement="true">
            <external_backend executable="/usr/bin/env" confinement="%s">%s</external_backend>
          </backends>
        </shell_access>|}
      confinement
      atoms
  in
  List.iter
    [ source "<arg value='--'/>" "declared"
    ; source "<command_argv/>" "verified"
    ]
    ~f:(fun source ->
      match compile source with
      | Ok _ -> print_endline "unexpected success"
      | Error diagnostics -> print_endline (List.hd_exn diagnostics).D.code);
  [%expect
    {|
    shell.external_backend_command_argv
    shell.external_backend_unverifiable
    |}]
;;
