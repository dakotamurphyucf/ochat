open! Core
module CM = Prompt.Chat_markdown
module R = Chat_response.Agent_runtime
module SR = Chat_response.Native_tool_pool.Source_requirement

let with_host source f =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    Eio.Switch.run (fun sw ->
      let root = Eio.Path.(Eio.Stdenv.cwd env / "_build" / "native-inference") in
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 root;
      let elements = CM.parse_chat_inputs ~source:"native.chatmd" ~dir:root source in
      let host =
        R.host
          ~env
          ~workspace:root
          ~tool_dir:root
          ~prompt_dir:root
          ~session_dir:root
          ~cache_dir:root
          ~home:root
          ~session_id:"native-inference-fixture"
          ~prompt_elements:elements
        |> function
        | Ok host -> host
        | Error diagnostics ->
          failwith
            (List.map diagnostics ~f:R.diagnostic_to_string |> String.concat ~sep:"; ")
      in
      f ~sw ~host ~elements))
;;

let prepare ?(registrations = []) ~sw ~host ~elements () =
  R.prepare_extensions_without_inference
    ~sw
    ~host
    ~platform:Chatmd_shell_spec.Shell_spec.Macos
    ~prompt_elements:elements
    ~native_registrations:registrations
    ~manifest_authorizer:Shell_runtime.Manifest_authorizer.assume_authorized
    ~approval_provider:Shell_runtime.Approval_broker.None_available
    ~approval_store:(Shell_access.Approval.create_store ())
    ()
;;

let create_plain ~sw ~host ~elements () =
  R.create_without_inference
    ~sw
    ~host
    ~platform:Chatmd_shell_spec.Shell_spec.Macos
    ~prompt_elements:elements
    ~manifest_authorizer:Shell_runtime.Manifest_authorizer.assume_authorized
    ~approval_provider:Shell_runtime.Approval_broker.None_available
    ~approval_store:(Shell_access.Approval.create_store ())
    ()
;;

let codes = function
  | Ok _ -> [ "ok" ]
  | Error diagnostics -> List.map diagnostics ~f:(fun diagnostic -> diagnostic.R.code)
;;

let prepared_or_fail = function
  | Ok prepared -> prepared
  | Error diagnostics ->
    failwith (List.map diagnostics ~f:R.diagnostic_to_string |> String.concat ~sep:"; ")
;;

let%expect_test "native-only host selects ordinary tools and denies missing inference" =
  with_host {|<tool name="read_file"/>|} (fun ~sw ~host ~elements ->
    let runtime = create_plain ~sw ~host ~elements () |> prepared_or_fail in
    let names =
      List.map runtime.functions ~f:(fun tool -> tool.Ochat_function.info.function_.name)
    in
    print_s [%sexp (names : string list)]);
  with_host
    {|<shell_access id="safe" extends="builtin:yolo@1"/>
<tool name="probe" type="shell" mode="fixed" runtime="safe" command="/bin/echo"/>|}
    (fun ~sw ~host ~elements ->
       let resources = prepare ~sw ~host ~elements () |> prepared_or_fail in
       print_s
         [%sexp
           (List.map resources.native.functions ~f:(fun tool ->
              tool.Ochat_function.info.function_.name)
            : string list)]);
  with_host {|<tool name="meta_refine"/>|} (fun ~sw ~host ~elements ->
    print_s [%sexp (codes (prepare ~sw ~host ~elements ()) : string list)]);
  with_host
    {|<tool name="child" agent="child.chatmd" local/>|}
    (fun ~sw ~host ~elements ->
       print_s [%sexp (codes (prepare ~sw ~host ~elements ()) : string list)]);
  [%expect
    {|
    (read_file)
    (probe)
    (agent.inference_required)
    (agent.inference_required)
    |}]
;;

let%expect_test "native-only host uses exact supplied agent implementation" =
  let module Definition = struct
    type input = string

    let name = "child"
    let description = Some "Constructed by the authenticated host."
    let type_ = "function"
    let parameters = `Object [ "type", `String "object" ]
    let input_of_string text = text
  end
  in
  let implementation =
    Ochat_function.create_function
      (module Definition)
      (fun text -> Openai.Responses.Tool_output.Output.Text text)
  in
  let registration =
    R.
      { implementation
      ; implementation_revision = Chatmd_shell_spec.Source_ref.digest "fixture-v1"
      ; result_contract = Chat_response.Tool_capability.Native_output
      ; authoring_metadata = None
      ; source_requirement = Chat_response.Native_tool_pool.Source_requirement.Ordinary
      }
  in
  with_host
    {|<tool name="child" agent="child.chatmd" local/>|}
    (fun ~sw ~host ~elements ->
       let resources =
         prepare ~registrations:[ registration ] ~sw ~host ~elements ()
         |> prepared_or_fail
       in
       let source_requirement =
         match resources.native.native_registrations with
         | [ registration ] ->
           (match registration.source_requirement with
            | SR.Ordinary -> "ordinary"
            | SR.Source None -> "source_unavailable"
            | SR.Source (Some _) -> "source_owned")
         | _ -> "unexpected_registration_count"
       in
       print_s
         [%sexp
           (List.exists resources.native.functions ~f:(phys_equal implementation) : bool)
         , (source_requirement : string)];
       let source implementation =
         Chat_response.Native_source_registration.create
           ~implementation
           ~prepare:(fun ~launch:_ ~request:_ ~checkpoint:_ -> Ok None)
       in
       let with_source implementation =
         { registration with
           source_requirement = SR.Source (Some (source implementation))
         }
       in
       let owned =
         prepare ~registrations:[ with_source implementation ] ~sw ~host ~elements ()
         |> prepared_or_fail
       in
       print_s
         [%sexp
           ((match owned.native.native_registrations with
             | [ registration ] ->
               (match registration.source_requirement with
                | SR.Source (Some source) ->
                  Chat_response.Native_source_registration.matches_implementation
                    source
                    implementation
                | SR.Ordinary | SR.Source None -> false)
             | _ -> false)
            : bool)];
       let foreign =
         Ochat_function.create_function
           (module Definition)
           (fun text -> Openai.Responses.Tool_output.Output.Text text)
       in
       print_s
         [%sexp
           (codes (prepare ~registrations:[ with_source foreign ] ~sw ~host ~elements ())
            : string list)]);
  [%expect
    {|
    (true source_unavailable)
    true
    (agent.source_registration_mismatch)
    |}]
;;

let%expect_test "inherited model reviewer rejects before registry activation" =
  let source =
    {|<shell_access id="base" extends="builtin:yolo@1">
  <reviewers strategy="first_terminal">
    <reviewer id="model" kind="model" agent="reviewer" failure="deny"/>
  </reviewers>
</shell_access>
<shell_access id="derived" extends="base"/>
<tool name="probe" type="shell" mode="fixed" runtime="derived" command="/bin/echo"/>|}
  in
  with_host source (fun ~sw ~host ~elements ->
    let grants = ref 0 in
    let result =
      R.prepare_extensions_without_inference
        ~sw
        ~host
        ~platform:Chatmd_shell_spec.Shell_spec.Macos
        ~prompt_elements:elements
        ~manifest_authorizer:(fun _ ->
          incr grants;
          Shell_runtime.Manifest_authorizer.Authorize_once)
        ~approval_provider:Shell_runtime.Approval_broker.None_available
        ~approval_store:(Shell_access.Approval.create_store ())
        ()
    in
    print_s [%sexp (codes result : string list), (!grants : int)]);
  [%expect {| ((agent.inference_required) 0) |}]
;;
