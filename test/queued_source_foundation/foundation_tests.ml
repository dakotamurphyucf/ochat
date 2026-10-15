open Core
module B = Chat_response.Background_request
module C = Chat_response.Tool_capability
module R = Chat_response.Native_source_registration
module A = B.Agent_artifact
module P = Agent_protocol

let ok result =
  Result.map_error result ~f:(fun error -> error.P.Error.message) |> Result.ok_or_failwith
;;

let cap result =
  Result.map_error result ~f:(fun error -> error.C.message) |> Result.ok_or_failwith
;;

let digest = Chatmd_shell_spec.Source_ref.digest

let policy : Chat_response.One_off_request.policy =
  { compilation = { max_source_bytes = 4096; wall_seconds = 1. }
  ; execution =
      { fuel = 123
      ; max_tasks = 2
      ; max_calls = 3
      ; max_invocation_depth = 4
      ; allocation_bytes = 4194304
      ; max_value_bytes = 512
      ; max_array_items = 16
      ; max_depth = 8
      ; wall_seconds = 2.
      }
  ; max_output_bytes = 256
  }
;;

let native calls =
  let module Definition = struct
    type input = string

    let name = "agent"
    let description = None
    let type_ = "function"
    let parameters = `True
    let input_of_string input = input
  end
  in
  Ochat_function.create_function
    (module Definition)
    (fun _ ->
       Int.incr calls;
       failwith "foundation must not invoke native code")
;;

let bindings implementation =
  C.create
    ~owner:"original-owner"
    ~resource_fingerprint:(digest "original-resources")
    [ digest "original-wrapper", implementation ]
  |> cap
;;

let binding registry = C.find registry ~name:"agent" |> cap

let request registry =
  B.tool
    ~capabilities:registry
    ~reference:(C.reference (binding registry))
    ~input:`Null
    ~policy
  |> ok
;;

let selected () =
  A.create
    ~revision_id:(P.Id.Prompt_revision.create ())
    ~manifest_sha256:
      (Chatml_source.Digest.of_hex (digest "complete-manifest") |> Or_error.ok_exn)
    ~root_relative_path:
      (Source.Portable_path.create "nested/child.chatmd" |> Or_error.ok_exn)
;;

let replace json name value =
  match json with
  | `Object fields ->
    `Object ((name, value) :: List.Assoc.remove fields name ~equal:String.equal)
  | _ -> assert false
;;

let remove json name =
  match json with
  | `Object fields -> `Object (List.Assoc.remove fields name ~equal:String.equal)
  | _ -> assert false
;;

let%test_unit "v1 independent byte oracle and strict selected-code admission" =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let calls = ref 0 in
    let registry = bindings (native calls) in
    let original = request registry in
    let old_bytes =
      {|{"version":1,"target":{"kind":"tool","name":"agent"},"input":null,"pins":{"agent":"|}
      ^ C.permission_fingerprint (binding registry)
      ^ {|"},"policy":{"compile_seconds":1,"wall_seconds":2,"max_source_bytes":4096,"max_output_bytes":256,"fuel":123,"max_tasks":2,"max_calls":3,"max_invocation_depth":4,"allocation_bytes":4194304,"max_value_bytes":512,"max_array_items":16,"max_depth":8}}|}
    in
    [%test_eq: string] (Jsonaf.to_string (B.to_json original)) old_bytes;
    let old_canonical =
      P.Json_codec.canonical_string (Jsonaf.of_string old_bytes) |> ok
    in
    [%test_eq: string]
      (B.fingerprint original)
      (digest ("ochat.background-request.v1\n" ^ old_canonical));
    let artifact = selected () in
    let captured = B.with_agent_artifact original artifact |> ok in
    let wire = B.to_json captured in
    let saved = P.Developer_agent_artifact.Saved_tool_request.of_payload wire |> ok in
    [%test_eq: string]
      (P.Developer_agent_artifact.Saved_tool_request.selected_name saved)
      "agent";
    [%test_eq: string]
      (P.Id.Prompt_revision.to_string
         (P.Developer_agent_artifact.revision_id
            (P.Developer_agent_artifact.Saved_tool_request.artifact saved)))
      (P.Id.Prompt_revision.to_string (A.revision_id artifact));
    let restored = B.of_json ~policy wire |> ok in
    let admitted = B.agent_artifact restored |> Option.value_exn in
    [%test_eq: string]
      (P.Id.Prompt_revision.to_string (A.revision_id admitted))
      (P.Id.Prompt_revision.to_string (A.revision_id artifact));
    [%test_eq: string]
      (Source.Portable_path.to_string (A.root_relative_path admitted))
      "nested/child.chatmd";
    [%test_eq: string]
      (Chatml_source.Digest.to_hex (A.manifest_sha256 admitted))
      (digest "complete-manifest");
    [%test_eq: string] (B.fingerprint captured) (B.fingerprint restored);
    assert (not (String.equal (B.fingerprint original) (B.fingerprint captured)));
    assert (Result.is_error (B.with_agent_artifact captured artifact));
    let reference = Jsonaf.member_exn "agent_artifact" wire in
    let invalid_reference field value =
      replace wire "agent_artifact" (replace reference field value)
    in
    let malformed =
      [ replace wire "version" (`Number "1")
      ; remove wire "agent_artifact"
      ; replace wire "agent_artifact" `Null
      ; invalid_reference "revision_id" (`String "not-a-revision")
      ; invalid_reference "manifest_sha256" (`String (String.make 64 'g'))
      ; replace
          wire
          "target"
          (`Object
              [ "kind", `String "script"
              ; "source", `String "let main input = Task.pure(input)"
              ; "compiler_contract", `String (digest "compiler")
              ])
      ; replace
          wire
          "agent_artifact"
          (`Object (List.init 1000 ~f:(fun _ -> "revision_id", `Null)))
      ; replace wire "target" (`Object (List.init 1000 ~f:(fun _ -> "kind", `Null)))
      ; replace wire "agent_artifact" (`Object [ String.make 1025 'a', `Null ])
      ; replace wire "pins" (`Object (List.init 1000 ~f:(fun _ -> "agent", `Null)))
      ; replace wire "pins" (`Object [ String.make 257 'a', `String (digest "pin") ])
      ; replace wire "pins" (`Object [ "", `String (digest "pin") ])
      ]
      @ List.map
          [ ""; "../child.chatmd"; "/child.chatmd"; "a\\b"; String.make 1025 'a' ]
          ~f:(fun value -> invalid_reference "root_relative_path" (`String value))
    in
    List.iter malformed ~f:(fun json -> assert (Result.is_error (B.of_json ~policy json)));
    List.iter
      [ replace wire "version" (`Number "1")
      ; remove wire "agent_artifact"
      ; replace
          wire
          "target"
          (`Object [ "kind", `String "tool"; "name", `String "agent"; "extra", `Null ])
      ; replace wire "pins" (`Object [ "foreign", `String (digest "pin") ])
      ; replace
          wire
          "pins"
          (`Object [ "agent", `String (digest "pin"); "agent", `String (digest "pin") ])
      ; `Object
          (("version", `Number "2")
           ::
           (match wire with
            | `Object fields -> fields
            | _ -> assert false))
      ]
      ~f:(fun json ->
        assert (
          Result.is_error (P.Developer_agent_artifact.Saved_tool_request.of_payload json)));
    (match
       B.prepare ~env ~current_capabilities:(fun () -> registry) ~policy restored |> ok
     with
     | B.Tool { reference; _ } ->
       [%test_eq: string]
         reference.fingerprint
         (C.reference (binding registry)).fingerprint
     | Script _ -> assert false);
    [%test_eq: int] !calls 0)
;;

exception Callback_failed of int ref

let%test_unit "exact native association and post-yield cancellation preserve identity" =
  Eio_main.run (fun _ ->
    Mirage_crypto_rng_unix.use_default ();
    let native_calls = ref 0 in
    let implementation = native native_calls in
    let registry = bindings implementation in
    let original = request registry in
    let owner = P.Job.Invocation (P.Id.Invocation.create ()) in
    let callbacks = ref 0 in
    let artifact = selected () in
    let registration =
      R.create
        ~implementation
        ~prepare:(fun ~launch ~request:actual_request ~checkpoint ->
          assert (
            Option.equal
              P.Job.equal_launch_owner
              (Some owner)
              (Chat_response.Source_launch.conversation_owner launch));
          [%test_eq: string] (B.fingerprint original) (B.fingerprint actual_request);
          checkpoint ();
          Eio.Fiber.yield ();
          Int.incr callbacks;
          Ok (Some artifact))
    in
    let invoke ?(checkpoint = Eio.Fiber.check) registration binding =
      R.prepare
        registration
        ~binding
        ~launch:(Chat_response.Source_launch.conversation owner)
        ~request:original
        ~checkpoint
    in
    assert (Option.is_some (invoke registration (binding registry) |> ok));
    let foreign = bindings (native native_calls) in
    let actual_descriptor =
      { implementation.info with
        function_ = { implementation.info.function_ with name = "managed" }
      }
    in
    let managed =
      C.extend_managed
        registry
        ~owner:"managed-owner"
        ~resource_fingerprint:(digest "managed-resources")
        [ C.
            { descriptor = actual_descriptor
            ; target =
                Chatmd_shell_spec.Extension_spec.Standalone
                  { script = "code"; entrypoint = "run" }
            ; implementation_revision = digest "managed-code"
            ; metadata = Chatmd_shell_spec.Authoring_metadata.empty
            }
        ]
      |> cap
    in
    List.iter
      [ binding foreign; C.find managed ~name:"managed" |> cap ]
      ~f:(fun binding -> assert (Result.is_error (invoke registration binding)));
    [%test_eq: int] !callbacks 1;
    let identity = ref 47 in
    let raises () = raise (Callback_failed identity) in
    (match invoke ~checkpoint:raises registration (binding registry) with
     | _ -> assert false
     | exception Callback_failed actual -> assert (phys_equal actual identity));
    let raising =
      R.create ~implementation ~prepare:(fun ~launch:_ ~request:_ ~checkpoint:_ ->
        raises ())
    in
    (match invoke raising (binding registry) with
     | _ -> assert false
     | exception Callback_failed actual -> assert (phys_equal actual identity));
    let delivered = ref false in
    (match
       Eio.Cancel.sub (fun cancel ->
         let canceling =
           R.create ~implementation ~prepare:(fun ~launch:_ ~request:_ ~checkpoint:_ ->
             Eio.Fiber.yield ();
             Eio.Cancel.cancel cancel (Callback_failed identity);
             Ok (Some artifact))
         in
         let value = invoke canceling (binding registry) in
         delivered := true;
         value)
     with
     | _ -> assert false
     | exception Eio.Cancel.Cancelled (Callback_failed actual) ->
       assert (phys_equal actual identity));
    assert (not !delivered);
    [%test_eq: int] !native_calls 0)
;;

let%expect_test "selected code changes the recorded request, not the original binding" =
  Eio_main.run (fun _ ->
    Mirage_crypto_rng_unix.use_default ();
    let calls = ref 0 in
    let registry = bindings (native calls) in
    let old = request registry in
    let fresh = B.with_agent_artifact old (selected ()) |> ok in
    let pins request = Jsonaf.member_exn "pins" (B.to_json request) in
    print_s
      [%sexp
        (Jsonaf.exactly_equal (pins old) (pins fresh) : bool)
      , (B.agent_artifact old |> Option.is_none : bool)
      , (B.agent_artifact fresh |> Option.is_some : bool)
      , (!calls : int)]);
  [%expect {| (true true true 0) |}]
;;
