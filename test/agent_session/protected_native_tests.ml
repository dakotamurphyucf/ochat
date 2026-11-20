open! Core
open Fixtures
module P = Agent_protocol
module C = Chat_response.Tool_capability
module D = Chat_response.In_memory_stream.Tool_dispatch
module N = Agent_session.Native_tool_invocation
module Worker = Agent_session.Operation_worker

let%expect_test
    "an admitted native call transfers protected data only after its safe result commits"
  =
  let body = "private-note-fixture" in
  let raw = Jsonaf.to_string (`Object [ "note", `String body ]) in
  let calls = ref 0 in
  let materializations = ref 0 in
  let module Definition = struct
    type input = string

    let name = "protected_test"
    let description = None
    let type_ = "function"
    let parameters = `Object [ "type", `String "object" ]
    let input_of_string input = input
  end
  in
  let implementation =
    Ochat_function.create_function
      (module Definition)
      (fun input ->
         assert (String.equal raw input);
         let borrowed = N.borrow () |> protocol_ok in
         let selected = N.borrowed_capabilities borrowed |> protocol_ok in
         let binding =
           C.find selected ~name:Definition.name |> Result.ok |> Option.value_exn
         in
         let admission = N.Admission.capture borrowed ~binding |> protocol_ok in
         Agent_session.Recovery_native_result.deliver
           admission
           ~binding
           ~materialize:(fun () ->
             Int.incr materializations;
             Ok (Text body))
           ~refresh:(fun ~scope:_ () -> true)
         |> protocol_ok;
         assert (
           Result.is_error
             (Agent_session.Recovery_native_result.acknowledge admission ~binding));
         Int.incr calls;
         Openai.Responses.Tool_output.Output.Text body)
  in
  let declared =
    C.create
      ~result_contracts:[ Definition.name, Protected_recovery_v1 ]
      ~owner:"protected-fixture"
      ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "resource")
      [ Chatmd_shell_spec.Source_ref.digest "implementation", implementation ]
    |> Result.ok
    |> Option.value_exn
  in
  with_handoff_actor
    ~make_worker:(fun _env _ready ->
      Worker.create ~run:(fun ~sw:_ ~input caps ->
        let id () =
          History_entry.Id_source.allocate caps.id_source |> Result.ok_or_failwith
        in
        let entry ?call_relation item =
          let entry =
            Openai.Responses_history.create_with_id_exn ?call_relation ~id:(id ()) item
          in
          let semantic = History_entry.Payload.semantic (History_entry.payload entry) in
          History_entry.with_payload entry (History_entry.Payload.authored semantic)
        in
        let dispatch =
          Agent_session.Native_tool_dispatch.create
            ~input
            ~capabilities:caps
            ~declared
            ~registry:(fun () -> declared)
            ~now:P.Timestamp.now
            ~is_halted:(fun () -> false)
            ~admit:(fun _ _ -> Ok ())
            ~prepare_output:(fun _ -> failwith "private output reached disclosure")
        in
        let call =
          entry
            (Chat_response.Tool_call.call_item
               ~kind:Function
               ~name:Definition.name
               ~payload:"{}"
               ~call_id:"call"
               ~id:None)
        in
        let history = input.history @ [ call ] in
        let request =
          D.
            { kind = Function
            ; original_name = Definition.name
            ; original_payload = raw
            ; name = Definition.name
            ; payload = raw
            ; rejection = None
            ; call
            ; history
            ; source = None
            ; parent_call_id = None
            }
        in
        let protection = Option.value_exn dispatch.protection in
        let unredacted =
          entry
            (Chat_response.Tool_call.call_item
               ~kind:Function
               ~name:Definition.name
               ~payload:raw
               ~call_id:"call"
               ~id:None)
        in
        (match dispatch.commit_call { request with call = unredacted } with
         | exception Agent_session.Native_tool_dispatch.Dispatch_error _ -> ()
         | _ -> failwith "unredacted call entered the session");
        assert (dispatch.commit_call request);
        let returned =
          dispatch.run request ~authorize:(fun () -> ()) |> Option.value_exn
        in
        assert (List.is_empty (protection.take ()));
        let output =
          entry
            ~call_relation:(Bound (History_entry.id call))
            (Chat_response.Tool_call.output_item
               ~kind:Function
               ~call_id:"call"
               ~output:returned.output)
        in
        (Option.value_exn returned.commit_output) output;
        assert (!materializations = 0);
        let plans = protection.take () in
        assert (List.length plans = 1 && List.is_empty (protection.take ()));
        let module M = Inference_runtime.Protected_delivery.Materialized in
        let materialized =
          M.prepare plans ~history:(history @ [ output ]) |> Result.ok |> Option.value_exn
        in
        (match M.find materialized (History_entry.id output) with
         | Some (Text delivered) -> assert (String.equal delivered body)
         | _ -> failwith "missing protected output");
        assert (!materializations = 1);
        protection.close ();
        Worker.Completed
          { final_history = history @ [ output ]
          ; runtime_requests = []
          ; moderator_snapshot = None
          }))
    (fun _env actor _writer backend ->
       let rec wait () =
         let state = Agent_session.Session_actor.state actor |> protocol_ok in
         match state.active_operation with
         | None -> state
         | Some _ ->
           Eio.Fiber.yield ();
           wait ()
       in
       let state = wait () in
       assert_same_session_snapshot state (Agent_session.Memory_backend.state backend);
       let invocation = List.hd_exn state.invocations in
       (match invocation.context.input, invocation.status with
        | `Null, Published (Complete `Null) -> ()
        | _, status ->
          raise_s [%sexp "unexpected protected outcome", (status : P.Invocation.status)]);
       assert (
         not
           (String.is_substring
              (Jsonaf.to_string (P.Invocation.to_json invocation))
              ~substring:body));
       List.iter state.conversation.canonical_history ~f:(fun entry ->
         assert (
           not
             (String.is_substring
                (Sexp.to_string (P.History.sexp_of_entry entry))
                ~substring:body)));
       printf "handler calls %d; stored input and output null\n" !calls);
  [%expect {| handler calls 1; stored input and output null |}]
;;
