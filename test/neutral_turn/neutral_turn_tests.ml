open! Core
module R = Inference.Request
module E = Inference.Event
module O = Inference.Observation
module Runtime = Inference_runtime
module C = Chat_response
module P = History_entry.Payload

exception Host_ack_failed

let ok result =
  Result.map_error result ~f:(fun _ -> "fixture admission") |> Result.ok_or_failwith
;;

let limits = Transcript.Admission.default

let target =
  R.Target.create
    ~adapter:"synthetic"
    ~profile:"selected"
    ~profile_revision:None
    ~account:None
    ~endpoint:"local"
    ~model:"fixture"
    ~settings:[]
    ~limits
  |> ok
;;

let identity () =
  let next = ref 0 in
  C.Neutral_turn.Identity.
    { new_preparation_id = (fun () -> "preparation-" ^ Int.to_string (!next + 1))
    ; with_attempt =
        (fun _ ~relation ~f ->
          Int.incr next;
          let attempt = Int.to_string !next in
          let scope =
            Transcript.Scope.create
              ~source:(Transcript.Source_id.of_string "fixture-owner" |> ok)
              ~attempt:(Transcript.Attempt_id.of_string attempt |> ok)
              ~relation
            |> ok
          in
          let accounting_id = O.Observation_id.of_string ("usage-" ^ attempt) |> ok in
          f ~scope ~accounting_id)
    }
;;

let usage scope accounting_id =
  let unknown = O.Count.create (Unknown Not_reported) |> ok in
  let usage =
    O.Usage.create
      ~counts:
        { input = unknown
        ; output = unknown
        ; reported_total = unknown
        ; cached_input = unknown
        ; cache_write_input = unknown
        ; reasoning_output = unknown
        }
      ~inclusions:[]
    |> ok
  in
  O.create
    ~scope
    ~id:accounting_id
    ~revision:0L
    ~payload:(Usage usage)
    ~limits:O.Admission.observation
  |> ok
;;

let receipt scope accounting_id output =
  Runtime.Receipt.create
    ~terminal:
      (E.Terminal.create ~scope ~delivery:Response_started ~outcome:Completed |> ok)
    ~usage:(usage scope accounting_id)
    ~output
    ~output_coverage:Response_output
    ~limits:Runtime.Limits.default
  |> ok
;;

let context ?(on_prepare = ignore) ?on_session_prepare run =
  let prepare ~on_prepare ~preparation_id request =
    on_prepare request;
    let configuration =
      O.Configuration.of_target
        (R.target request)
        ~preparation_id
        ~transport:In_process
        ~capabilities:[]
        ~limits:O.Admission.observation
      |> ok
    in
    Runtime.Plan.create ~request ~configuration ~fingerprint:"fixture" ~run:(run request)
  in
  let open_session =
    Option.map on_session_prepare ~f:(fun on_prepare _owner ~policy:_ ->
      Ok (prepare ~on_prepare))
  in
  Runtime.Adapter.create
    ~preflight_history:(fun ~target:_ _ -> Ok ())
    ~id:"synthetic"
    ~limits:Runtime.Limits.default
    ~bind:(fun _ -> Ok ())
    ~prepare:(prepare ~on_prepare)
    ?open_session
    ()
  |> ok
  |> Runtime.Context.create ~target
  |> ok
;;

let message text =
  P.Semantic.create
    (Message
       { form = Output
       ; role = Assistant
       ; content = [ Text { text; annotations = []; logprobs = Absent } ]
       ; phase = Absent
       })
    ~metadata:P.Metadata.empty
  |> ok
  |> P.authored
;;

let call name =
  P.Semantic.create
    (Call
       { kind = Function; name; namespace = Absent; input_bytes = "{}"; async = Absent })
    ~metadata:{ P.Metadata.empty with call_id = Value "same-alias" }
  |> ok
  |> P.authored
;;

let candidate scope id payload local_execution =
  let semantic = P.semantic payload in
  let call_name =
    match P.Semantic.view semantic with
    | Call { name; _ } -> Some name
    | Message _ | Result _ | Reasoning _ | Unknown _ -> None
  in
  let item =
    Transcript.Item.create
      ~scope
      ~id:(Transcript.Item_id.of_string id |> ok)
      ~entry_id:None
      ~header:(Some (Transcript.Header.of_semantic semantic))
      ~call_name
    |> ok
  in
  E.create (Candidate_ready { item; payload; local_execution }) ~limits |> ok
;;

let execute
      env
      context
      ?(history = [])
      ?(table = String.Table.create ())
      ?(on_commit = ignore)
      ?(on_attempt = ignore)
      ()
  =
  let allocator =
    History_entry.Allocator.create ~namespace:"host" ~next_sequence:0 |> ok
  in
  C.In_memory_stream.run_completion_stream_in_memory_entries
    ~env
    ~inference_context:context
    ~inference_identity:(identity ())
    ~on_inference_attempt:on_attempt
    ~on_inference_completion:(fun _ -> ())
    ~allocator
    ~history
    ~tools:(Some [])
    ~tool_tbl:table
    ~parallel_tool_calls:false
    ~on_history_item_appended:on_commit
    ()
;;

let kind entry =
  match P.Semantic.view (P.semantic (History_entry.payload entry)) with
  | Message { content = [ Text { text; _ } ]; _ } -> "message:" ^ text
  | Call { name; _ } -> "call:" ^ name
  | Result { relation = Bound id; _ } -> "output:" ^ History_entry.Id.to_string id
  | Result _ -> "unbound output"
  | Message _ | Reasoning _ | Unknown _ -> "other"
;;

let%expect_test "early call retains arrival commit order and terminal unseen items" =
  Eio_main.run (fun env ->
    let rounds = ref 0 in
    let called = ref 0 in
    let observed_parent = ref None in
    let table = String.Table.create () in
    Hashtbl.set table ~key:"inspect" ~data:(fun ~invocation _ ->
      Int.incr called;
      observed_parent := Ochat_function.Invocation.inference_parent invocation;
      assert (not (Ochat_function.Invocation.is_observed invocation));
      Openai.Responses.Tool_output.Output.Text "host output");
    let context =
      context
        (fun _ ~sw:_ ~scope ~accounting_id ~note_delivery:_ ~on_event ~on_observation:_ ->
           Int.incr rounds;
           if !rounds = 1
           then (
             let early = candidate scope "call" (call "inspect") Tool_candidate in
             let preceding =
               candidate scope "message" (message "provider-array-first") Not_eligible
             in
             on_event early;
             receipt scope accounting_id [ preceding; early ])
           else
             receipt
               scope
               accounting_id
               [ candidate scope "answer" (message "done") Not_eligible ])
    in
    let commits = Queue.create () in
    let history = execute env context ~table ~on_commit:(Queue.enqueue commits) () in
    assert (
      List.equal
        (fun a b ->
           History_entry.Id.equal (History_entry.id a) (History_entry.id b)
           && Document_schema.Json.equal
                (P.to_json (History_entry.payload a))
                (P.to_json (History_entry.payload b)))
        (Queue.to_list commits)
        history);
    let call_entry = List.hd_exn history in
    let parent = Option.value_exn !observed_parent in
    assert (
      Option.equal
        History_entry.Id.equal
        parent.call_entry_id
        (Some (History_entry.id call_entry)));
    assert (Option.equal String.equal parent.call_alias (Some "same-alias"));
    printf "rounds=%d executions=%d parent=true\n" !rounds !called;
    List.iter history ~f:(fun entry -> print_endline (kind entry)));
  [%expect
    {|
    rounds=2 executions=1 parent=true
    call:inspect
    message:provider-array-first
    output:4:host:0
    message:done
    |}]
;;

let%expect_test "retained unsupported call is not execution eligibility" =
  Eio_main.run (fun env ->
    let called = ref false in
    let table = String.Table.create () in
    Hashtbl.set table ~key:"inspect" ~data:(fun ~invocation:_ _ ->
      called := true;
      Openai.Responses.Tool_output.Output.Text "must not run");
    let context =
      context
        (fun
            _
             ~sw:_
             ~scope
             ~accounting_id
             ~note_delivery:_
             ~on_event:_
             ~on_observation:_
           ->
           receipt
             scope
             accounting_id
             [ candidate scope "retained" (call "inspect") Not_eligible ])
    in
    let history = execute env context ~table () in
    assert (not !called);
    printf "retained=%d executed=false\n" (List.length history));
  [%expect {| retained=1 executed=false |}]
;;

let%expect_test
    "required host admission before dispatch can reject without backend effects"
  =
  Eio_main.run (fun env ->
    let called = ref false in
    let context =
      context
        (fun
            _
             ~sw:_
             ~scope
             ~accounting_id
             ~note_delivery:_
             ~on_event:_
             ~on_observation:_
           ->
           called := true;
           receipt scope accounting_id [])
    in
    (match execute env context ~on_attempt:(fun _ -> raise Host_ack_failed) () with
     | _ -> assert false
     | exception Host_ack_failed -> ()
     | exception exn -> raise exn);
    printf "backend_started=%b\n" !called);
  [%expect {| backend_started=false |}]
;;

let%expect_test
    "captured unknown envelopes stay exact at selected request and commit boundaries"
  =
  Eio_main.run (fun env ->
    let semantic =
      P.Semantic.create (Unknown { provider_kind = "future" }) ~metadata:P.Metadata.empty
      |> ok
    in
    let raw =
      `Object [ "future", `Object [ "number", `Number "1e+00"; "null", `Null ] ]
    in
    let payload = P.captured semantic ~origin:P.Origin.unavailable ~raw |> ok in
    let input_id = History_entry.Id.create ~namespace:"foreign" ~sequence:19 |> ok in
    let input = History_entry.create_with_id ~id:input_id payload in
    let context =
      context
        (fun
            request
             ~sw:_
             ~scope
             ~accounting_id
             ~note_delivery:_
             ~on_event:_
             ~on_observation:_
           ->
           assert (
             Document_schema.Json.equal
               (P.to_json (History_entry.payload (List.hd_exn (R.history request))))
               (P.to_json payload));
           receipt scope accounting_id [ candidate scope "opaque" payload Not_eligible ])
    in
    let history = execute env context ~history:[ input ] () in
    assert (History_entry.Id.equal (History_entry.id (List.hd_exn history)) input_id);
    let output = List.last_exn history in
    assert (
      Jsonaf.exactly_equal (P.to_json (History_entry.payload output)) (P.to_json payload));
    printf "retained=%d opaque=exact input_id=preserved\n" (List.length history));
  [%expect {| retained=2 opaque=exact input_id=preserved |}]
;;

let%expect_test "repeated provider alias executes distinct committed call occurrences" =
  Eio_main.run (fun env ->
    let rounds = ref 0 in
    let called = ref 0 in
    let table = String.Table.create () in
    Hashtbl.set table ~key:"inspect" ~data:(fun ~invocation:_ _ ->
      Int.incr called;
      Openai.Responses.Tool_output.Output.Text "host output");
    let context =
      context
        (fun _ ~sw:_ ~scope ~accounting_id ~note_delivery:_ ~on_event ~on_observation:_ ->
           Int.incr rounds;
           if !rounds = 1
           then (
             let first = candidate scope "first" (call "inspect") Tool_candidate in
             let second = candidate scope "second" (call "inspect") Tool_candidate in
             on_event first;
             on_event second;
             receipt scope accounting_id [ first; second ])
           else receipt scope accounting_id [])
    in
    let history = execute env context ~table () in
    History_entry.validate_relations history |> ok;
    printf "executions=%d entries=%d\n" !called (List.length history);
    List.iter history ~f:(fun entry -> print_endline (kind entry)));
  [%expect
    {|
    executions=2 entries=4
    call:inspect
    call:inspect
    output:4:host:0
    output:4:host:1
    |}]
;;

let%expect_test
    "selected temperature overrides admit integral floats and reject nonfinite before \
     dispatch"
  =
  Eio_main.run (fun env ->
    let seen = Queue.create () in
    let context =
      context
        (fun
            request
             ~sw:_
             ~scope
             ~accounting_id
             ~note_delivery:_
             ~on_event:_
             ~on_observation:_
           ->
           let setting =
             R.Target.settings (R.target request)
             |> List.find_exn ~f:(fun setting ->
               String.equal (R.Setting.name setting) "temperature")
           in
           (match R.Setting.value setting with
            | Value (`Number number) -> Queue.enqueue seen number
            | Absent | Null | Value _ -> failwith "expected numeric override");
           receipt scope accounting_id [])
    in
    let run temperature =
      C.In_memory_stream.run_completion_stream_in_memory_entries
        ~env
        ~inference_context:context
        ~inference_identity:(identity ())
        ~on_inference_attempt:ignore
        ~on_inference_completion:ignore
        ~allocator:
          (History_entry.Allocator.create ~namespace:"temperature" ~next_sequence:0 |> ok)
        ~history:[]
        ~tools:(Some [])
        ~tool_tbl:(String.Table.create ())
        ~temperature
        ()
    in
    List.iter [ 0.; 1.; 1.25 ] ~f:(fun value -> ignore (run value : History_entry.t list));
    List.iter [ Float.nan; Float.infinity; Float.neg_infinity ] ~f:(fun value ->
      let rejected =
        try
          ignore (run value : History_entry.t list);
          false
        with
        | Failure _ -> true
      in
      assert rejected);
    print_s [%sexp (Queue.to_list seen : string list)]);
  [%expect {| (0.0 1.0 1.25) |}]
;;

let%expect_test "Driver rejects retired transport before document or model effects" =
  Eio_main.run (fun env ->
    let dispatched = ref false in
    let context =
      context
        (fun
            _
             ~sw:_
             ~scope:_
             ~accounting_id:_
             ~note_delivery:_
             ~on_event:_
             ~on_observation:_
           ->
           dispatched := true;
           failwith "retired transport dispatched")
    in
    let rejected =
      try
        C.Driver.run_completion_stream
          ~env
          ~inference_context:context
          ~inference_identity:(identity ())
          ~on_inference_attempt:ignore
          ~on_inference_completion:ignore
          ~post_stream:(fun ~sw:_ ~inputs:_ -> failwith "retired transport invoked")
          ~output_file:"retired-transport-missing-directory/prompt.chatmd"
          ();
        false
      with
      | Invalid_argument _ -> true
    in
    print_s [%sexp (rejected : bool), (!dispatched : bool)]);
  [%expect {| (true false) |}]
;;

let fork_call =
  P.Semantic.create
    (Call
       { kind = Function
       ; name = "fork"
       ; namespace = Absent
       ; input_bytes = {|{"command":"inspect","arguments":[]}|}
       ; async = Absent
       })
    ~metadata:{ P.Metadata.empty with call_id = Value "fork-alias" }
  |> ok
  |> P.authored
;;

let%expect_test
    "built-in fork extracts captured last assistant semantics and leaves unknown last \
     opaque"
  =
  Eio_main.run (fun env ->
    List.iter [ false; true ] ~f:(fun unknown_last ->
      let raw =
        `Object [ "type", `String "future-envelope"; "private", `Number "1e+00" ]
      in
      let captured =
        P.captured
          (P.semantic (message "captured child"))
          ~origin:P.Origin.unavailable
          ~raw
        |> ok
      in
      let unknown =
        P.Semantic.create
          (Unknown { provider_kind = "future" })
          ~metadata:P.Metadata.empty
        |> ok
        |> fun semantic -> P.captured semantic ~origin:P.Origin.unavailable ~raw |> ok
      in
      let parent_round = ref 0 in
      let child_count = ref 0 in
      let context =
        context
          (fun
              _
               ~sw:_
               ~scope
               ~accounting_id
               ~note_delivery:_
               ~on_event:_
               ~on_observation:_
             ->
             match scope.relation with
             | Nested _ ->
               Int.incr child_count;
               let output = [ candidate scope "captured" captured Not_eligible ] in
               let output =
                 if unknown_last
                 then output @ [ candidate scope "unknown" unknown Not_eligible ]
                 else output
               in
               receipt scope accounting_id output
             | Root ->
               Int.incr parent_round;
               receipt
                 scope
                 accounting_id
                 [ (if !parent_round = 1
                    then candidate scope "fork" fork_call Tool_candidate
                    else candidate scope "answer" (message "parent done") Not_eligible)
                 ])
      in
      let history = execute env context () in
      let output =
        List.find_map_exn history ~f:(fun entry ->
          match P.Semantic.view (P.semantic (History_entry.payload entry)) with
          | Result { output = Text text; relation = Bound _; _ } -> Some text
          | _ -> None)
      in
      assert (List.length history = 3);
      assert (!child_count = 1);
      assert (
        List.for_all history ~f:(fun entry ->
          String.equal (History_entry.Id.namespace (History_entry.id entry)) "host"));
      print_s [%sexp (unknown_last : bool), (output : string)]));
  [%expect
    {|
    (false "captured child")
    (true "")
    |}]
;;

let%expect_test "legacy wrapper fork depths retain the shared child bound" =
  Eio_main.run (fun env ->
    List.iter [ false; true ] ~f:(fun observed ->
      List.iter [ 0; 1; 2 ] ~f:(fun fork_depth ->
        let calls = ref 0 in
        let nested = ref 0 in
        let rejected = ref false in
        let context =
          context
            (fun
                request
                 ~sw:_
                 ~scope
                 ~accounting_id
                 ~note_delivery:_
                 ~on_event:_
                 ~on_observation:_
               ->
               Int.incr calls;
               (match scope.relation with
                | Root -> ()
                | Nested _ -> Int.incr nested);
               let has_output =
                 match List.last (R.history request) with
                 | Some entry ->
                   (match P.Semantic.view (P.semantic (History_entry.payload entry)) with
                    | Result { relation = Bound _; output = Text text; _ } ->
                      if String.is_prefix text ~prefix:"Error: Called the [fork]"
                      then rejected := true;
                      true
                    | _ -> false)
                 | None -> false
               in
               receipt
                 scope
                 accounting_id
                 [ (if has_output
                    then candidate scope "answer" (message "done") Not_eligible
                    else candidate scope "fork" fork_call Tool_candidate)
                 ])
        in
        let dir = Eio.Stdenv.cwd env in
        let ctx =
          C.Ctx.create
            ~inference_context:context
            ~inference_identity:(identity ())
            ~on_inference_attempt:ignore
            ~on_inference_completion:ignore
            ~env
            ~dir
            ~tool_dir:dir
            ~cache:(C.Cache.create ~max_size:1 ())
            ()
        in
        let allocator =
          History_entry.Allocator.create ~namespace:"fork-bound" ~next_sequence:0 |> ok
        in
        let history =
          if observed
          then
            C.Agent_response_loop.run_entries
              ~ctx
              ~allocator
              ~fork_depth
              ~tool_tbl:(String.Table.create ())
              ~observer:{ on_event = ignore; on_tool_execution = ignore }
              []
          else
            C.Response_loop.run_entries
              ~ctx
              ~allocator
              ~fork_depth
              ~tool_tbl:(String.Table.create ())
              []
        in
        assert (List.length history = 3);
        assert !rejected;
        print_s
          [%sexp (observed : bool), (fork_depth : int), (!calls : int), (!nested : int)])));
  [%expect
    {|
    (false 0 6 4)
    (false 1 4 2)
    (false 2 2 0)
    (true 0 6 4)
    (true 1 4 2)
    (true 2 2 0)
    |}]
;;

let%expect_test
    "recipe admission guard survives derived contexts and blocks selected dispatch"
  =
  Eio_main.run (fun env ->
    Eio.Switch.run (fun sw ->
      let active = ref false
      and guards = ref 0
      and upstream = ref 0
      and transports = ref 0 in
      let context =
        context
          (fun
              _
               ~sw:_
               ~scope
               ~accounting_id
               ~note_delivery
               ~on_event:_
               ~on_observation:_
             ->
             Int.incr transports;
             note_delivery E.Terminal.Response_started;
             receipt
               scope
               accounting_id
               [ candidate scope "answer" (message "done") Not_eligible ])
      in
      let dir = Eio.Stdenv.cwd env in
      let ctx =
        C.Ctx.create
          ~inference_context:context
          ~inference_identity:(identity ())
          ~on_inference_attempt:(fun _ -> Int.incr upstream)
          ~on_inference_completion:ignore
          ~env
          ~dir
          ~tool_dir:dir
          ~cache:(C.Cache.create ~max_size:1 ())
          ()
      in
      let guard _ =
        Int.incr guards;
        if not !active then raise C.Ctx.Inference_admission_rejected
      in
      let derived_target = R.Target.with_model target ~model:"derived" ~limits |> ok in
      let derived = Runtime.Context.derive context ~target:derived_target |> ok in
      let child =
        C.Ctx.with_inference_attempt_guard ctx ~before_attempt:guard
        |> fun ctx -> C.Ctx.with_inference ctx ~inference_context:derived
      in
      let complete ctx =
        C.Ctx.inference_execution ctx
        |> fun execution ->
        Inference_client.Execution.complete_text
          execution
          ~sw
          ~settings:[]
          ~messages:[ User, "input" ]
          ()
        |> ok
      in
      (match complete child with
       | _ -> failwith "revoked admission dispatched"
       | exception C.Ctx.Inference_admission_rejected -> ());
      [%test_eq: int] 0 !upstream;
      [%test_eq: int] 0 !transports;
      active := true;
      [%test_eq: string] "done" (complete child);
      [%test_eq: int] 1 !upstream;
      [%test_eq: int] 1 !transports;
      active := false;
      let executor =
        C.Model_executor.create
          ~sw
          ~exec_context:
            { ctx
            ; fetch_prompt =
                (fun ~ctx:_ ~prompt:_ ~is_local:_ ->
                  Ok ("<developer>Recipe</developer>", None))
            ; run_agent =
                (fun ?history_compaction:_ ?prompt_dir:_ ?session_id:_ ~ctx _ _ ->
                  complete ctx)
            }
          ()
      in
      let recipe =
        C.Model_executor.recipe_agent_prompt_v1
          executor
          ~session_id:"recipe-owner"
          ~inference_context:derived
          ~before_inference_attempt:guard
          ()
      in
      (match
         recipe.call
           ~payload:
             (Jsonaf.of_string
                {|{"prompt":"recipe.chatmd","is_local":true,"input":"test"}|})
       with
       | Ok (C.Moderation.Capabilities.Model_error error) ->
         [%test_eq: string] "model inference admission was revoked" error
       | _ -> failwith "recipe did not report safe admission revocation");
      [%test_eq: int] 3 !guards;
      [%test_eq: int] 1 !upstream;
      [%test_eq: int] 1 !transports));
  print_endline
    "revoked derived and recipe attempts stop before upstream/transport; live control \
     dispatches";
  [%expect
    {| revoked derived and recipe attempts stop before upstream/transport; live control dispatches |}]
;;

let%expect_test
    "nested and exported fork calls detach while root keeps its session binding"
  =
  Mirage_crypto_rng_unix.use_default ();
  Eio_main.run (fun env ->
    Eio.Switch.run (fun sw ->
      let prepared_bindings = ref [] in
      let context =
        context
          ~on_prepare:(fun _ -> prepared_bindings := "detached" :: !prepared_bindings)
          ~on_session_prepare:(fun _ ->
            prepared_bindings := "attached" :: !prepared_bindings)
          (fun _
            ~sw:_
            ~scope
            ~accounting_id
            ~note_delivery:_
            ~on_event:_
            ~on_observation:_ ->
             receipt
               scope
               accounting_id
               [ candidate scope "answer" (message "done") Not_eligible ])
        |> fun context ->
        Runtime.Context.with_session context (Runtime.Session.create ~sw) |> ok
      in
      let parent =
        Transcript.Scope.
          { scope =
              { source = Transcript.Source_id.of_string "parent" |> ok
              ; attempt = Transcript.Attempt_id.of_string "0" |> ok
              }
          ; call_entry_id = None
          ; call_alias = Some "fork-call"
          }
      in
      let suffix = Agent_protocol.Id.Transaction.(create () |> to_string) in
      let directory =
        Eio.Path.(Eio.Stdenv.fs env / "/tmp" / ("ochat-binding-" ^ suffix))
      in
      Eio.Path.mkdir ~perm:0o700 directory;
      Exn.protect
        ~finally:(fun () -> Eio.Path.rmtree directory)
        ~f:(fun () ->
          let identity = identity () in
          let turn relation namespace =
            C.In_memory_stream.run_completion_stream_in_memory_entries
              ~env
              ~inference_context:context
              ~inference_identity:identity
              ~on_inference_attempt:ignore
              ~on_inference_completion:ignore
              ~inference_relation:relation
              ~datadir:directory
              ~allocator:(History_entry.Allocator.create ~namespace ~next_sequence:0 |> ok)
              ~history:[]
              ~tools:(Some [])
              ~parallel_tool_calls:false
              ()
            |> ignore
          in
          turn Root "first-root";
          turn (Nested parent) "nested";
          let ctx =
            C.Ctx.create
              ~inference_context:context
              ~inference_identity:identity
              ~on_inference_attempt:ignore
              ~on_inference_completion:ignore
              ~env
              ~dir:directory
              ~tool_dir:directory
              ~cache:(C.Cache.create ~max_size:1 ())
              ()
          in
          List.iter [ false; true ] ~f:(fun observed ->
            let invocation_id = C.Fork.Invocation_id.create () in
            let transcript_observer =
              if observed then Some C.Fork.{ parent; observe = ignore } else None
            in
            let result =
              C.Fork.execute_entries
                ~ctx
                ~allocator:(C.Fork.allocator ~parent_namespace:"root" invocation_id)
                ~history:[]
                ~invocation_id
                ~call_id:"fork-call"
                ~arguments:{|{"command":"inspect","arguments":[]}|}
                ~tools:[]
                ~tool_tbl:(String.Table.create ())
                ?transcript_observer
                ~on_fn_out:ignore
                ()
            in
            [%test_eq: string] result "done");
          turn Root "last-root";
          print_s [%sexp (List.rev !prepared_bindings : string list)])));
  [%expect {| (attached detached detached detached attached) |}]
;;
