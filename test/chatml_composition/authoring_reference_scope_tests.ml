open Core
open Agent_server_test_support
open Authoring_context_tests
module P = Agent_protocol
module I = P.Invocation
module R = P.Authoring_reference
module S = Agent_session.Authoring_reference_scope
module N = Agent_session.Native_tool_invocation
module Services = Agent_session.Authoring_services

let%expect_test
    "an internal script read annotates its own invocation without claiming model delivery"
  =
  let source =
    {|
let main input =
  let* result = Tool.call("ochat_authoring_context", input) in
  match result with
  | `Ok(`String(text)) ->
    (match Json.get_field(Json.parse(text), "items") with
     | `Some(`Array(items)) -> Task.pure(`Bool(Array.length(items) > 0))
     | _ -> Task.fail("reference response has no items"))
  | _ -> Task.fail("reference lookup failed")
|}
  in
  Fixtures.with_daemon
    ~sources:
      [ ( "agent.chatmd"
        , {|<developer>Inspect documentation through a script.</developer><tool name="run_chatml"/>|}
        )
      ]
    ~calls:
      [ ( "script-reference"
        , "run_chatml"
        , `Object
            [ "source", `String source
            ; "input", request ~task:"one_off_script" ~topic_id:"chatml.tasks" "topic"
            ; "tools", `Array [ `String "ochat_authoring_context" ]
            ] )
      ]
    (fun state ->
       let parent = Fixtures.model_invocation state "script-reference" in
       assert (Option.is_none parent.authoring_reference);
       (match Fixtures.result state "script-reference" with
        | Complete `True -> ()
        | outcome -> raise_s [%sexp (outcome : I.outcome)]);
       let annotated =
         List.filter state.invocations ~f:(fun invocation ->
           Option.is_some invocation.I.authoring_reference)
       in
       let inner = List.hd_exn annotated in
       assert (List.length annotated = 1);
       assert (I.equal_origin inner.context.origin Script);
       assert (Option.is_none inner.output_entry_id);
       let reference = Option.value_exn inner.authoring_reference in
       match Fixtures.outcome inner with
       | Complete value -> assert (R.matches_output reference value)
       | _ -> assert false);
  print_endline
    "real nested lookup retained; summarized script result has no read receipt; internal \
     output has no model history identity";
  [%expect
    {| real nested lookup retained; summarized script result has no read receipt; internal output has no model history identity |}]
;;

let context capabilities name generation : I.context =
  { id = P.Id.Invocation.of_string name |> protocol_ok
  ; session_id = P.Id.Session.of_string "ses_reference_scope" |> protocol_ok
  ; generation
  ; origin = Script
  ; provider_call_id = None
  ; call_entry_id = None
  ; parent_invocation = None
  ; parent_job = None
  ; tool_name = "reference_helper"
  ; implementation_revision = "reference-helper-fixture"
  ; capability_fingerprint = C.fingerprint capabilities
  ; input = `Null
  ; created_at = P.Timestamp.of_string "2026-09-12T12:00:00Z" |> protocol_ok
  ; deadline = None
  }
;;

let%expect_test
    "helper receipts follow captured invocations across overlapping and nested scopes"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let selected = capabilities () in
    let request = request ~task:"moderator_tool" ~topic_id:"chatml.tasks" "topic" in
    let contexts =
      [ context selected "inv_reference_left" 1
      ; context selected "inv_reference_right" 2
      ]
    in
    let entered_left, signal_left = Eio.Promise.create () in
    let entered_right, signal_right = Eio.Promise.create () in
    let run ctx ready signal =
      let default_tokens = 6000 + (ctx.I.generation * 1000) in
      let caller =
        V.context_budget ~default_tokens ~max_tokens:9000 ~preload_tokens:16000
        |> Result.bind ~f:(V.configure_context_budget (host ()))
        |> Result.ok_or_failwith
      in
      let service = Services.create ~env ~host:caller in
      let invocation = I.create ctx |> protocol_ok |> I.dispatch |> protocol_ok in
      let retained_borrow = ref None in
      let retained_scope = ref None in
      let call borrowed =
        Services.reference service borrowed request
        |> Result.map_error ~f:(fun error -> error.I.message)
        |> Result.ok_or_failwith
      in
      let result, references =
        S.collect ctx (fun () ->
          retained_scope := S.capture ~invocation_id:ctx.id;
          N.with_dispatched_scope
            ~selected
            ~invocation
            ~execute:(fun ~invocation:_ _ -> failwith "readonly helper executed a tool")
            (fun () ->
               let borrowed = N.borrow () |> protocol_ok in
               retained_borrow := Some borrowed;
               Eio.Promise.resolve signal ();
               Eio.Promise.await ready;
               let first = call borrowed in
               (* The helper retains the lending invocation even while another
               collector is dynamically bound. Repeated exact reads deduplicate. *)
               let nested =
                 { ctx with
                   id =
                     P.Id.Invocation.of_string
                       (P.Id.Invocation.to_string ctx.id ^ "_nested")
                     |> protocol_ok
                 }
               in
               let again, nested_references =
                 S.collect nested (fun () -> call borrowed)
               in
               assert (List.is_empty nested_references);
               require_json first again;
               Ok first)
          |> protocol_ok)
      in
      let reference = List.hd_exn references in
      require_json
        (`Number (Int.to_string default_tokens))
        (field (field result "budget") "max_tokens");
      assert (List.length references = 1);
      assert (R.matches_response reference result);
      assert (
        String.equal
          reference.scope
          (R.scope_for ~session_id:ctx.session_id ~generation:ctx.generation));
      assert (
        Result.is_error
          (Services.reference service (Option.value_exn !retained_borrow) request));
      let query_service =
        Q.create ~secret:"expired-scope-fixture" () |> Result.ok_or_failwith
      in
      let response =
        Q.query_with_receipt
          query_service
          ~host:caller
          ~capabilities:selected
          ~scope:(R.scope_for ~session_id:ctx.session_id ~generation:ctx.generation)
          request
      in
      assert (
        Result.is_error
          (S.record
             (Option.value_exn !retained_scope)
             ~invocation_id:ctx.id
             ~capability_fingerprint:(C.fingerprint selected)
             response));
      assert (Option.is_none (S.capture ~invocation_id:ctx.id))
    in
    Eio.Fiber.both
      (fun () -> run (List.hd_exn contexts) entered_right signal_left)
      (fun () -> run (List.last_exn contexts) entered_left signal_right));
  print_endline
    "overlapping generations and host budgets isolated; nested helper retains owner; \
     duplicate reads coalesce; late borrows and collectors expire";
  [%expect
    {| overlapping generations and host budgets isolated; nested helper retains owner; duplicate reads coalesce; late borrows and collectors expire |}]
;;
