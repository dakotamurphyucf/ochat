open Core
open Fixtures
module Q = Authoring_context_tests
module Flow = Authoring_compaction_tests

(* Repairs deliberately come from the installed reference returned by the native
   query tool. This is a scripted provider transcript, not a model-quality test. *)
let example ?(language = "ocaml") text id =
  let marker = "Example " ^ id ^ " (" in
  let rec find = function
    | [] -> failwith ("reference did not contain example " ^ id)
    | line :: rest ->
      (match String.is_substring line ~substring:marker with
       | false -> find rest
       | true -> fence rest)
  and fence = function
    | opening :: rest when String.equal opening ("```" ^ language) ->
      let code, closing =
        List.split_while rest ~f:(fun line -> not (String.equal line "```"))
      in
      assert (not (List.is_empty closing));
      String.concat ~sep:"\n" code
    | _ :: rest -> fence rest
    | [] -> failwith "missing documented code fence"
  in
  find (String.split_lines text)
;;

let inline ?(tools = []) target source =
  `Object
    ([ "version", `Number "1"
     ; "target", `String target
     ; "source", `String source
     ; "tools", `Array (List.map tools ~f:(fun name -> `String name))
     ]
     @
     match target with
     | "standalone_tool" -> [ "input_schema", `True; "output_schema", `True ]
     | _ -> [])
;;

let bundle sources =
  `Object
    [ "version", `Number "1"
    ; "target", `String "generated_chatmd"
    ; "root_file", `String "child.chatmd"
    ; "tools", `Array [ `String "read_file" ]
    ; ( "sources"
      , `Array
          (List.map sources ~f:(fun (path, text) ->
             `Object [ "path", `String path; "text", `String text ])) )
    ]
;;

type candidate =
  { id : string
  ; task : string
  ; topic : string
  ; invalid : Jsonaf.t
  ; repair : string -> Jsonaf.t
  }

let candidates =
  let language id topic source example_id =
    { id
    ; task = "one_off_script"
    ; topic
    ; invalid = inline "one_off_script" source
    ; repair = (fun text -> inline "one_off_script" (example text example_id))
    }
  in
  let generated =
    {|<developer>Read only the delegated reports.</developer>
<import src="tools.chatmd"/>
<script id="owner" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = fail("VALIDATION-MUST-NOT-EVALUATE")
let on_event ctx state event = Task.pure(state)
</script>|}
  in
  [ language
      "calls"
      "chatml.syntax.calls"
      [%blob "../chatml_extensibility_fixtures/x11-authoring-repair/invalid-call.chatml"]
      "calls.wrapper"
  ; language
      "records"
      "chatml.types"
      "let main input = let record = {name = \"Bob\"} in\n\
       match record with | {age = a} -> Task.pure(`String(a))"
      "records.open-pattern"
  ; language
      "variants"
      "chatml.types"
      "let close v = match v with | `None -> `None | `Some(x) -> `Some(x)\n\
       let read v = close(v); match v with | `None -> 0 | `Some(x) -> x | _ -> 2\n\
       let main input = Task.pure(`Bool(read(`Some(1)) == 1))"
      "variants.closed-total"
  ; language
      "task"
      "runtime.invocations.one-off"
      "let main input = Task.pure({answer = 42})"
      "tasks.bind-map"
  ; { id = "standalone"
    ; task = "standalone_tool"
    ; topic = "runtime.invocations.standalone"
    ; invalid = inline "standalone_tool" "let run ctx input = Task.pure(42)"
    ; repair =
        (fun text ->
          inline
            ~tools:[ "read_file" ]
            "standalone_tool"
            (example text "runtime.standalone.compare"))
    }
  ; { id = "moderator"
    ; task = "moderator_tool"
    ; topic = "runtime.invocations.moderator"
    ; invalid =
        inline
          "moderator"
          "let initial_state = 0\nlet on_event ctx state event = state + 1"
    ; repair = (fun text -> inline "moderator" (example text "runtime.moderator.reviews"))
    }
  ; { id = "declaration"
    ; task = "child_agent"
    ; topic = "chatmd.declarations.schemas"
    ; invalid =
        bundle
          [ ( "child.chatmd"
            , {|<script id="owner" language="chatml" kind="unknown">let main input = Task.pure(input)</script>|}
            )
          ]
    ; repair =
        (fun text ->
          assert (String.is_substring text ~substring:"api=\"extensibility-v1\"");
          bundle
            [ "child.chatmd", generated
            ; "tools.chatmd", {|<tool type="inherited" name="read_file"/>|}
            ])
    }
  ; { id = "inherited-declaration"
    ; task = "child_agent"
    ; topic = "chatmd.definitions"
    ; invalid = bundle [ "child.chatmd", {|<tool name="read_file"/>|} ]
    ; repair =
        (fun text ->
          bundle
            [ "child.chatmd", example ~language:"xml" text "chatmd.inherited-reader" ])
    }
  ; { id = "import"
    ; task = "child_agent"
    ; topic = "runtime.delegation.generated"
    ; invalid = bundle [ "child.chatmd", {|<import src="../private.chatmd"/>|} ]
    ; repair =
        (fun text ->
          assert (String.is_substring text ~substring:"inherited");
          bundle
            [ "child.chatmd", generated
            ; "tools.chatmd", {|<tool type="inherited" name="read_file"/>|}
            ])
    }
  ]
;;

type stage =
  | Invalid
  | References
  | Repaired
  | Executed
  | Finished

let%expect_test "X11 native diagnostics drive retrieved repairs without granting effects" =
  let stage = ref Invalid in
  let inputs = ref [] in
  let pending = ref [] in
  let pages = String.Table.create () in
  let repaired = String.Table.create () in
  let final_count = ref Int.max_value in
  let query_count = ref 0 in
  let reader =
    {|let poison = fail("VALIDATION-MUST-NOT-EVALUATE")
let main input =
  let* result = Tool.call("read_file", input) in
  match result with
  | `Ok(value) -> Task.pure(value)
  | `Error(code) -> Task.fail(code)|}
  in
  let executable_reader =
    String.chop_prefix_exn
      reader
      ~prefix:"let poison = fail(\"VALIDATION-MUST-NOT-EVALUATE\")\n"
  in
  let execute ?(extra = []) ~tools source =
    `Object
      ([ "source", `String source
       ; "input", `Object [ "root", `String "reports"; "file", `String "../secret.json" ]
       ; "tools", `Array (List.map tools ~f:(fun tool -> `String tool))
       ]
       @ extra)
  in
  let response id = Flow.response !inputs id in
  let report id valid =
    let value = response id in
    (match Bool.equal valid (Q.field value "valid" |> Jsonaf.bool_exn) with
     | true -> ()
     | false ->
       raise_s
         [%sexp
           "unexpected validation", (id : string), (valid : bool), (value : Jsonaf.t)]);
    value
  in
  let query candidate request =
    incr query_count;
    let id = "reference-" ^ Int.to_string !query_count in
    pending := (candidate, id) :: !pending;
    id, "ochat_authoring_context", request
  in
  with_daemon
    ~sources:
      [ ( "agent.chatmd"
        , [%blob "../chatml_extensibility_fixtures/x11-authoring-repair/agent.chatmd"] )
      ]
    ~calls:(List.map candidates ~f:(fun c -> "bad-" ^ c.id, "ochat_validate", c.invalid))
    ~request_counts:(fun () -> !final_count, !final_count)
    ~inspect_request:(fun number actual ->
      assert (number < 80);
      inputs := actual)
    ~followup_calls:(fun number ->
      match !stage with
      | Invalid ->
        stage := References;
        List.map candidates ~f:(fun c ->
          let diagnostics =
            Q.field (report ("bad-" ^ c.id) false) "diagnostics" |> Jsonaf.list_exn
          in
          let topics =
            List.concat_map diagnostics ~f:(fun d ->
              Q.field d "topic_ids" |> Jsonaf.list_exn |> List.map ~f:Jsonaf.string_exn)
          in
          assert (List.mem topics c.topic ~equal:String.equal);
          query c (Q.request ~task:c.task ~topic_id:c.topic ~max_tokens:32000 "topic"))
      | References ->
        let previous = !pending in
        pending := [];
        let continuations =
          List.filter_map previous ~f:(fun (c, id) ->
            let page = response id in
            assert (not (Q.has_error page));
            Hashtbl.add_multi pages ~key:c.id ~data:page;
            match Q.field page "next_cursor" with
            | `String cursor ->
              Some (query c (Q.request ~cursor ~max_tokens:32000 "continue"))
            | `Null ->
              Q.require_json `True (Q.field page "complete");
              None
            | _ -> assert false)
        in
        (match continuations with
         | _ :: _ -> continuations
         | [] ->
           stage := Repaired;
           let calls =
             List.map candidates ~f:(fun c ->
               let text = Flow.content (Hashtbl.find_multi pages c.id |> List.rev) in
               let request = c.repair text in
               Hashtbl.set repaired ~key:c.id ~data:request;
               "fixed-" ^ c.id, "ochat_validate", request)
           in
           calls
           @ [ ( "poison-reader"
               , "ochat_validate"
               , inline ~tools:[ "read_file" ] "one_off_script" reader )
             ; ( "reader"
               , "ochat_validate"
               , inline ~tools:[ "read_file" ] "one_off_script" executable_reader )
             ; ( "narrowed-reader"
               , "ochat_validate"
               , inline "one_off_script" executable_reader )
             ])
      | Repaired ->
        List.iter candidates ~f:(fun c ->
          ignore (report ("fixed-" ^ c.id) true : Jsonaf.t));
        let identity id = Q.field (report id true) "validation_id" |> Jsonaf.string_exn in
        let receipt = identity "reader" in
        assert (not (String.equal receipt (identity "poison-reader")));
        assert (not (String.equal receipt (identity "narrowed-reader")));
        stage := Executed;
        List.filter_map candidates ~f:(fun c ->
          match c.task with
          | "one_off_script" ->
            let source =
              Q.field (Hashtbl.find_exn repaired c.id) "source" |> Jsonaf.string_exn
            in
            Some ("execute-" ^ c.id, "run_chatml", execute ~tools:[] source)
          | _ -> None)
        @ [ "narrowed-execution", "run_chatml", execute ~tools:[] executable_reader
          ; "file-denial", "run_chatml", execute ~tools:[ "read_file" ] executable_reader
          ; ( "forged-receipt"
            , "run_chatml"
            , execute
                ~tools:[ "read_file" ]
                ~extra:[ "validation_id", `String receipt ]
                (executable_reader ^ "\n(* changed *)") )
          ]
      | Executed ->
        stage := Finished;
        final_count := number;
        []
      | Finished -> failwith "unexpected repair model turn")
    (fun state ->
       (* Native validation/query calls may record their own results, but no
         candidate gets an invocation, job, session, or effect during validation. *)
       List.iter state.invocations ~f:(fun invocation ->
         match invocation.context.tool_name with
         | "ochat_validate" | "ochat_authoring_context" ->
           assert (List.is_empty (children state invocation))
         | _ -> ());
       List.iter [ "calls"; "variants" ] ~f:(fun id ->
         assert (I.equal_outcome (Complete `True) (result state ("execute-" ^ id))));
       assert (I.equal_outcome (Complete (`String "Bob")) (result state "execute-records"));
       assert (I.equal_outcome (Complete (`String "ready")) (result state "execute-task"));
       List.iter [ "narrowed-execution"; "forged-receipt" ] ~f:(fun id ->
         match result state id with
         | Fail error -> print_s [%sexp (id : string), (error : I.tool_error)]
         | other -> raise_s [%sexp (id : string), (other : I.outcome)]);
       (* The legacy reader reports permission failures as text. The passthrough
          script must preserve that denial, and must never receive file bytes. *)
       assert (
         I.equal_outcome
           (Complete
              (`String
                  "error running read_file: requested file is outside the configured \
                   read roots"))
           (result state "file-denial"));
       [%test_eq: int] 1 (List.length (native_reads state));
       assert (
         not
           (String.is_substring
              (Sexp.to_string (Agent_session.Session_state.sexp_of_t state))
              ~substring:"PRIVATE-REPORT-SENTINEL"));
       print_endline
         "9 invalid candidates -> linked installed references -> 9 valid repairs";
       print_endline
         "4 documented programs executed; poison initializers and generated sessions \
          never executed";
       print_endline
         "source/selection change identities; tool/file denial and forged-receipt \
          rejection remain enforced");
  [%expect
    {|
    (narrowed-execution
     ((code chatml.execution_failed) (message invocation.unselected_tool)
      (retryable false) (details Null)))
    (forged-receipt
     ((code invocation.invalid_input)
      (message "The original tool arguments do not satisfy its input schema.")
      (retryable false) (details Null)))
    9 invalid candidates -> linked installed references -> 9 valid repairs
    4 documented programs executed; poison initializers and generated sessions never executed
    source/selection change identities; tool/file denial and forged-receipt rejection remain enforced
    |}]
;;
