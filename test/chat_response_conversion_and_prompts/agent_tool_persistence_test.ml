open Core
module CM = Prompt.Chat_markdown
module Contract = Chat_response.Agent_tool_contract
module P = Agent_protocol

let%expect_test
    "authored policy controls model schema and per-call selection without changing \
     ordinary agents"
  =
  Eio_main.run (fun env ->
    let dir = Eio.Stdenv.cwd env in
    let declaration attribute =
      sprintf
        {|<tool name="reviewer" agent="review.chatmd" description="Review a change." %s/>|}
        attribute
    in
    let parse source =
      match CM.parse_chat_inputs ~dir source with
      | [ Tool tool ] -> tool
      | _ -> failwith "expected one agent tool"
    in
    let ordinary = parse (declaration "") in
    let explicit = parse (declaration {|persistence="one_off"|}) in
    [%test_eq: int] 0 (CM.compare_tool ordinary explicit);
    (match ordinary with
     | CM.Agent _ -> ()
     | _ -> failwith "ordinary identity changed");
    let id =
      P.Id.Session.of_string "ses_contract_child"
      |> Result.map_error ~f:(fun error -> error.P.Error.message)
      |> Result.ok_or_failwith
    in
    let input extra = `Object (("input", `String "Review the patch.") :: extra) in
    let report policy label json =
      match Contract.decode policy json with
      | Ok call ->
        print_s
          [%sexp
            (label : string)
          , (call.mode : Contract.mode)
          , (call.session_id : P.Id.Session.t option)]
      | Error error -> print_s [%sexp (label : string), (error.message : string)]
    in
    List.iter [ "optional"; "persistent" ] ~f:(fun spelling ->
      let parsed = parse (declaration (sprintf "persistence=\"%s\"" spelling)) in
      match parsed with
      | Persistent_agent (agent, policy) ->
        let description = Contract.description agent policy in
        assert (String.is_prefix description ~prefix:"Review a change.\n\n");
        assert (
          String.is_substring
            description
            ~substring:"agent_send, agent_read, agent_status, agent_wait and agent_stop");
        print_s [%sexp (spelling : string), (Contract.parameters policy : Jsonaf.t)];
        report policy "default" (input []);
        report policy "continue" (input [ "session_id", P.Id.Session.to_json id ]);
        report
          policy
          "select persistent"
          (input [ "mode", `String "persistent"; "session_id", P.Id.Session.to_json id ]);
        report
          policy
          "duplicate input"
          (`Object [ "input", `String "first"; "input", `String "second" ]);
        report policy "unknown option" (input [ "model", `String "replacement" ]);
        report policy "invalid ID" (input [ "session_id", `String "bad" ]);
        Eio.Switch.run (fun sw ->
          let ctx =
            Chat_response.Ctx.create
              ~env
              ~dir
              ~tool_dir:dir
              ~cache:(Chat_response.Cache.create ~max_size:1 ())
          in
          match
            Chat_response.Tool.of_declaration
              ~sw
              ~ctx
              ~run_agent:
                (fun
                  ?prompt_dir:_ ?session_id:_ ?observer:_ ~source:_ ~ctx:_ _ _ ->
                failwith "legacy execution must not run")
              parsed
          with
          | _ -> failwith "persistence silently used legacy runtime"
          | exception Failure message ->
            assert (String.is_prefix message ~prefix:"agent.persistence_unavailable:"))
      | _ -> failwith "missing persistence policy");
    List.iter
      [ declaration {|persistence="sometimes"|}
      ; declaration {|persistence="optional" persistence="persistent"|}
      ; {|<tool name="read_file" persistence="optional"/>|}
      ; {|<tool name="reviewer" agent="review.chatmd" type="inherited" persistence="optional"/>|}
      ]
      ~f:(fun source ->
        match parse source with
        | _ -> failwith "invalid authored policy accepted"
        | exception Failure message -> print_endline message));
  [%expect
    {|
    (optional
     (Object
      ((type (String object))
       (properties
        (Object
         ((input (Object ((type (String string)))))
          (session_id
           (Object
            ((type (String string))
             (description
              (String
               "Continue an existing instance of this authored tool. Omit to create a new persistent instance.")))))
          (mode
           (Object
            ((type (String string))
             (enum (Array ((String one_off) (String persistent))))
             (default (String one_off))))))))
       (required (Array ((String input)))) (additionalProperties False))))
    (default One_off ())
    (continue "One-off agent calls cannot include session_id.")
    ("select persistent" Persistent (ses_contract_child))
    ("duplicate input" "duplicate object field: input")
    ("unknown option" "Unexpected agent tool field: model")
    ("invalid ID" "identifier has the wrong type prefix")
    (persistent
     (Object
      ((type (String object))
       (properties
        (Object
         ((input (Object ((type (String string)))))
          (session_id
           (Object
            ((type (String string))
             (description
              (String
               "Continue an existing instance of this authored tool. Omit to create a new persistent instance."))))))))
       (required (Array ((String input)))) (additionalProperties False))))
    (default Persistent ())
    (continue Persistent (ses_contract_child))
    ("select persistent" "Unexpected agent tool field: mode")
    ("duplicate input" "duplicate object field: input")
    ("unknown option" "Unexpected agent tool field: model")
    ("invalid ID" "identifier has the wrong type prefix")
    Tool persistence must be one_off, persistent or optional, specified once.
    Tool persistence must be one_off, persistent or optional, specified once.
    Tool persistence is only supported on agent declarations.
    Tool persistence is only supported on agent declarations.
    |}]
;;
