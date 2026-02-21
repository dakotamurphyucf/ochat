(** ChatML script interpreter.

    This module implements the public executable
    [`dsl_script`].  The binary embeds a *hard-coded* snippet of
    ChatML source code, parses it, registers the standard built-in
    library and evaluates the program.

    The file is mostly intended as a smoke-test and demonstration of
    how the ChatML compiler pipeline can be driven from OCaml.

    {1 Pipeline}

    – {!val:parse} converts a raw source to an untyped AST \
      ({!type:Chatml_lang.stmt_node}) using the generated Menhir parser
      {!module:Chatml_parser}.
    – A fresh interpreter environment is allocated with
      {!Chatml_lang.create_env}.
    – {!Chatml_builtin_modules.add_global_builtins} injects the default
      standard library modules.
    – The program is then type-checked, resolved and executed by
      {!Chatml_resolver.eval_program}.

    The executable exits with the result of evaluating the last
    top-level expression or raises an exception if compilation fails. *)

open Core
open Chatml
open Chatml_builtin_modules

(** [parse src] returns the AST corresponding to the ChatML program
      contained in [src].

      @raise Failure if the parser encounters a syntax error *)
let parse (src : string) : Chatml_lang.stmt_node list =
  let lexbuf = Lexing.from_string src in
  try Chatml_parser.program Chatml_lexer.token lexbuf with
  | Chatml_parser.Error -> failwith "Parse error"
;;

let () =
  let open Chatml_lang in
  let env = create_env () in
  BuiltinModules.add_global_builtins env;
  let code =
    {|
    (* Type checking succeeded! Uncaught exception: (Failure "No field 'age' in record") *)
    (* --------------------------------------- *)

     let mk_task id title =
       { id = id
       ; title = title
       ; status = `Pending
       ; notes = ""
       ; attempts = 0
       }

     let init_state st =
       if st.inited then st else
         { inited = true
         ; autopilot = true
         ; task_index = 0
         ; task_count = 4
         ; tasks =
             [ mk_task("t1", "Survey the repo and identify relevant files/docs.")
             , mk_task("t2", "Write an implementation plan with a checklist.")
             , mk_task("t3", "Execute checklist items safely (read before edit), verify.")
             , mk_task("t4", "Final pass: summarize changes, risks, next steps.")
             ]
         }

     let current_task st =
       st.tasks[st.task_index]

     let set_task_status st new_status =
       let t = current_task(st) in
       t.status <- new_status;
       st

       let bump_attempts st =
         let t = current_task(st) in
         t.attempts <- t.attempts + 1;
         st

       let emit_task_action st =
         let t = current_task(st) in
         `InsertMsg(
           { role = `User
           ; synthetic = true
           ; via = "emit_task"
           ; text =
               "Current task (" ++ num2str(st.task_index + 1) ++ "/" ++ num2str(st.task_count) ++ ") [" ++ t.id ++ "]:\n\n"
               ++ t.title ++ "\n\n"
               ++ "Rules:\n"
               ++ "- Do only this task.\n"
               ++ "- End your message with a line `TASK_DONE` when complete.\n"
               ++ "- If blocked, include a line starting with `TASK_BLOCKED:`.\n"
           }
         )

       print("ssdd " ++ num2str(2323 + 1) ++ " ++")

       let finish_action () =
         `InsertMsg(
           { role = `User
           ; synthetic = true
           ; via = "finish"
           ; text =
               "All tasks complete. Provide a final consolidated summary:\n"
               ++ "- what was done per task\n"
               ++ "- what changed (files / high-level)\n"
               ++ "- remaining risks / follow-ups\n"
           }
         )

      print(finish_action ())
      module Orch = struct
      let has_line s d = true
      let has_prefix_line s d = false
      end
      let on_event ev st0 =
        let st = init_state(st0) in

        match ev.name with
        | `TurnStart ->
            if st.autopilot == false then `Tup(st, [])
            else
              (if st.task_index >= st.task_count then
              let st2 = { st with autopilot = false } in
              `Tup(st2, [ finish_action() ])
            else
              (* mark running + bump attempts, then emit task *)
              set_task_status(st, `Running);
              bump_attempts(st);
              `Tup(st, [ emit_task_action(st) ]))
          | `MessageAppended ->
                  if st.autopilot == false then `Tup(st, []) else
                  (match ev.last_assistant with
                   | `None -> `Tup(st, [])
                   | `Some(txt) ->
                       if Orch.has_line(txt, "TASK_DONE") then
                         set_task_status(st, `Done);
                         let st2 = { st with task_index = st.task_index + 1 } in
                         `Tup( st2
                         , [ `Compact({ keep = "system,developer,latest:6"
                                     ; strategy = "relevance+summary"
                                     ; threshold_tokens = 0
                                     ; via = "done_compact"
                                     })
                           ; `InsertMsg({ role = `User; synthetic = true; via = "next"; text = "Proceed to the next task." })
                           ]
                         )
                       else if Orch.has_prefix_line(txt, "TASK_BLOCKED:") then
                        set_task_status(st, `Blocked);
                         let st2 = { st with autopilot = false } in
                         `Tup( st2
                         , [ `InsertMsg({ role = `User; synthetic = true; via = "blocked"
                                       ; text = "Autopilot stopped (task blocked). Please respond to unblock, or edit tasks and resume."
                                       })
                           ]
                         )
                       else
                         `Tup(st, [])
                  )

                  | `TurnEnd ->
                      match `And(st.autopilot, ev.context_tokens > 40000) with
                      | `And(true, true) ->
                        `Tup( st
                        , [ `Compact({ keep = "system,developer,latest:8"
                                    ; strategy = "relevance+summary"
                                    ; threshold_tokens = 40000
                                    ; via = "size_compact"
                                    })
                          ]
                        )
                     | _ ->
                        `Tup(st, [])

                  | `PreToolCall ->
                      (* optional governance hooks here *)
                      `Tup(st, [])

                  | `PostToolResponse ->
                      `Tup(st, [])

      let d = [ mk_task("t1", "Survey the repo and identify relevant files/docs.")
      , mk_task("t2", "Write an implementation plan with a checklist.")
      , mk_task("t3", "Execute checklist items safely (read before edit), verify.")
      , mk_task("t4", "Final pass: summarize changes, risks, next steps.")
      ]
      let e = { inited = true
      ; autopilot = true
      ; task_index = 0
      ; task_count = 4
      ; tasks =
         d
      }
      print(on_event({name=`TurnStart; last_assistant=`None; context_tokens=299}, e ))

  |}
  in
  let _module_code =
    {|
    (* Type error: Extra field 'age' not allowed; record is closed on the other side *)
   let f p =
      let p = { name = "Charlie" } in

		  (* Now define inc_age in the same scope, so it knows p is a record: *)
		  let inc_age person =
		    person.age <- person.age + 1
		  in
      print([p.age]);
      inc_age(p)


    f("")
    (* Type checking succeeded!  [|25|]  *)
    let f s =
      let p = { name = "Charlie"; age = 25 } in

		  (* Now define inc_age in the same scope, so it knows p is a record: *)
		  let inc_age person =
		    person.age <- person.age + 1
		  in
      print([p.age]);
      inc_age(p)
		 let a = `Some(1, 2)
     let b = `None
     match a with
     | `Some(x, y) -> print([x; y])
     | `None -> print([0])

    f("")
  |}
  in
  let ast = parse code in
  Chatml_resolver.eval_program env (ast, code);
  print_endline "Program completed successfully."
;;
(* let file = (Sys.get_argv ()).(1) in *)
(* print_endline ("Read file: " ^ file) *)
