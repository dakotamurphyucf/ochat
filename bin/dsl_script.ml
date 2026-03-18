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
let parse (str : string) : Chatml_lang.program =
  let lexbuf = Lexing.from_string str in
  try
    { Chatml_lang.stmts = Chatml_parser.program Chatml_lexer.token lexbuf
    ; source_text = str
    }
  with
  | Chatml_parser.Error -> failwith "Parse error"
;;

let () =
  let open Chatml_lang in
  let env = BuiltinModules.create_default_env () in
  let code =
    {|
      (* Type checking succeeded! Uncaught exception: (Failure "No field 'age' in record") *)
      (* --------------------------------------- *)

       type status = [ `Pending | `Running | `Done | `Blocked ]
       type task =
         { id : string
         ; title : string
         ; status : status
         ; notes : string
         ; attempts : int
         }
       type state =
         { inited : bool
         ; autopilot : bool
         ; task_index : int
         ; task_count : int
         ; tasks : task array
         }
       type role = [ `User ]
       type insert_msg =
         { role : role
         ; synthetic : bool
         ; via : string
         ; text : string
         }
       type compact_action =
         { keep : string
         ; strategy : string
         ; threshold_tokens : int
         ; via : string
         }
       type action = [ `InsertMsg(insert_msg) | `Compact(compact_action) ]
       type maybe_text = [ `None | `Some(string) ]
       type event_name =
         [ `TurnStart
         | `MessageAppended
         | `TurnEnd
         | `PreToolCall
         | `PostToolResponse
         ]
       type orch_event =
         { name : event_name
         ; last_assistant : maybe_text
         ; context_tokens : int
         }
       type outcome = [ `Tup(state, action array) ]

       let mk_task : string -> string -> task =
         fun id title ->
           { id = id
           ; title = title
           ; status = `Pending
           ; notes = ""
           ; attempts = 0
           }

       let init_state : state -> state =
         fun st -> st

       let current_task : state -> task =
         fun st ->
           st.tasks[st.task_index]

       let set_task_status : state -> status -> state =
         fun st new_status ->
           let t : task = st.tasks[st.task_index] in
           let t : task = { t with status = new_status } in
           st.tasks[st.task_index] <- t;
           st

        let bump_attempts : state -> state =
         fun st ->
           let t : task = st.tasks[st.task_index] in
           let t : task = { t with attempts = t.attempts + 1 } in
           st.tasks[st.task_index] <- t;
           st

         let emit_task_action : state -> action =
           fun st ->
             let t : task = current_task(st) in
             `InsertMsg(
               { role = `User
               ; synthetic = true
               ; via = "emit_task"
               ; text =
                   "Current task (" ++ to_string(st.task_index + 1) ++ "/" ++ to_string(st.task_count) ++ ") [" ++ t.id ++ "]:\n\n"
                   ++ t.title ++ "\n\n"
                   ++ "Rules:\n"
                   ++ "- Do only this task.\n"
                   ++ "- End your message with a line `TASK_DONE` when complete.\n"
                   ++ "- If blocked, include a line starting with `TASK_BLOCKED:`.\n"
               }
             )

         print("ssdd " ++ to_string(2323 + 1) ++ " ++")

         let finish_action : unit -> action =
           fun () ->
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
        let on_event : orch_event -> state -> outcome =
          fun ev st0 ->
          let st = st0 in

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
                        (match `And(st.autopilot, ev.context_tokens > 40000) with
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
                          `Tup(st, []))

                    | `PreToolCall ->
                        (* optional governance hooks here *)
                        `Tup(st, [])

                    | `PostToolResponse ->
                        `Tup(st, [])

        let d : task array =
          [ mk_task("t1", "Survey the repo and identify relevant files/docs.")
          , mk_task("t2", "Write an implementation plan with a checklist.")
          , mk_task("t3", "Execute checklist items safely (read before edit), verify.")
          , mk_task("t4", "Final pass: summarize changes, risks, next steps.")
          ]
        let e : state =
          { inited = true
          ; autopilot = true
          ; task_index = 0
          ; task_count = 4
          ; tasks =
             d
          }
        print(on_event({name=`TurnStart; last_assistant=`None; context_tokens=299}, e ))
    |}
  in
  let ast = parse code in
  let res = Chatml_resolver.eval_program env ast in
  (match res with
   | Ok () -> ()
   | Error diagnostic -> failwith (Chatml_typechecker.format_diagnostic code diagnostic));
  print_endline "Program completed successfully.";
  let task_runner_code =
    {|(* A tiny workflow engine that processes events and mutates tasks in-place. *)

  type status = [ `Pending | `Running | `Done | `Error(string) ]
  type task =
    { name : string
    ; attempts : int
    ; status : status
    }
  type event = [ `Start | `Tick | `Fail(string) | `Stop ]
  type state =
    { tasks : task array
    ; idx : int
    ; running : bool
    }

  (* Witness: forces the status variant row to include all tags we will use. *)
  let status_witness : int -> status =
    fun n ->
      match n with
      | 0 -> `Pending
      | 1 -> `Running
      | 2 -> `Done
      | _ -> `Error("")

  let mk_task : string -> task =
    fun name ->
      { name = name; attempts = 0; status = status_witness(0) }

  let show_task : task -> string =
    fun t ->
      t.name ++ " attempts=" ++ to_string(t.attempts) ++ " status=" ++ variant_tag(t.status)

  let set_status : state -> status -> state =
    fun st new_status ->
      let i = st.idx in
      let t : task = st.tasks[i] in
      st.tasks[i] <- { t with status = new_status };
      st

  let set_running : state -> bool -> state =
    fun st running ->
      { st with running = running }

  let bump_attempts : state -> state =
    fun st ->
      let i = st.idx in
      let t : task = st.tasks[i] in
      st.tasks[i] <- { t with attempts = t.attempts + 1 };
      st

  let step : state -> event -> state =
    fun st ev ->
      match ev with
      | `Start ->
        if st.idx >= length(st.tasks) then set_running(st, false)
        else set_status(set_running(st, true), status_witness(1))

      | `Tick ->
        if st.running == false then set_running(st, false)
        else
          let st = set_running(st, true) in
          let st = bump_attempts(st) in
          let t : task = st.tasks[st.idx] in
          if t.attempts >= 3 then
            let st = set_status(st, `Done) in
            let st = { st with idx = st.idx + 1 } in
            if st.idx < length(st.tasks) then set_status(set_running(st, true), status_witness(1)) else st
          else st

      | `Fail(msg) ->
        let st = set_status(st, `Error(msg)) in
        set_running(st, false)

      | `Stop ->
        set_running(st, false)

  let run : event array -> unit =
    fun events ->
    let tasks = [ mk_task("fetch"), mk_task("transform"), mk_task("upload") ] in
    let st0 = { tasks = tasks; idx = 0; running = false } in

    let i = ref(0) in
    let st_ref = ref(st0) in

    while !i < length(events) do
      let ev = events[!i] in

      st_ref := step(!st_ref, ev);

      let st = !st_ref in
      print(
        "ev=" ++ variant_tag(ev) ++
        " idx=" ++ to_string(st.idx) ++
        " task=" ++
          (if st.idx < length(st.tasks)
            then show_task(st.tasks[st.idx])
            else "<none>")
      );

      i := !i + 1
    done

  let events =
    [ `Start
    , `Tick, `Tick, `Tick
    , `Tick, `Tick, `Tick
    , `Tick, `Fail("network")
    , `Stop
    ]

  run(events)|}
  in
  let env = BuiltinModules.create_default_env () in
  let ast = parse task_runner_code in
  let res = Chatml_resolver.eval_program env ast in
  (match res with
   | Ok () -> ()
   | Error diagnostic ->
     failwith (Chatml_typechecker.format_diagnostic task_runner_code diagnostic));
  print_endline "Task Runner Program completed successfully.";
  let small_expr_code =
    {|
        type expr =
          [ `Int(int)
          | `Add(expr, expr)
          | `Sub(expr, expr)
          | `Mul(expr, expr)
          | `Div(expr, expr)
          | `Let(string, expr, expr)
          | `Var(string)
          ]

        let rec eval : expr -> int =
          fun e ->
          match e with
          | `Int(n) -> n
          | `Add(a, b) -> eval(a) + eval(b)
          | `Sub(a, b) -> eval(a) - eval(b)
          | `Mul(a, b) -> eval(a) * eval(b)
          | `Div(a, b) ->
              let x = eval(a) in
              let y = eval(b) in
              if y == 0 then fail("division by zero in AST")
              else x / y
          | `Let(name, rhs, body) ->
              (* An environment-less "let" by substitution for demo purposes:
                 Let only supports binding `x` here. *)
              if name != "x" then fail("only name \"x\" is supported in this demo")
              else
                let v = eval(rhs) in
                eval(subst_x(body, v))
          | `Var(name) -> fail("free variable in AST: " ++ name)

        and subst_x : expr -> int -> expr =
          fun e v ->
          match e with
          | `Int(_) -> e
          | `Var(name) ->
              if name == "x" then `Int(v) else e
          | `Add(a, b) -> `Add(subst_x(a, v), subst_x(b, v))
          | `Sub(a, b) -> `Sub(subst_x(a, v), subst_x(b, v))
          | `Mul(a, b) -> `Mul(subst_x(a, v), subst_x(b, v))
          | `Div(a, b) -> `Div(subst_x(a, v), subst_x(b, v))
          | `Let(name, rhs, body) ->
              if name == "x" then
                (* shadowing: don't substitute into body *)
                `Let(name, subst_x(rhs, v), body)
              else
                `Let(name, subst_x(rhs, v), subst_x(body, v))

        let program : expr =
          `Let("x",
               `Add(`Int(10), `Int(5)),
               `Div(`Mul(`Var("x"), `Int(2)), `Sub(`Int(9), `Int(7)))
          )

        print("result=" ++ to_string(eval(program)))
    |}
  in
  let env = BuiltinModules.create_default_env () in
  let ast = parse small_expr_code in
  let res = Chatml_resolver.eval_program env ast in
  (match res with
   | Ok () -> ()
   | Error diagnostic ->
     failwith (Chatml_typechecker.format_diagnostic small_expr_code diagnostic));
  print_endline "Small expr Program completed successfully.";
  let bfs_code =
    {|
      let and_ a b =
        match `Tup(a, b) with
        | `Tup(true, true) -> true
        | _ -> false

      module Graph = struct
        (* adjacency matrix: g[u][v] = 1 if edge *)
        let neighbors g u = g[u]

        let bfs_distance g start goal =
          let n = length(g) in

          (* distances initialized to -1 (unvisited) *)
          let dist = [-1, -1, -1, -1, -1, -1] in
          dist[start] <- 0;

          (* simple fixed-size queue of nodes *)
          let q = [0, 0, 0, 0, 0, 0] in
          let head = ref(0) in
          let tail = ref(0) in

          q[0] <- start;
          tail := 1;

          while !head < !tail do
            let u = q[!head] in
            head := !head + 1;

            let du = dist[u] in
            let row = neighbors(g, u) in

            let v = ref(0) in
            while !v < n do
              if (and_(row[!v] == 1, dist[!v] == -1)) then
                dist[!v] <- du + 1;
                q[!tail] <- !v;
                tail := !tail + 1
              else ();
              v := !v + 1
            done
          done;

          dist[goal]
      end

      let g =
        [ [0,1,1,0,0,0]
        , [1,0,0,1,0,0]
        , [1,0,0,1,1,0]
        , [0,1,1,0,0,1]
        , [0,0,1,0,0,1]
        , [0,0,0,1,1,0]
        ]

      print("dist 0->5 = " ++ to_string(Graph.bfs_distance(g, 0, 5)))
    |}
  in
  let env = BuiltinModules.create_default_env () in
  let ast = parse bfs_code in
  let res = Chatml_resolver.eval_program env ast in
  (match res with
   | Ok () -> ()
   | Error diagnostic ->
     failwith (Chatml_typechecker.format_diagnostic bfs_code diagnostic));
  print_endline "BFS Program completed successfully.";
  let config_code =
    {|
        (* Helpers that work on any record with at least certain fields. *)

        let require_enabled cfg =
          if cfg.enabled == true then cfg else fail("feature disabled")

        let with_timeout cfg ms =
          (* copy-update can add fields and change types *)
          { cfg with timeout_ms = ms }

        let describe cfg =
          "keys=" ++ to_string(record_keys(cfg))

        let cfg0 = { name = "ingest"; enabled = true }
        let cfg1 = require_enabled(cfg0)
        let cfg2 = with_timeout(cfg1, 2500)

        print(describe(cfg0))
        print(describe(cfg2))
        print("timeout=" ++ to_string(cfg2.timeout_ms))
      |}
  in
  let env = BuiltinModules.create_default_env () in
  let ast = parse config_code in
  let res = Chatml_resolver.eval_program env ast in
  (match res with
   | Ok () -> ()
   | Error diagnostic ->
     failwith (Chatml_typechecker.format_diagnostic config_code diagnostic));
  print_endline "Config Program completed successfully."
;;
(* let file = (Sys.get_argv ()).(1) in *)
(* print_endline ("Read file: " ^ file) *)
