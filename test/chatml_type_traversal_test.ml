open Core
module T = Chatml_typechecker
module L = Chatml.Chatml_lang
module B = Chatml.Chatml_builtin_spec
module S = Chatml.Chatml_builtin_surface

let shared_type depth =
  let rec loop remaining ty =
    match remaining with
    | 0 -> ty
    | _ -> loop (remaining - 1) (T.Con ("pair", [ ty; ty ]))
  in
  loop depth T.TInt
;;

let row prefix =
  List.fold (List.init 512 ~f:Fn.id) ~init:T.Empty_row ~f:(fun rest index ->
    T.Row (T.Env.singleton (prefix ^ Int.to_string index) T.TInt, rest))
;;

let cancelled name run =
  let exception Cancelled_by_host in
  let polls = ref 0 in
  let state =
    T.create_state
      ~checkpoint:(fun () ->
        Int.incr polls;
        if !polls = 2 then raise Cancelled_by_host)
      ()
  in
  match run state (fun () -> T.checkpoint state) with
  | () -> failwith (name ^ ": finished without observing cancellation")
  | exception Cancelled_by_host -> print_endline (name ^ ": cancelled")
;;

let%expect_test "inference-owned type inspections and row operations cooperate" =
  (* These fixtures also terminate without polling, so a missing callback fails
     the test instead of leaving an exponential stress test running forever. *)
  let ty = shared_type 14 in
  let left = T.Record (row "left") in
  let right = T.Record (row "right") in
  cancelled "lookup" (fun state _ ->
    ignore (T.lookup state (T.Env.singleton "value" ty) "value" : T.typ));
  cancelled "pattern binding" (fun state _ ->
    ignore (T.infer_pattern state T.Env.empty (L.PVar "value") ty : T.tenv));
  cancelled "type declaration" (fun state _ ->
    ignore
      (T.infer_stmt
         state
         T.Env.empty
         (T.Env.singleton "seed" ty)
         (L.SType ("alias", L.TEName "seed"))
       : T.tenv * T.type_env));
  cancelled "record coverage" (fun state _ ->
    T.validate_match_exhaustiveness state left [ L.PRecord ([], false) ]);
  cancelled "disjoint record join" (fun state _ ->
    ignore (T.join_type state left right : T.typ));
  cancelled "lambda row reopening" (fun state _ ->
    ignore (T.reopen_lambda_param_type state left : T.typ));
  [%expect
    {|
    lookup: cancelled
    pattern binding: cancelled
    type declaration: cancelled
    record coverage: cancelled
    disjoint record join: cancelled
    lambda row reopening: cancelled
    |}]
;;

let%expect_test
    "surface initialization and entrypoint conversion inherit compiler control"
  =
  let rec ty depth =
    match depth with
    | 0 -> B.TInt
    | _ ->
      let nested = ty (depth - 1) in
      B.TRecord (B.TRow_extend ([ "left", nested; "right", nested ], B.TRow_empty))
  in
  let large = ty 14 in
  let program = Chatml.Chatml_parse.parse_program_exn "" in
  let initialized = ref false in
  let builtin : B.builtin =
    { name = "unexecuted"
    ; scheme = large
    ; impl =
        (fun _ ->
          initialized := true;
          L.VUnit)
    }
  in
  List.iter
    [ "global surface", { S.empty with globals = [ builtin ] }, []
    ; ( "alias surface"
      , { S.empty with type_aliases = [ { name = "large"; body = large } ] }
      , [] )
    ; "entrypoint contract", S.empty, [ "main", large ]
    ]
    ~f:(fun (name, surface, required_bindings) ->
      cancelled name (fun _ poll ->
        ignore
          (T.check_program_with_surface
             ~checkpoint:poll
             ~required_bindings
             surface
             program
           : (T.checked_program, T.diagnostic) Result.t)));
  print_s [%sexp (!initialized : bool)];
  [%expect
    {|
    global surface: cancelled
    alias surface: cancelled
    entrypoint contract: cancelled
    false
    |}]
;;

let%expect_test
    "annotation conversion cooperates and normal unit-arrow contracts stay intact"
  =
  let rec annotation depth =
    match depth with
    | 0 -> L.TEName "int"
    | _ ->
      let nested = annotation (depth - 1) in
      L.TERecord [ "left", nested; "right", nested ]
  in
  cancelled "annotation" (fun _ poll ->
    ignore (T.typ_of_type_expr ~checkpoint:poll T.Env.empty (annotation 14) : T.typ));
  List.iter
    [ "type unit_alias = unit\nlet f : unit_alias -> int = fun () -> 1"
    ; "let f : int -> unit -> int = fun x -> x"
    ; "type node = { value : int; next : node array }\n\
       let count : node -> int = fun n -> n.value"
    ]
    ~f:(fun source ->
      let program = Chatml.Chatml_parse.parse_program_exn source in
      match T.check_program program with
      | Ok _ -> print_endline "compiled"
      | Error diagnostic -> failwith diagnostic.message);
  [%expect
    {|
    annotation: cancelled
    compiled
    compiled
    compiled
    |}]
;;

let%expect_test
    "cancelling type inspection joins its Eio domain and permits fresh compilation"
  =
  Eio_main.run (fun env ->
    let ready, signal = Eio.Promise.create () in
    let joined = Atomic.make false in
    let result =
      Eio.Fiber.first
        (fun () ->
           Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env) (fun () ->
             Exn.protect
               ~finally:(fun () -> Atomic.set joined true)
               ~f:(fun () ->
                 let started = ref false in
                 let state =
                   T.create_state
                     ~checkpoint:(fun () ->
                       (match !started with
                        | false ->
                          started := true;
                          Eio.Promise.resolve signal ()
                        | true -> ());
                       Eio.Fiber.yield ())
                     ()
                 in
                 ignore
                   (T.lookup state (T.Env.singleton "value" (shared_type 16)) "value"
                    : T.typ);
                 "completed")))
        (fun () ->
           Eio.Promise.await ready;
           "cancelled")
    in
    print_s [%sexp (result : string), (Atomic.get joined : bool)];
    match
      Chatml_compilation.compile
        ~env
        ~target:One_off_v1
        ~source:"let main input = Task.pure(input)"
        ()
    with
    | Ok _ -> print_endline "fresh compilation succeeded"
    | Error error -> failwith error.message);
  [%expect
    {|
    (cancelled true)
    fresh compilation succeeded
    |}]
;;
