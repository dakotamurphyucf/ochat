open! Core
module L = Chatml.Chatml_lang
module T = Chatml_typechecker

let parse = Chatml.Chatml_parse.parse_program_exn
let check source = T.check_program (parse source)

let run source =
  let env = Chatml_builtin_modules.create_default_env () in
  match Chatml_resolver.run_program env (parse source) with
  | Ok () -> Option.value_exn (L.find_var env "result")
  | Error (Type_diagnostic diagnostic) -> failwith (T.format_diagnostic source diagnostic)
  | Error (Runtime_diagnostic diagnostic) -> failwith diagnostic.message
;;

let require source ~accepted =
  match check source, accepted with
  | Ok _, true -> ()
  | Error { span = Some _; _ }, false -> ()
  | Error diagnostic, _ -> failwith (T.format_diagnostic source diagnostic)
  | Ok _, false -> failwith ("Unexpected acceptance: " ^ source)
;;

let assert_checkpoints ~f =
  let observed = ref 0 in
  f (fun () -> Int.incr observed);
  assert (!observed > 0);
  let exception Stop of unit ref in
  for position = 1 to !observed do
    let token = ref () in
    let polls = ref 0 in
    (match
       f (fun () ->
         Int.incr polls;
         if Int.equal !polls position then raise (Stop token))
     with
     | () -> failwith "Checkpoint exception swallowed"
     | exception Stop actual ->
       assert (phys_equal actual token);
       [%test_eq: int] !polls position);
    f (fun () -> ())
  done
;;
