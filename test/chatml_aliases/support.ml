open Core
module L = Chatml.Chatml_lang
module T = Chatml_typechecker

let parse = Chatml.Chatml_parse.parse_program_exn
let check source = T.check_program (parse source)

let run source =
  let env = Chatml_builtin_modules.create_default_env () in
  match Chatml_resolver.run_program env (parse source) with
  | Error (Type_diagnostic diagnostic) -> failwith (T.format_diagnostic source diagnostic)
  | Error (Runtime_diagnostic diagnostic) -> failwith diagnostic.message
  | Ok () -> Option.value_exn (L.find_var env "result")
;;

let require source expected =
  match check source, expected with
  | Ok _, true -> ()
  | Error { span = Some _; _ }, false -> ()
  | Error diagnostic, _ -> failwith (T.format_diagnostic source diagnostic)
  | Ok _, false -> failwith ("Unexpectedly accepted:\n" ^ source)
;;
