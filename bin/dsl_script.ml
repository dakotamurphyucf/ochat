open Core
open Chatml
open Chatml_builtin_modules

let parse str =
  let lexbuf = Lexing.from_string str in
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
    let p = {name = "Alice"; age = 25}
    let f p =
        let inc_age person =
            person.age <- person.age + 1;
            person
        in
        print(p.name);
        print(inc_age({p with age = 30 + p.age}))

    
    f(p)


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
		  
      
    f("")
  |}
  in
  let ast = parse code in
  Chatml_typechecker.infer_program (ast, code);
  eval_program env (ast, code)
;;
