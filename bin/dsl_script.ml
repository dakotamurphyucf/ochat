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
    let p = {name = "Alice"; age = 25}
    let f p =
        let inc_age person =
            person.age <- person.age + 1;
            person
        in
        print(p.name);
        print(inc_age({p with age = 30 + p.age}))

    
    f(p)
    let a = `Some(1, 2)
     let b = `None
     match a with
     | `Some(x, y) -> print([x, y])
     | `None -> print([0])


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
  Chatml_resolver.eval_program env (ast, code)
;;
