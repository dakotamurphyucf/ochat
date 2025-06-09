(** this module contains code for defining and implementing gpt functions that can be made availible to gpt prompt *)

open Core

(* Defines a module type for a gpt function definition. 
   This contains the metadata for a gpt function like name, description, and parameters. 
   Also defines a input_of_string function for converting string inputs from gpt model to the input type defined in the module
*)
module type Def = sig
  type input

  val name : string
  val description : string option
  val parameters : Jsonaf.t
  val input_of_string : string -> input
end

(* represents a gpt function implementation *)
type t =
  { info : Openai.Completions.tool
  ; run : string -> string
  }

(* takes a module of type Def and a function Def.input -> string and returns type t. Use to create a gpt function implementation for the given the  gpt function definition and implementation function *)
let create_function (type a) (module M : Def with type input = a) ?(strict = true) f =
  let run s = f @@ M.input_of_string s in
  let info =
    Openai.Completions.
      { type_ = "function"
      ; function_ =
          { name = M.name
          ; description = M.description
          ; parameters = M.parameters
          ; strict
          }
      }
  in
  { info; run }
;;

(** 
  takes a (t list) and returns a tuple with openai function defenitions and a hashtbl of the function implementations. Use this function to get the function definitions that need to be passed to the openai api, as well as get a hashtbl that maps function name to implementation so that you can locate the function implementation when the api returns a function call request
*)
let functions gpt_funcs =
  (* tbl of gpt function implementations *)
  let tbl = Hashtbl.create (module String) in
  let details =
    List.fold gpt_funcs ~init:[] ~f:(fun funcs t ->
      Hashtbl.add_exn tbl ~key:t.info.function_.name ~data:t.run;
      t.info :: funcs)
  in
  details, tbl
;;
