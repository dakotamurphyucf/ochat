open Chatml_lang

type eval_result =
  | Value of value
  | TailCall of clos * value list

val finish_eval : Frame_env.env -> eval_result -> value

val eval_expr
  :  env
  -> Frame_env.env
  -> resolved_expr node
  -> eval_result

val eval_program
  :  env
  -> resolved_program
  -> unit
