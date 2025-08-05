open Core
open Meta_prompting

(* A judge that rewards longer prompts, ensuring that any metadata appended by
   [Recursive_mp.transform_prompt] increases the score and therefore makes the
   refinement win over the original draft. *)

module Len_judge : Evaluator.Judge = struct
  let name = "len"
  let evaluate ?env:_ candidate = Float.of_int (String.length candidate)
end

let%expect_test "refined_prompt_diff" =
  let orig_msg = "Hello world" in
  let prompt = Prompt_intf.make ~body:orig_msg () in
  let refined_prompt =
    Recursive_mp.refine
      ~judges:[ Judge (module Len_judge : Evaluator.Judge) ]
      ~max_iters:2
      prompt
  in
  let refined_msg = Prompt_intf.to_string refined_prompt in
  let diff_text =
    Printf.sprintf
      "[meta_refine] applied changes:\n--- original\n%s\n--- refined\n%s"
      orig_msg
      refined_msg
  in
  print_endline diff_text;
  [%expect
    {|
    [meta_refine] applied changes:
    --- original
    Hello world
    --- refined
    Hello world
    Hello world
    <!-- iteration: 1 -->
    |}]
;;
