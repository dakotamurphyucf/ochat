open Core
open Expect_test_helpers_core

let%expect_test "extract Revised_Prompt section" =
  let sample =
    "Overview\n\
     ...\n\n\
     Issues_Found\n\
     ...\n\n\
     Minimal_Edit_List\n\
     ...\n\n\
     Revised_Prompt\n\
     THIS IS THE REVISED PROMPT\n\
     WITH MULTIPLE LINES\n\n\
     Optional_Toggles\n\
     ...\n\
     API_Parameter_Suggestions\n\
     ...\n"
  in
  let out =
    Meta_prompting.Prompt_factory_online.extract_section
      ~text:sample
      ~section:"Revised_Prompt\n"
  in
  (match out with
   | None -> print_endline "none"
   | Some s -> print_endline s);
  [%expect
    {|
    THIS IS THE REVISED PROMPT
    WITH MULTIPLE LINES
    |}]
;;
