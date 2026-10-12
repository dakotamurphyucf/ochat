open Core
open Support
module CM = Prompt.Chat_markdown
module C = Chatml_prompt_programs
module P = Chatml_prepared_program
module S = Chatml_source
module Plan = Chat_response.Script_contract_plan
module Select = Chat_response.Retained_selection
module Input = Chatmd_parser_input

let limits = Script_tests.custody_limits
let plan = Script_tests.plan

let tool =
  sprintf
    {|<script id="lookup" language="chatml" kind="tool">%s</script>|}
    Script_tests.body
;;

let legacy =
  {|<script id="owner" language="chatml" kind="moderator">let initial_state = 0
let on_event = fun event state -> Task.pure(state)</script>|}
;;

let with_context
      ?(root_text = {|<tool name="child" agent="child.chatmd" local="true"/>|})
      child_text
      f
  =
  with_workspace (fun env _ root ->
    save root "root.chatmd" root_text;
    save root "child.chatmd" child_text;
    let definition = definition root in
    Eio.Switch.run (fun sw ->
      let owner = open_owner sw root definition in
      Owner.with_scope owner ~f:(fun scope ->
        let captured =
          capture env scope definition
          |> Result.map_error ~f:(fun error ->
            Sexp.to_string_hum (Owner.Error.sexp_of_t error))
          |> Result.ok_or_failwith
          |> Owner.Capture.value
        in
        let parent = Builder.Capture.root captured in
        let child = snd (List.hd_exn (Builder.Capture.agents captured)) in
        let input = CM.Parsed.root_input parent in
        let authored =
          Chatmd_source_bundle.create
            ~root_file:(Input.path (CM.Parsed.root_input child))
            ~sources:
              ((Input.path input, Input.authored input)
               :: Builder.Capture.sources captured)
            ()
          |> ok
        in
        f ~authored ~parent ~child;
        Ok ())
      |> ok))
;;

let for_root authored parsed =
  Chatmd_source_bundle.create
    ~root_file:(Input.path (CM.Parsed.root_input parsed))
    ~sources:(Chatmd_source_bundle.sources authored)
    ()
  |> ok
;;

let provided program =
  let graph =
    P.single_file
      ~compiler_contract:(Plan.Program.compiler_contract program)
      ~source:(CM.Declaration.script_body (Plan.Program.declaration program))
      ~checkpoint:ignore
      ()
    |> ok
  in
  Plan.Program.binding program, C.Input.Provided graph
;;

let custody ~authored ~parent ~child programs =
  C.capture
    authored
    ~parsed_contexts:[ parent; child ]
    ~root:(CM.Parsed.root_input child)
    ~programs
    ~active_programs:[]
    ~limits:(limits ())
    ~checkpoint:ignore
  |> ok
;;

let select ?(checkpoint = ignore) ?(max_work_steps = 1_000_000) planned ~authored custody =
  Select.select planned ~authored ~custody ~limits:(limits ()) ~max_work_steps ~checkpoint
;;

let error result code =
  match result with
  | Ok _ -> failwith "expected retained selection rejection"
  | Error errors ->
    assert (
      List.exists errors ~f:(fun error ->
        String.equal error.Chatmd_shell_spec.Diagnostic.code code))
;;

let selected = function
  | Select.Selected custody -> custody
  | Legacy _ -> failwith "recorded graph took legacy path"
;;

let restore planned custody =
  Chat_response.Restored_programs.restore
    planned
    ~custody
    ~limits:(limits ())
    ~checkpoint:ignore
  |> ok
;;

let%expect_test
    "inactive Provided graphs select authentic roles instead of legacy fallback"
  =
  with_context tool (fun ~authored ~parent ~child ->
    let planned = plan child in
    let program = List.hd_exn (Plan.programs planned) in
    let empty = custody ~authored ~parent ~child [] in
    (match select planned ~authored empty |> ok with
     | Legacy original ->
       assert (Option.is_none (restore planned original));
       print_endline "no records: legacy"
     | Selected _ -> assert false);
    let inactive = custody ~authored ~parent ~child [ provided program ] in
    let active = select planned ~authored inactive |> ok |> selected in
    let restored = restore planned active |> Option.value_exn in
    let actual = List.hd_exn (C.Restored.programs restored) in
    let expected =
      match snd (provided program) with
      | Provided graph -> graph
      | Selected _ -> assert false
    in
    assert (
      S.Digest.equal
        (P.fingerprint expected)
        (P.fingerprint (C.Restored.Program.prepared actual)));
    assert (
      List.equal
        (fun (left_binding, left_contract) (right_binding, right_contract) ->
           C.Binding.equal left_binding right_binding
           && S.Digest.equal left_contract right_contract)
        (C.bindings inactive)
        (C.bindings active));
    print_endline "inactive Provided: selected, exact graph restored";
    [%expect
      {|
      no records: legacy
      inactive Provided: selected, exact graph restored
      |}])
;;

let%test_unit
    "wrong roles, partial records, configured omissions and foreign Roots reject"
  =
  with_context (legacy ^ tool) (fun ~authored ~parent ~child ->
    let headless =
      plan ~moderator_surface:Chatml.Chatml_builtin_surface.moderator_surface child
    in
    let ui = plan child in
    let programs = Plan.programs headless in
    let role =
      List.find_exn programs ~f:(fun program ->
        not (String.equal (C.Binding.script_id (Plan.Program.binding program)) "lookup"))
    in
    let ui_role =
      List.find_exn (Plan.programs ui) ~f:(fun program ->
        C.Binding.equal (Plan.Program.binding program) (Plan.Program.binding role))
    in
    assert (
      not
        (S.Digest.equal
           (Plan.Program.compiler_contract role)
           (Plan.Program.compiler_contract ui_role)));
    let inactive = custody ~authored ~parent ~child (List.map programs ~f:provided) in
    error (select ui ~authored inactive) "chatml.program_missing_role";
    let partial =
      custody
        ~authored
        ~parent
        ~child
        [ provided
            (List.find_exn programs ~f:(fun program -> not (phys_equal program role)))
        ]
    in
    error (select headless ~authored partial) "chatml.program_missing_role";
    let foreign =
      C.reroot
        inactive
        ~authored:(for_root authored parent)
        ~parsed:parent
        ~programs:[]
        ~limits:(limits ())
        ~checkpoint:ignore
      |> ok
    in
    error (select headless ~authored foreign) "chatml.program_selection_invalid");
  with_context
    (sprintf
       {|<script id="lookup" language="chatml" kind="tool" project="chatml.project.json">%s</script>|}
       Script_tests.body)
    (fun ~authored ~parent ~child ->
       error
         (select (plan child) ~authored (custody ~authored ~parent ~child []))
         "chatml.program_missing_role")
;;

let%test_unit "exact registration wins while ambiguous remapped aliases reject" =
  let root_text =
    {|<tool name="child" agent="child.chatmd" local="true"/>
<import src="child.chatmd" namespace="first"/>
<import src="child.chatmd" namespace="second"/>|}
  in
  with_context ~root_text tool (fun ~authored ~parent ~child ->
    let original = plan parent in
    let current = plan child in
    let aliases = List.map (Plan.programs original) ~f:provided in
    assert (List.length aliases = 2);
    let ambiguous = custody ~authored ~parent ~child aliases in
    List.iter
      [ aliases; List.rev aliases ]
      ~f:(fun programs ->
        error
          (select current ~authored (custody ~authored ~parent ~child programs))
          "chatml.program_ambiguous_role");
    let direct = List.hd_exn (Plan.programs current) |> provided in
    let preferred = custody ~authored ~parent ~child (direct :: aliases) in
    let active = select current ~authored preferred |> ok |> selected in
    assert (
      List.length (C.Restored.programs (restore current active |> Option.value_exn)) = 1);
    assert (List.length (C.bindings active) = 3);
    let unique = custody ~authored ~parent ~child [ List.hd_exn aliases ] in
    let active = select current ~authored unique |> ok |> selected in
    assert (
      List.length (C.Restored.programs (restore current active |> Option.value_exn)) = 1);
    ignore ambiguous)
;;

let%test_unit
    "whole work boundary and each callback interruption preserve original failure"
  =
  let root_text =
    sprintf
      {|<tool name="child" agent="child.chatmd" local="true"/>
<import src="child.chatmd" namespace="%s"/>|}
      (String.make 8193 'q')
  in
  with_context ~root_text tool (fun ~authored ~parent ~child ->
    let authored = for_root authored parent in
    let current = plan parent in
    let inactive =
      C.capture
        authored
        ~parsed_contexts:[ parent; child ]
        ~root:(CM.Parsed.root_input parent)
        ~programs:(List.map (Plan.programs current) ~f:provided)
        ~active_programs:[]
        ~limits:(limits ())
        ~checkpoint:ignore
      |> ok
    in
    let calls = ref 0 in
    let checkpoint () = incr calls in
    ignore (select ~checkpoint current ~authored inactive |> ok |> selected);
    let complete = !calls in
    assert (complete > 1);
    ignore (select ~max_work_steps:complete current ~authored inactive |> ok |> selected);
    error
      (select ~max_work_steps:(complete - 1) current ~authored inactive)
      "chatml.program_selection_limit";
    let failure = Failure "original borrowed callback" in
    for stop = 1 to complete do
      let calls = ref 0 in
      let checkpoint () =
        incr calls;
        if !calls = stop then raise failure
      in
      let caught =
        try
          ignore (select ~checkpoint current ~authored inactive);
          None
        with
        | exn -> Some exn
      in
      assert (
        Option.value_map caught ~default:false ~f:(fun exn -> phys_equal exn failure))
    done;
    ignore (select current ~authored inactive |> ok |> selected))
;;
