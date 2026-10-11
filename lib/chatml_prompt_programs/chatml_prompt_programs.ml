open Core

let iter_result values ~f = List.fold_result values ~init:() ~f:(fun () value -> f value)

module Captured = Chatml_source
module Prepared = Chatml_prepared_program
module Build = Chatml_project_build
module Contract = Chatml.Chatml_compiler_contract
module Storage = Agent_store.Prompt_program_bundle
module Parser_input = Chatmd_parser_input
module Binding = Program_binding
module Inventory = Program_inventory
module Limits = Inventory.Limits
module Input = Inventory.Input
module Error = Inventory.Error
module Restored = Program_restore

type t = Program_document.t

let to_storage = Program_document.to_storage
let of_storage = Program_document.of_storage
let adopt = Program_document.adopt
let parser_inputs t = (Program_document.inventory t).inputs
let root_input t = (Program_document.inventory t).root

let bindings t =
  List.map (Program_document.inventory t).programs ~f:(fun p ->
    p.binding, p.compiler_contract)
;;

let restore t = Program_restore.restore (Program_document.inventory t)

let capture authored ~parsed_contexts ~root ~programs ~active_programs ~limits ~checkpoint
  =
  let open Result.Let_syntax in
  let%bind inventory =
    Inventory.capture
      authored
      ~parsed_contexts
      ~root
      ~programs
      ~active_programs
      ~limits
      ~checkpoint
  in
  Program_document.create inventory ~authored ~limits ~checkpoint
;;

let reroot t ~authored ~parsed ~programs ~limits ~checkpoint =
  let original = Program_document.inventory t in
  let open Result.Let_syntax in
  let%bind () = Inventory.count programs (Limits.max_programs limits) ~checkpoint in
  let%bind () =
    iter_result programs ~f:(fun (_, _, current) ->
      if Binding.matches_parsed current parsed ~checkpoint
      then Ok ()
      else Error (Error.Invalid_binding "Child binding is not an actual declaration"))
  in
  let selections =
    List.map programs ~f:(fun (original, contract, binding) ->
      Inventory.Selection.{ program = Inventory.Program.key original contract; binding })
  in
  let inventory =
    { original with root = Prompt.Chat_markdown.Parsed.root_input parsed; selections }
  in
  Program_document.replace t inventory ~authored ~limits ~checkpoint
;;

let extend t ~authored ~parsed_contexts ~programs ~limits ~checkpoint =
  let original = Program_document.inventory t in
  let open Result.Let_syntax in
  let%bind added =
    Inventory.capture
      authored
      ~parsed_contexts
      ~root:original.root
      ~programs
      ~active_programs:[]
      ~limits
      ~checkpoint
  in
  let inventory =
    { original with
      programs = original.programs @ added.programs
    ; inputs = original.inputs @ added.inputs
    ; sources = original.sources @ added.sources
    }
  in
  (* Equal existing inventory associations are shared, not admitted duplicates. *)
  let%bind sources = Inventory.admit_sources inventory.sources ~limits ~checkpoint in
  let%bind inputs = Inventory.admit_inputs inventory.inputs ~limits ~checkpoint in
  Program_document.replace
    t
    { inventory with sources; inputs }
    ~authored
    ~limits
    ~checkpoint
;;

let validate_selection t ~parsed ~limits ~checkpoint =
  let inventory = Program_document.inventory t in
  let open Result.Let_syntax in
  let%bind () =
    Inventory.count inventory.selections (Limits.max_programs limits) ~checkpoint
  in
  if
    not
      (Inventory.input_equal_exact
         inventory.root
         (Prompt.Chat_markdown.Parsed.root_input parsed)
         ~checkpoint)
  then Error (Error.Invalid_binding "Replayed current Root differs")
  else
    iter_result inventory.selections ~f:(fun selection ->
      if Binding.matches_parsed selection.Inventory.Selection.binding parsed ~checkpoint
      then Ok ()
      else
        Error
          (Error.Invalid_binding "Active binding is not an actual replayed declaration"))
;;

let validate_capture t ~parsed_contexts ~root ~limits ~checkpoint =
  let inventory = Program_document.inventory t in
  let open Result.Let_syntax in
  let%bind () =
    Inventory.count parsed_contexts (Limits.max_parser_inputs limits) ~checkpoint
  in
  let%bind inputs =
    List.fold_result parsed_contexts ~init:[] ~f:(fun inputs parsed ->
      Inventory.admit_inputs
        (inputs @ Prompt.Chat_markdown.Parsed.inputs parsed)
        ~limits
        ~checkpoint)
  in
  let%bind () =
    if
      (not (Inventory.input_equal_exact inventory.root root ~checkpoint))
      || List.length inputs <> List.length inventory.inputs
      || not
           (List.for_all inputs ~f:(fun input ->
              checkpoint ();
              List.exists inventory.inputs ~f:(fun actual ->
                checkpoint ();
                Inventory.input_equal_exact input actual ~checkpoint)))
    then Error (Error.Invalid_inventory "Custody differs from actual parser capture")
    else Ok ()
  in
  let%bind () =
    iter_result inventory.programs ~f:(fun program ->
      let prepared_binding = program.Inventory.Program.binding in
      if
        List.exists parsed_contexts ~f:(fun parsed ->
          Binding.matches_parsed prepared_binding parsed ~checkpoint)
      then Ok ()
      else Error (Error.Invalid_binding "Custody binding is not in actual capture"))
  in
  let%bind parsed =
    List.find parsed_contexts ~f:(fun parsed ->
      Inventory.input_equal_exact
        root
        (Prompt.Chat_markdown.Parsed.root_input parsed)
        ~checkpoint)
    |> Result.of_option
         ~error:(Error.Invalid_binding "Current Root has no actual parser context")
  in
  validate_selection t ~parsed ~limits ~checkpoint
;;
