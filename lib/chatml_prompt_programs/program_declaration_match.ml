open Core
module Binding = Program_binding
module Inventory = Program_inventory
module CM = Prompt.Chat_markdown

type t =
  { original : Binding.t
  ; current : Binding.t
  ; compiler_contract : Chatml_source.Digest.t
  ; declaration : CM.Declaration.t
  }

let original t = t.original
let current t = t.current
let compiler_contract t = t.compiler_contract
let declaration t = t.declaration

let count values maximum ~checkpoint =
  List.fold_result values ~init:0 ~f:(fun count _ ->
    checkpoint ();
    if count >= maximum then Error Inventory.Error.Limit_exceeded else Ok (count + 1))
;;

let fold inventory ~parsed ~limits ~checkpoint ~init ~f =
  let open Result.Let_syntax in
  let maximum = Inventory.Limits.max_programs limits in
  let%bind original_count = count inventory.Inventory.programs maximum ~checkpoint in
  if original_count = 0
  then (
    checkpoint ();
    Ok init)
  else (
    let declarations = CM.Parsed.declarations parsed in
    let%bind current_count = count declarations Int.max_value ~checkpoint in
    if current_count > 0 && original_count > Int.max_value / current_count
    then Error Inventory.Error.Limit_exceeded
    else
      List.fold_result declarations ~init ~f:(fun acc declaration ->
        checkpoint ();
        let%bind current =
          Binding.of_declaration parsed declaration ~checkpoint
          |> Result.map_error ~f:(fun _ ->
            Inventory.Error.Invalid_binding "Foreign current declaration")
        in
        List.fold_result inventory.programs ~init:acc ~f:(fun acc program ->
          checkpoint ();
          if not (Binding.same_declaration program.Inventory.Program.binding current)
          then Ok acc
          else
            Ok
              (f
                 acc
                 { original = program.binding
                 ; current
                 ; compiler_contract = program.compiler_contract
                 ; declaration
                 }))))
;;
