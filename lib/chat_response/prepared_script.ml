open Core
module Contract = Chatml.Chatml_compiler_contract

type t =
  { selected : Captured_script.t
  ; target : Chatml_compilation.target
  }

let select inventory ~script ~target ~capabilities ~checkpoint =
  let contract_target : Contract.Target.t =
    match target with
    | Chatml_compilation.One_off_v1 -> One_off_v1
    | Tool_v1 -> Tool_v1
    | Developer_tool_v1 -> Developer_tool_v1
    | Moderator_v1 -> Moderator_v1
    | Delegated_moderator_v1 -> Delegated_moderator_v1
  in
  let contract = Contract.for_target contract_target ~checkpoint in
  let open Result.Let_syntax in
  let%bind selected =
    Captured_script.select inventory ~declaration:(Extension script) ~contract ~checkpoint
  in
  let%map () = Captured_script.admit_tools selected ~capabilities ~checkpoint in
  { selected; target }
;;

let fingerprint t = Captured_script.fingerprint t.selected
let input_bytes t = Captured_script.input_bytes t.selected
let prepared t = Captured_script.prepared t.selected

let compile t ~env ~limits ~max_dependency_depth =
  Chatml_compilation.compile_prepared
    (Captured_script.prepared t.selected)
    ~target:t.target
    ~limits
    ~max_dependency_depth
    ~env
    ()
  |> Result.map ~f:(fun program -> Script_program.Prepared program)
;;
