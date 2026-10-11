open Core
module Spec = Chatml.Chatml_builtin_spec
module Contract = Chatml.Chatml_compiler_contract

let contract surface ~checkpoint =
  let state = Spec.TVar "state" in
  Contract.for_static_surface
    (Chatml.Chatml_static_surface.of_runtime_surface surface ~checkpoint)
    ~required_bindings:
      [ "initial_state", state
      ; ( "on_event"
        , Spec.TFun ([ Spec.context_ty; state; Spec.TVar "event" ], Spec.task_ty state) )
      ]
;;

let compile
      inventory
      ~script
      ~surface
      ~capabilities
      ~env
      ~checkpoint
      ~limits
      ~max_dependency_depth
  =
  let contract = contract surface ~checkpoint in
  let open Result.Let_syntax in
  let%bind selected =
    Captured_script.select inventory ~declaration:(Moderator script) ~contract ~checkpoint
  in
  let%bind () = Captured_script.admit_tools selected ~capabilities ~checkpoint in
  Chatml_compilation.compile_prepared_with_contract
    (Captured_script.prepared selected)
    ~contract
    ~limits
    ~max_dependency_depth
    ~env
    ()
;;
