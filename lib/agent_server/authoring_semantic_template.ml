open! Core
module Owner = Authoring_context_service_core
module Ledger = Chatml_authoring_context
module Build = Chatml_project_build
module Contract = Chatml.Chatml_compiler_contract
module Template = Authoring_checked_template
module Cache = Authoring_checked_template_cache
module Effective = Chatml_authoring_effective

let retire current ~identity =
  Owner.Current.checked_templates current
  |> Result.map ~f:(fun before -> Cache.retire before ~identity)
;;

let template_error = function
  | Template.Error.Budget_exceeded -> Owner.Error.Resource_limit
  | Invalid_selected_entry
  | Incompatible_selected_contract
  | Recomposition _
  | Compile_failure _ -> Owner.Error.Checked_template_failed
;;

let cache_error = function
  | Cache.Error.Capacity_limit | Retained_byte_limit -> Owner.Error.Resource_limit
  | Invalid_binding -> Owner.Error.Stale_context
;;

let prepare
      current
      ~identity
      ~binding
      ~selected
      ~contract
      ~requested
      ~limits
      ~env
      ~checkpoint
  =
  let open Result.Let_syntax in
  let%bind before = Owner.Current.checked_templates current in
  let retired = Cache.retire before ~identity in
  if not requested
  then Ok retired
  else (
    let%bind outcome =
      Template.prepare selected ~contract ~limits ~env ~checkpoint
      |> Result.map_error ~f:template_error
    in
    match outcome with
    | Template.Inapplicable -> Ok retired
    | Checked witness ->
      let%bind binding = binding witness |> Result.map_error ~f:cache_error in
      Cache.prepare retired ~binding |> Result.map_error ~f:cache_error)
;;

let project current ~inputs ~receipt ~contract ~requested ~limits ~env ~checkpoint =
  prepare
    current
    ~identity:(Ledger.Inputs.identity inputs)
    ~binding:(fun witness -> Cache.Binding.project ~inputs ~receipt ~witness)
    ~selected:(Effective.Overlay.build receipt)
    ~contract
    ~requested
    ~limits
    ~env
    ~checkpoint
;;

let document
      current
      ~inputs
      ~script
      ~selected
      ~contract
      ~requested
      ~limits
      ~env
      ~checkpoint
  =
  prepare
    current
    ~identity:(Ledger.Document.Inputs.identity inputs)
    ~binding:(fun witness -> Cache.Binding.document ~inputs ~script ~witness)
    ~selected
    ~contract
    ~requested
    ~limits
    ~env
    ~checkpoint
;;
