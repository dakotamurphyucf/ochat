open Core
module Context = Chatml_authoring_context
module Build = Chatml_project_build

module Overlay = struct
  type t =
    { identity : Context.Identity.t
    ; generation : Context.Generation.t
    ; build : Build.t
    }

  let identity t = t.identity
  let generation t = t.generation
  let build t = t.build
end

let validate_overlay inputs ~lock_id ~contract ~limits ~checkpoint =
  let identity = Context.Inputs.identity inputs in
  let generation = Context.Inputs.generation inputs in
  let base = Context.Inputs.base inputs in
  Build.Effective.recompose
    base
    ~sources:(Context.Inputs.sources inputs)
    ~lock_id
    ~contract
    ~limits
    ~checkpoint
  |> Result.map ~f:(fun build -> { Overlay.identity; generation; build })
;;
