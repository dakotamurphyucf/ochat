open Core
module Build = Chatml_project_build

type t =
  { build : Build.t
  ; declared_at : Chatml_project.Declared_at.t
  }

let create build ~declared_at = { build; declared_at }
let build t = t.build
let required_tools t = Chatml_project.Lock.required_tools (Build.lock t.build)
let declared_at t = t.declared_at
