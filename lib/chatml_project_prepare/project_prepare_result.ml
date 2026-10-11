module Build = Chatml_project_build
module Accounting = Project_prepare_accounting

type t =
  { build : Build.t
  ; accounting : Accounting.t
  }

let create build ~accounting = { build; accounting }
let build t = t.build
let accounting t = t.accounting
