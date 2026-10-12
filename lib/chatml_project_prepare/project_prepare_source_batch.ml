open Core

type t =
  { preparations : Project_prepare_source_result.t list
  ; accounting : Project_prepare_accounting.t
  }

let create preparations ~accounting = { preparations; accounting }
let preparations t = t.preparations
let accounting t = t.accounting
