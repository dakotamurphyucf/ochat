open Core

type t =
  { name : string
  ; site : Chatml_syntax_sites.Id.t option
  }

let unlocated name = { name; site = None }
let located name site = { name; site = Some site }
let name t = t.name
let site t = t.site
let compare left right = String.compare left.name right.name
let equal left right = String.equal left.name right.name
let sexp_of_t t = String.sexp_of_t t.name
let t_of_sexp sexp = unlocated (String.t_of_sexp sexp)
