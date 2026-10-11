module Budget = Chatml_local_capture_budget

type t =
  { catalog : Chatml_catalog.t
  ; accounting : Budget.Accounting.t
  }

let create catalog ~accounting = { catalog; accounting }
let catalog t = t.catalog
let accounting t = t.accounting
