module Limits = Chatml_local_capture_budget.Limits
module Accounting = Chatml_local_capture_budget.Accounting
module Error = Chatml_local_capture_error
module Binding = Chatml_catalog.Binding

type t = Chatml_local_capture_catalog.t

let capture = Chatml_local_capture_acquire.capture
let capture_from_owner = Chatml_local_capture_acquire.capture_from_owner
let catalog = Chatml_local_capture_catalog.catalog
let accounting = Chatml_local_capture_catalog.accounting
let project t = Chatml_catalog.project (catalog t)
let selection t = Chatml_catalog.selection (catalog t)
let sources t = Chatml_catalog.sources (catalog t)
let entry t = Chatml_catalog.entry (catalog t)
let definitions t = Chatml_catalog.definitions (catalog t)
let bindings t = Chatml_catalog.bindings (catalog t)
let find_source t = Chatml_catalog.find_source (catalog t)
let scope_for t = Chatml_catalog.scope_for (catalog t)
