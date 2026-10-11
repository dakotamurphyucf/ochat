open Core
module Budget = Chatml_local_capture_budget

module Declaration_details = struct
  type t = Chatml_project.Error.t

  let sexp_of_t t =
    [%sexp
      { source = (Chatml_source.id (Chatml_project.Error.source t) : Chatml_source.Id.t)
      ; details = (Chatml_project.Error.details t : Document_schema.Error.t)
      ; field_path = (Chatml_project.Error.field_path t : string list)
      }]
  ;;
end

module Catalog_details = struct
  type t = Chatml_catalog.Error.t

  let sexp_of_t t = Chatml_catalog.Error.sexp_of_problem (Chatml_catalog.Error.problem t)
end

type limit = Budget.Limit.t =
  | Directory_entries
  | Source_count
  | Source_bytes
  | Total_source_bytes
  | Path_components
  | Work_steps
[@@deriving equal, sexp_of]

type problem =
  | Declaration of Declaration_details.t
  | Filesystem of
      { failure : Chatml_local_source.Error.t
      ; member : Source.Portable_path.t option
      }
  | Catalog of Catalog_details.t
  | Limit_exceeded of
      { limit : limit
      ; maximum : int
      }
  | Accounting_overflow
[@@deriving sexp_of]

type t =
  { problem : problem
  ; declared_at : Chatml_project.Declared_at.t
  ; related : Chatml_project.Declared_at.t option
  }

let create declared_at ?related problem = { problem; declared_at; related }

let of_budget declared_at = function
  | Budget.Exceeded { limit; maximum } ->
    create declared_at (Limit_exceeded { limit; maximum })
  | Budget.Overflow -> create declared_at Accounting_overflow
;;

let problem t = t.problem
let declared_at t = t.declared_at
let related t = t.related
