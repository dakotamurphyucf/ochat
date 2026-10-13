open Core

type t =
  | Input_limit
  | Incompatible_contract
  | Invalid_dependency_depth
  | Invalid_cursor
  | Invalid_input of Chatml_diagnostic.t
  | Work_limit
  | Sites of Chatml.Chatml_syntax_sites.Error.t
  | Recording of Chatml_typechecker.Semantic.Error.t
  | Type_graph of Chatml_typechecker.Type_snapshot.Error.t
  | Publication_fact_limit
  | Publication_edge_limit
  | Publication_label_byte_limit
  | Accounting_overflow
[@@deriving sexp_of]
