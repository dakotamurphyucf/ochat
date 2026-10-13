open Core

type t =
  | Unsupported_kind
  | Evidence_mismatch
  | Stale_source
  | Source_unavailable
  | Foreign_symbol
  | Invalid_site_projection
  | Work_limit
  | Label_byte_limit
  | Member_edge_limit
  | Projection_limit
  | Query of Chatml_semantics.Snapshot.Error.t
  | Display of Chatml_type_display.Error.t
  | Wire of Agent_protocol.Error.t

let sexp_of_t = function
  | Unsupported_kind -> Sexp.Atom "unsupported_kind"
  | Evidence_mismatch -> Sexp.Atom "evidence_mismatch"
  | Stale_source -> Sexp.Atom "stale_source"
  | Source_unavailable -> Sexp.Atom "source_unavailable"
  | Foreign_symbol -> Sexp.Atom "foreign_symbol"
  | Invalid_site_projection -> Sexp.Atom "invalid_site_projection"
  | Work_limit -> Sexp.Atom "work_limit"
  | Label_byte_limit -> Sexp.Atom "label_byte_limit"
  | Member_edge_limit -> Sexp.Atom "member_edge_limit"
  | Projection_limit -> Sexp.Atom "projection_limit"
  | Query error ->
    Sexp.List [ Sexp.Atom "query"; Chatml_semantics.Snapshot.Error.sexp_of_t error ]
  | Display error ->
    Sexp.List [ Sexp.Atom "display"; Chatml_type_display.Error.sexp_of_t error ]
  | Wire _ -> Sexp.Atom "wire"
;;
