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

val sexp_of_t : t -> Sexp.t
