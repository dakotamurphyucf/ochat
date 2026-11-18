(** Exact maximum JSON depth, separate from additive byte/field/node growth. *)

(** Validates the complete raw input before bounded traversal. The root is depth
    one and each object member/array element adds one, including scalar leaves,
    exactly as in [Document_schema.Json.validate]. *)
val measure
  :  Jsonaf.t
  -> limits:Document_schema.Limits.t
  -> (int, Agent_protocol.Error.t) result

(** [ancestors] counts concrete enclosing JSON path edges, not spare capacity.
    Rejects negative/invalid coordinates and depth above the actual owner bound.
    Does not charge additive quotas or increase any supplied limit. *)
val validate
  :  int
  -> ancestors:int
  -> limits:Document_schema.Limits.t
  -> (unit, Agent_protocol.Error.t) result
