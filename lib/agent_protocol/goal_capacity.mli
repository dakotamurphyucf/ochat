(** Storage reservation only, never a hypothetical domain state or proof. All
    measurements include compact JSON escaping. Bounds are finite engineering
    limits independent of provider billing or execution policy. *)
type t

val max_projection_bytes : int
val max_document_bytes : int
val encoded_bytes : t -> int
val fields : t -> int
val nodes : t -> int
val measure : Jsonaf.t -> (t, Error.t) result

(** Upper bound for any goal projection retaining this request, including maximum
    bounded mutable text, budget/source/revisions and current/predecessor terminal
    receipt slots. The request is retained exactly once in the Goal wire format.
    Reject before staging its Run Finish if this exceeds the projection limit. *)
val for_request : Goal_achievement.Request.t -> (t, Error.t) result
