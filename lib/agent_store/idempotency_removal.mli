(** Private fixed-width slot in a protected lifecycle receipt. Published data
    must also pass the caller's exact typed key/retention/outcome checks. *)
type t =
  | Reserved
  | Applied of int
[@@deriving sexp_of]

val field : string
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Document_schema.Error.t) result

module Evidence : sig
  type t = private
    { session_id : Agent_protocol.Id.Session.t
    ; generation : int
    }

  val valid_result
    :  session_id:Agent_protocol.Id.Session.t
    -> generation:int
    -> Agent_protocol.Method_result.Delete.t
    -> bool

  val of_delete_json
    :  session_id:Agent_protocol.Id.Session.t
    -> generation:int
    -> Jsonaf.t
    -> (t, Store_error.t) result
end

module Index : sig
  (** Derived metadata. The owner installs it only after a successful durable
      save or validated reload. Lookup does not scan the protected receipts. *)
  type t

  val empty : t
  val add : t -> Evidence.t -> t
  val find : t -> session_id:Agent_protocol.Id.Session.t -> Evidence.t option
end
