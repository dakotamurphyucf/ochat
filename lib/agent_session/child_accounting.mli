(** Pure projection helpers consumed by actual Factory birth and Actor writers.
    They grant no child visibility, Goal ownership or execution permission. *)

(** Only the actual new-session Factory owner calls this before the first Store
    publication. Historical missing fields must not use this path. The supplied
    bounds are the validated Factory delegation profile. *)
val initialize_new
  :  Session_state.t
  -> max_children:int
  -> max_encoded_bytes:int
  -> (Session_state.t, Agent_protocol.Error.t) result

(** Read only ACKed native State. None is historical missing own accounting,
    never exact zero. Missing historical child inventory is carried as unknown;
    no mutable external delegation scan enters this version or digest. *)
val snapshot
  :  Session_state.t
  -> (Child_usage_snapshot.t option, Agent_protocol.Error.t) result

(** Build the exact next slot document; the Actor separately authenticates the
    real parent/child/reference before publication. Historical absence initializes
    only Historical_children_unknown. Existing stored bounds stay unchanged. *)
val register
  :  Session_state.t
  -> reference:Agent_store.Delegation_store.Reference.t
  -> max_children:int
  -> max_encoded_bytes:int
  -> (Child_contributions.t, Agent_protocol.Error.t) result
