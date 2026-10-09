(** Host-private, disposable LRU of readable canonical projections. One host owns
    this value; operations do not yield. Keys are observations, never authority.
    The service must recheck current visibility and content before disclosure.
    No raw payload, durable plaintext copy or execution resource is retained. *)
type t

module Key : sig
  type t

  val create
    :  scope_identity:string
    -> session:Agent_protocol.Session_ref.t
    -> generation:int
    -> session_revision:int64
    -> canonical_index:int
    -> (t, Agent_protocol.Error.t) result
end

(** Explicit finite budgets: 1 byte–64 MiB accounted bytes, 1–4096 entries and a
    positive per-session limit no larger than the entry limit. Defaults are
    8 MiB, 256 entries and 32 per session. Memory accounting includes key strings,
    safe selected text and conservative bookkeeping, not an exact heap census. *)
val create
  :  ?max_bytes:int
  -> ?max_entries:int
  -> ?max_session_entries:int
  -> unit
  -> (t, Agent_protocol.Error.t) result

(** A hit may contain None: the canonical entry was checked and excluded.
    A miss is distinct. Reads update recency and take O(max_entries) work. *)
val find : t -> Key.t -> [ `Miss | `Hit of Search_entry.t option ]

(** Replaces an exact key and evicts older revisions for that session/scope.
    Oversized candidates are simply not retained. The caller's canonical read
    remains successful. Oldest entries are evicted to satisfy every budget. *)
val add : t -> Key.t -> Search_entry.t option -> unit

val invalidate_session : t -> Agent_protocol.Session_ref.t -> unit
val clear : t -> unit

module Stats : sig
  type t = private
    { entries : int
    ; accounted_bytes : int
    }
  [@@deriving sexp_of]
end

val stats : t -> Stats.t
