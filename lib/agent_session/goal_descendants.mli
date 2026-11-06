(** Bounded descendant-accounting DATA. Construction/decoding does not
    authenticate ancestry, canonical ACK, no-writer seals or current policy.
    State consumes the closed Update at one actual ordered edge. *)
module Profile : sig
  type t [@@deriving equal, sexp_of]

  (** Captured host bounds: max_scopes is nonnegative (zero disables inherited
      scopes); the remaining bounds are positive. max_roots <= 16; component bytes <= 256KiB;
      child bytes <= 64MiB. Complete structural limits remain depth64,
      fields16384/nodes32768. Factory supplies its actual delegation depth,
      recovery count/bytes; direct hosts supply their actual explicit profile. *)
  val create
    :  max_scopes:int
    -> max_roots:int
    -> max_branches:int
    -> max_encoded_bytes:int
    -> child_max_encoded_bytes:int
    -> (t, Agent_protocol.Error.t) result

  val max_scopes : t -> int
  val max_roots : t -> int
  val max_branches : t -> int
  val max_encoded_bytes : t -> int
  val child_max_encoded_bytes : t -> int
end

module Branch = Goal_descendant_branch

type t

val profile : t -> Profile.t
val session : t -> Agent_protocol.Session_ref.t

(** None is historical own-origin Unknown and cannot later be relabelled.
    Some is the exact positive origin captured only at actual new-session birth. *)
val origin : t -> Agent_store.Delegation_goal_origin.t option

(** Latest known Included head per inherited scope, in frozen scope order.
    Includes accepted deeper-child heads so an intermediate re-exports them.
    Unknown own origin does not manufacture an empty known scope vector. *)
val local_included : t -> Goal_descendant_included.t list

val roots : t -> Agent_protocol.Id.Goal.t list
val branches : t -> goal_id:Agent_protocol.Id.Goal.t -> Branch.t list

(** Actual unpublished birth only. Ledger identity/generation and exact known
    zero lifetime marks are validated here; State separately checks all its
    publication counters zero and real captured admission origin/profile. *)
val create_new
  :  profile:Profile.t
  -> session:Agent_protocol.Session_ref.t
  -> origin:Agent_store.Delegation_goal_origin.t
  -> ledger:Inference_ledger.t
  -> (t, Agent_protocol.Error.t) result

module Canonical : sig
  type t =
    | Attempt of Inference_ledger.Handle.t * Inference_ledger.tracking
    | Host_turn of
        Agent_protocol.Operation.t
        * Inference_ledger.Turn_handle.t
        * Inference_ledger.tracking

  module Fact : sig
    (** Checked durable DATA projection; its decoder never manufactures a Handle.
        State validates actual host association at replay, especially untracked
        admissions where the ledger retains no row or operation labels. *)
    type t

    val occurrence : t -> Goal_descendant_occurrence.t
    val tracking : t -> Inference_ledger.tracking
    val operation : t -> Agent_protocol.Operation.t option
  end
end

module Update : sig
  type accounting = t

  module Children : sig
    type t =
      | Preserve
      | Replace of Child_contributions.t
  end

  module Reason : sig
    (** Finite durable reasons; no arbitrary setter or whole-State dependency.
        Register_new_root distinguishes actual absent prior component from an
        existing one through the immutable original basis below. *)
    type t =
      | Register_new_root of
          { host_profile : Profile.t
          ; session : Agent_protocol.Session_ref.t
          ; goal_id : Agent_protocol.Id.Goal.t
          }
      | Register_child of
          { reference : Agent_store.Delegation_store.Reference.t
          ; origin : Agent_store.Delegation_goal_origin.t
          }
      | Register_branch of
          { scope : Agent_store.Delegation_goal_origin.Scope.t
          ; reference : Agent_store.Delegation_store.Reference.t
          ; origin : Agent_store.Delegation_goal_origin.t
          }
      | Reserve of
          { scope : Agent_store.Delegation_goal_origin.Scope.t
          ; occurrence : Goal_descendant_occurrence.t
          ; local_count : int64
          ; ceiling : int64 option
          }
      | Record_included of
          { fact : Canonical.Fact.t
          ; receipts : Goal_descendant_included.t list
          }
      | Import of Agent_store.Delegation_store.Reference.t
      | Close_before_writer of Goal_descendant_pending.t
  end

  type t

  val accounting : t -> accounting
  val children : t -> Children.t
  val reason : t -> Reason.t
  val shape : Document_schema.Shape.t
  val to_json : t -> (Jsonaf.t, Agent_protocol.Error.t) result
  val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result
end

(** Called at the actual public/native Create edge, before its first episode.
    [account] must equal the actual Child.create bootstrap against transaction-
    original [before_ledger]. State additionally proves this Goal/account came
    from its actual ordered catalog/account creation, retaining that evidence
    through same-Batch edits, start or closure. No fresh Run is needed.

    If previous=None, initializes own origin=None, no known inherited heads,
    and this genuinely new local root. Existing children are preserved exactly;
    absent children become Historical_children_unknown using captured profile.
    New profile branch/child-byte bounds are capped by existing child profile;
    stored child data/profile are never retroactively lowered or enlarged.
    If previous=Some, its profile/session/origin and all prior roots are frozen.
    An existing same root cannot be reset. No delegated own usage is created. *)
val register_new_root
  :  t option
  -> host_profile:Profile.t
  -> session:Agent_protocol.Session_ref.t
  -> children:Child_contributions.t option
  -> goal:Agent_protocol.Goal.t
  -> account:Goal_accounting_document.t
  -> before_ledger:Inference_ledger.t
  -> (Update.t, Agent_protocol.Error.t) result

val register_child
  :  t
  -> children:Child_contributions.t
  -> reference:Agent_store.Delegation_store.Reference.t
  -> origin:Agent_store.Delegation_goal_origin.t
  -> (Update.t, Agent_protocol.Error.t) result

(** Scope owner/Goal identifies an existing local root. Its real creator episode
    is available only now, not fabricated at held Goal creation. Exact branch
    replay is idempotent; changed frozen attribution conflicts. SAME child slot
    origin contains this exact scope and is registered in the same Update. Any
    other local-owner roots in that same frozen origin receive their frontiers
    atomically; all such local roots must already exist. Finite replay requires
    each new local scope's creator episode/source to match the actual current
    catalog Goal. The runtime admission owner must commit the parent frontier before child effects;
    recovery reuses its retained frontier after episode/source changes. *)
val register_branch
  :  t
  -> children:Child_contributions.t
  -> scope:Agent_store.Delegation_goal_origin.Scope.t
  -> reference:Agent_store.Delegation_store.Reference.t
  -> origin:Agent_store.Delegation_goal_origin.t
  -> (Update.t, Agent_protocol.Error.t) result

module Availability : sig
  type t =
    | Available
    | Reserved
    | Exhausted
    | Unknown
  [@@deriving equal, sexp_of]
end

(** Count policy only. None ceiling is Available despite labelled Unknown.
    Finite known committed lower bound at ceiling is Exhausted; pending occupied
    capacity yields Reserved; otherwise unresolved associated coverage is Unknown.
    Actual root/account evidence must exist even for unlimited policy.
    State validates local_count/ceiling against its actual Goal/account when a
    Reserve update is consumed. Token/deadline/control/original policy is separate. *)
val available
  :  t
  -> children:Child_contributions.t
  -> goal_id:Agent_protocol.Id.Goal.t
  -> local_count:int64
  -> ceiling:int64 option
  -> dimension:Goal_admission_policy.dimension
  -> (Availability.t, Agent_protocol.Error.t) result

val accounting_coverage
  :  t
  -> children:Child_contributions.t
  -> goal_id:Agent_protocol.Id.Goal.t
  -> (Child_usage_snapshot.Coverage.t, Agent_protocol.Error.t) result

module Reserve_result : sig
  type t =
    | Reserved of Update.t * Goal_descendant_pending.t
    | Already_pending of Goal_descendant_pending.t
    | Blocked of Availability.t
end

(** Same-branch serialization precedes count policy: exact pending occurrence
    replay is unchanged; another occurrence is Blocked Reserved even unlimited.
    New nonce/prefix allocated by the actual existing bounded Branch module. *)
val reserve
  :  t
  -> children:Child_contributions.t
  -> scope:Agent_store.Delegation_goal_origin.Scope.t
  -> occurrence:Goal_descendant_occurrence.t
  -> local_count:int64
  -> ceiling:int64 option
  -> (Reserve_result.t, Agent_protocol.Error.t) result

(** Validates genuine actual canonical Handle/Turn_handle against one SAME
    before/after edge, then records exactly all positively inherited scopes with
    one actual leaf occurrence. Root pending remains untouched until child import.
    Historical origin=None retains Unknown and accepts no invented inherited
    receipts. State ties the local occurrence to its transaction-original +1
    session revision; this operation does not compare remote parent revisions. *)
val record_included
  :  t
  -> children:Child_contributions.t
  -> before:Inference_ledger.t
  -> after:Inference_ledger.t
  -> canonical:Canonical.t
  -> occurrence:Goal_descendant_occurrence.t
  -> receipts:Goal_descendant_included.t list
  -> (Update.t, Agent_protocol.Error.t) result

(** SAME553 replacement and paired root confirmation. Only Added/Replaced can
    retire exact pending; duplicate/stale leave accounting and child bytes
    unchanged. Confirmation requires same frozen scope/receipt and exact +1
    known dimension subtotal while other dimension is unchanged, even Unknown
    coverage. Same-prefix corrections change no prefix/pending/count. Deeper
    heads are re-exported only within the immutable inherited scope vector. *)
val import
  :  t
  -> children:Child_contributions.t
  -> snapshot:Child_usage_snapshot.t
  -> (Update.t, Agent_protocol.Error.t) result

(** DATA exact relation only. The runtime admission owner supplies the actual same preparation seal.
    Keeps count/prefix and consumes nonce. Exact latest terminal replay never
    clears a newer pending reservation. *)
val close_before_writer
  :  t
  -> children:Child_contributions.t
  -> pending:Goal_descendant_pending.t
  -> (Update.t, Agent_protocol.Error.t) result

(** Replays the finite reason from State's actual original basis and requires its
    complete accounting/paired child candidate, preserving all unknowns.
    Projections stay below Session_state to avoid a dependency cycle.

    [creation] is supplied only at Register_new_root from the actual captured
    ordered Create Goal/account; [catalog]/[accounts] are actual root policy
    projections for Reserve, and [catalog] supplies the actual current creator
    episode/source for each genuinely new local branch. Local Record_included checks SAME canonical edge
    and original before/after session revisions (+1); parent Reserve/Import use
    remote coordinates and are never compared to those local revisions.
    State separately checks actual host operation/invocation associations for
    decoded untracked Fact DATA and all catalog/account/source invariants. *)
val validate_transition
  :  t option
  -> update:Update.t
  -> before_ledger:Inference_ledger.t
  -> after_ledger:Inference_ledger.t
  -> before_session_revision:int64
  -> after_session_revision:int64
  -> before_children:Child_contributions.t option
  -> creation:(Agent_protocol.Goal.t * Goal_accounting_document.t) option
  -> catalog:Goal_catalog.t option
  -> accounts:Goal_accounts.t option
  -> (unit, Agent_protocol.Error.t) result

(** Complete State consistency from projections only. Roots require actual local
    Goal/account identities; branch references require the exact existing child
    slot and frozen origin scope. Captured profile cannot exceed actual stored
    child profile. Positive inherited origin requires an actual own delegation;
    historical None remains Unknown. Positive known inherited usage requires a
    head whose branch prefix bounds that subtree count; a first child's head
    references its exact own delegation. Ledger/own delegation session identities
    and known root subtotal/frontier conservation are checked, no authority.
    These are completed State checks, not partial-delta admission. *)
val validate_context
  :  t
  -> session_id:Agent_protocol.Id.Session.t
  -> ledger:Inference_ledger.t
  -> children:Child_contributions.t option
  -> catalog:Goal_catalog.t option
  -> accounts:Goal_accounts.t option
  -> delegation:Agent_store.Delegation_store.Reference.t option
  -> (unit, Agent_protocol.Error.t) result

val of_document : Document_schema.Document.t -> (t, Agent_protocol.Error.t) result
val to_document : t -> (Document_schema.Document.t, Agent_protocol.Error.t) result

(** Merge preservation only between equal known projections, including all
    frontiers. Known state movement requires the closed Update relation above. *)
val adopt : t -> incoming:t -> (t, Agent_protocol.Error.t) result

(** Existing-slot future growth only; known future pending/outcome/receipt
    alternatives retain actual frozen scope/reference and compatible unknowns.
    Component growth is counted once; child snapshot growth remains SAME553's
    separate reserve. Actual full State nesting-depth admission belongs to State. *)
val reserved_growth : t -> (Capacity_growth.t, Agent_protocol.Error.t) result

val limits : Document_schema.Limits.t

(** Maximum root-relative full component-document JSON depth of current data and
    existing-slot future witnesses, including compatible local alternatives and
    preserved unknowns. Root is depth1, scalar leaves included. Intrinsic depth64
    is enforced here; an enclosing State must add its actual JSON path length
    and compare to its actual configured max_depth. No additive quota changes. *)
val required_depth : t -> (int, Agent_protocol.Error.t) result
