(** Authenticated, principal/query/data-bound cursors. Collection changes or host
    restart invalidate old cursors explicitly. Session paging preserves requested
    order; a signed changed data binding returns Conflict/refresh_required while
    authority/query/restart mismatches are invalid/expired. No cursor retains server memory.
    Partial history windows advertise structural incompleteness and navigation
    cursors; they must not be used as model input without completing the window. *)
type t

val create : unit -> t

val lists
  :  t
  -> Agent_protocol.Principal.t
  -> Agent_protocol.Command.t
  -> Agent_protocol.Method_result.t
  -> (Agent_protocol.Method_result.t, Agent_protocol.Error.t) result

(** Catalog paging uses the exact checked organization snapshot used for projection.
    Org revision changes conservatively refresh-conflict even for unchanged entries.
    Session_list results passed to [lists] are already paged by this operation. *)
val session_catalog
  :  t
  -> Agent_protocol.Principal.t
  -> Agent_protocol.Session.List_request.t
  -> host_id:Agent_protocol.Id.Server.t
  -> organization_revision:int64
  -> Agent_protocol.Session_catalog.t list
  -> ( Agent_protocol.Session_catalog.t Agent_protocol.Page.t
       , Agent_protocol.Error.t )
       result

val history
  :  t
  -> Agent_protocol.Principal.t
  -> Agent_protocol.Session.Get_request.t
  -> Agent_protocol.Snapshot.t
  -> (Agent_protocol.Snapshot.t, Agent_protocol.Error.t) result

(** Stateless admission-ordinal pagination for private-ledger projections.
    Authentication is rerun for every query; a cursor is consistency evidence,
    never authority. Raw credentials/attributes and durable rows are not encoded. *)
module Inference : sig
  type binding

  val binding
    :  principal:Agent_protocol.Principal.t
    -> request:Agent_protocol.Inference_query.Request.t
    -> generation:int
    -> accounting_revision:int64
    -> (binding, Agent_protocol.Error.t) result

  (** None starts before ordinal1. Verifies bounded claims/signature before
      trusting them. Changed authority/query/restart expires; authenticated same
      query with changed generation/revision returns Conflict/restart_required. *)
  val after
    :  t
    -> binding
    -> Agent_protocol.Page.Cursor.t option
    -> (int64, Agent_protocol.Error.t) result

  val cursor
    :  t
    -> binding
    -> after_ordinal:int64
    -> (Agent_protocol.Page.Cursor.t, Agent_protocol.Error.t) result
end
