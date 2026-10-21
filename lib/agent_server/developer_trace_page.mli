open! Core
module P = Agent_protocol

(** Pure bounded projection of one already authenticated immutable recorder
    snapshot. This module owns no Entry, source policy, read loan or transport.
    The Host has already checked the exact physical association and historical
    source-read authority through [Developer_host.Trace.snapshot]. A cursor is
    checked only as comparison data after that read. *)

(** Map all native Header/event/terminal variants exhaustively, retaining
    exact source identity/digest/native byte span, sequence, kind and safe
    correlation. Unknown or unrepresentable native data fails with a bounded
    typed error; it is never silently dropped or mapped to an invented site.
    The Host's prior site filtering is preserved. *)
val project
  :  limits:P.Developer_trace_wire.Limits.t
  -> request:P.Developer_trace_wire.Page_request.t
  -> availability:Developer_trace_retention.Availability.t
  -> (P.Developer_trace_wire.Availability.t, P.Error.t) result

(** [project] checks locator, host-minted Execution_id, Header submission and
    selection fingerprints and last returned sequence against this exact
    snapshot. A cursor from another action, reset/restart/reused key, evicted
    Entry or different Header yields Lease_stale after normal Host disclosure.
    An invalid cursor cannot create a new position or bypass a current read.

    It chooses a nonempty advancing prefix whenever retained events remain,
    validating the COMPLETE [Response.Page] JSON under the installed page and
    response Document_schema limits, including the repeated Header, cursor,
    array/framing, and max item count. If not even one remaining event fits,
    it returns Resource_limit; it never repeats an empty next cursor. When no
    event remains, the empty current snapshot has no next; its resume repeats
    the request cursor so a later Incomplete poll starts after the last
    delivered event, or remains absent for an initial empty page.
    Every call is bounded by the immutable recorder prefix and policy limits. *)
