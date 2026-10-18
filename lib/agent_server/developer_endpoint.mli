open! Core
module P = Agent_protocol

(** One installed authenticated Developer command/approval endpoint. It
    projects bounded protocol DTOs from the exact existing Developer_host and
    borrows the existing Authoring source owner only on a genuine historical
    key miss. It owns no command row, Root, scheduler, source resolver or
    approval broker. *)
type t

module Policy : sig
  type t

  (** Explicit opt-in for a portable endpoint. A Host-only Developer
      configuration installs no wire handlers. Bounds are positive and finite;
      every watched key has a bounded dirty/resync slot independent of hint
      queue capacity. Daemon composition subsequently checks these ceilings against the
      actual Host, approval owner and Initialize request/response limits. *)
  val create
    :  wire_limits:P.Developer_common.Limits.t
    -> max_connections:int
    -> max_watches_per_connection:int
    -> max_pending_hints_per_connection:int
    -> (t, P.Error.t) result

  val wire_limits : t -> P.Developer_common.Limits.t
  val change_limits : t -> Developer_change_hub.Limits.t
end

(** [wire_limits] are the effective installed ceilings, admitted against the
    daemon's actual Host/approval/Initialize bounds before construction.
    Absent Host means no endpoint or Developer method decoder. Construction
    verifies the exact Host/Authoring physical pairing. [changes] is the same
    hub whose nonyielding enqueue was passed to Host creation. *)
val create
  :  host:Developer_host.t
  -> authoring:Authoring_installation.t
  -> changes:Developer_change_hub.t
  -> policy:Policy.t
  -> (t, P.Error.t) result

val limits : t -> P.Developer_common.Limits.t

(** Actual installed families, filtered again by the physical connection's
    current negotiated features. Codec-supported methods alone are never
    advertised. Approval methods require a physically configured Host owner;
    Watch requires the live bounded hub. *)
val available_features : t -> string list

val available_methods : t -> connection:Connection_context.t -> string list

(** Authenticate through Connection_context.request_actor before any Host
    operation and after yielding source, Store or approval steps in this
    adapter. Submit additionally lends a nonyielding exact request-lease
    check into Host's serialized fresh-key preparation and Reserve admission.
    The successfully installed durable Reserve is the request handoff:
    subsequent daemon-owned execution retains its original actor and source
    checks, and a disconnected client cannot cancel it. A final response
    check may withhold publication after disconnect without undoing that work.
    Submit
    builds one canonical Original from the decoded DTO, then lends a
    [resolve_fresh] callback to Host. Host invokes it only inside the
    authoritative serialized historical-key miss; identical old-key retries
    never inspect a root, Expected editor context or uploaded source. Local
    indices select actual opaque Roots from this installed Authoring owner;
    editor routes use the exact current connection and Expected. Changed
    action/payload rejects by Host's canonical fingerprint before resolution.
    No generic Command_receipt/Task-row path handles this family. A successful
    Watch acknowledges future hints only; the client follows it with Observe
    of its exact key for the authoritative command baseline. Approval_list is
    optional and independently authorized; Check-only watches need no
    approval scope. The typed
    local path first roundtrips Request.method_name/params through
    Request.of_method_and_params under these installed limits before any
    Host/source work; a value constructed under broader limits is rejected
    just like the transport envelope. *)
val handle
  :  t
  -> ?actor:Operator_authorization.t
  -> connection:Connection_context.t
  -> P.Developer_wire.Request.t
  -> (P.Developer_wire.Response.t, P.Error.t) result

(** Current response projection validates the complete inner receipt/page and
    outer wire response under [limits] before returning. It preserves native
    versus claimed-Model approval origin and independently applies current
    read versus response/control policy. A decoded ID or pending DTO never
    supplies authority. *)
val encode_response : t -> P.Developer_wire.Response.t -> (Jsonaf.t, P.Error.t) result

val close_connection_and_wait : t -> Connection_context.t -> (unit, P.Error.t) result
val close_and_wait : t -> (unit, P.Error.t) result
