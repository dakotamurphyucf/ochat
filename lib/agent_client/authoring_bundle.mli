(** Portable immutable-upload client over an initialized OChat connection.
    Upload-only callers need the selected [authoring.bundle.v1] feature and an
    installed capability handler, not an authoring core source context. Each
    call checks the exact initialize negotiation, advertised method, result
    variant and echoed upload/source/manifest affinity before publication.
    Server authorization is repeated on every call; IDs and this client are
    not grants. The client never automatically replays uncertain mutations. *)

module B = Agent_protocol.Authoring_bundle.Bundle

type t

(** Reads [authoring.capabilities] under the selected bundle feature. A later
    initialize, even for the same host/principal, stales this client and its
    cached handler inventory; construct another client after negotiation. *)
val create : Connection.t -> (t, Agent_protocol.Error.t) result

(** A snapshot of actual handler inventory at [create], not authorization.
    Calls reject a client after a later initialize. *)
val capabilities : t -> Agent_protocol.Authoring.Capabilities.t

val begin_upload
  :  t
  -> B.Begin.request
  -> (B.Begin.response, Agent_protocol.Error.t) result

val upload_chunk
  :  t
  -> expected_manifest:B.Manifest.t
  -> B.Chunk.request
  -> (B.Chunk.response, Agent_protocol.Error.t) result

val commit
  :  t
  -> expected_manifest:B.Manifest.t
  -> B.Commit.request
  -> (B.Commit.response, Agent_protocol.Error.t) result

(** Read-only reconciliation for a known upload ID after a lost chunk/commit
    reply; it does not retry a mutation, extend retention or infer publication.
    A lost begin reply has no upload ID: retry [begin_upload] with the exact
    same idempotency key, manifest and optional parent context. *)
val status
  :  t
  -> expected_manifest:B.Manifest.t
  -> B.Status.request
  -> (B.Status.t, Agent_protocol.Error.t) result
