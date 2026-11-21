(** Exact admitted history part and physically owned publication. These calls
    are for a trusted host. [current] must independently reauthorize the
    original actor and current session without yielding. *)
open! Core

module P = Agent_protocol
module Ref = Agent_store.Recovery_asset_reference
module Store = Agent_store.Recovery_asset_store

type t

val create
  :  Agent_store.Blob_store.t
  -> sessions:Agent_store.Session_store.t
  -> handle:Agent_store.Session_store.Handle.t
  -> max_frame_bytes:int
  -> (t, Recovery_source_reader.error) result

type current = unit -> (Session_state.t, P.Error.t) result

(** Persist exact local bytes from a canonical inline image part. The source
    part determines content and media type. Network/opaque images are refused. *)
val publish_inline
  :  t
  -> sw:Eio.Switch.t
  -> current:current
  -> P.Recovery_window.Citation.t
  -> part:int
  -> principal:P.Id.Principal.t
  -> now:P.Timestamp.t
  -> max_bytes:int
  -> (Ref.t, Recovery_source_reader.error) result

module Attribution : sig
  (** The converter must provide the exact realized canonical row and original
      bytes it actually converted into this part. The constructor verifies the
      canonical identity/revision and selects the text part. The producer must
      retain exact parser-slot provenance. Identical rendered strings, file
      paths, URIs and provider IDs cannot establish it. *)
  type t

  val create
    :  P.Recovery_window.Citation.t
    -> part:int
    -> row:P.History.entry
    -> media_type:string
    -> original:string
    -> (t, Recovery_source_reader.error) result

  val citation : t -> P.Recovery_window.Citation.t
end

(** Publication re-reads and compares the entire selected canonical row and
    part before adopting the original bytes in the private source Store. *)
val publish_converted_file
  :  t
  -> sw:Eio.Switch.t
  -> current:current
  -> principal:P.Id.Principal.t
  -> now:P.Timestamp.t
  -> max_bytes:int
  -> Attribution.t
  -> (Ref.t, Recovery_source_reader.error) result

(** Recover a pending publication for this exact canonical part only, after
    verifying the available complete bytes against its durable intent. *)
val resume
  :  t
  -> sw:Eio.Switch.t
  -> current:current
  -> P.Recovery_window.Citation.t
  -> part:int
  -> max_bytes:int
  -> (Ref.t, Recovery_source_reader.error) result

(** Host-only progress for the exact selected slot. The recorded frame is not
    a verified result and grants no read authority. *)
val has_recorded_file_commit
  :  t
  -> current:current
  -> P.Recovery_window.Citation.t
  -> part:int
  -> (bool, Recovery_source_reader.error) result

(** Resolve the committed, physically owned exact part. The caller rechecks
    original authority again at any later protected output or provider write. *)
val read
  :  t
  -> sw:Eio.Switch.t
  -> current:current
  -> P.Recovery_window.Citation.t
  -> part:int
  -> max_bytes:int
  -> (Ref.t * string, Recovery_source_reader.error) result

(** Public ordinary semantic view. The additional nonyielding predicate binds
    an already verified body to its exact selected source/position. The host
    independently rechecks the owning session and recipient authority at
    delivery before applying it to the latest state of that same session. *)
val read_ordinary
  :  t
  -> sw:Eio.Switch.t
  -> current:current
  -> P.Recovery_window.Citation.t
  -> part:int
  -> max_bytes:int
  -> ((Ref.t * string) * (Session_state.t -> bool), Recovery_source_reader.error) result
