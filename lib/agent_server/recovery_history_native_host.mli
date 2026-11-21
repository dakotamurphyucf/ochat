(** Prepare the actual foreground operation's private recovery result. The
    Registry supplies the physical source borrow and submission exclusion. *)
open! Core

module P = Agent_protocol
module A = Agent_session
module Store = Agent_store.Session_store

type source =
  { with_source :
      'a.
      P.Session_ref.t
      -> authorize:(A.Session_state.t -> (unit, P.Error.t) result)
      -> f:
           (Store.Handle.t
            -> observation:A.Session_actor.Resource_observation.t
            -> current:(unit -> (A.Session_state.t, P.Error.t) result)
            -> check_owner:(unit -> (unit, P.Error.t) result)
            -> ('a, A.Recovery_source_reader.error) result)
      -> ('a, A.Recovery_source_reader.error) result
  ; custody : P.Session_ref.t -> Inference_runtime.Protected_delivery.custody
  }

val prepare
  :  Store.t
  -> blobs:Agent_store.Blob_store.t
  -> max_payload_bytes:int
  -> now:(unit -> Mtime.t)
  -> source
  -> actor:A.Session_actor.t
  -> owner:P.Session_ref.t
  -> A.Recovery_history_native_service.t
