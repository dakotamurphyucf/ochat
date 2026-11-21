open! Core
module P = Agent_protocol

(** Resolve the exact original message or tool-result image part from an
    admitted local source. The host must check role, item, part, and caller
    scope in [current] on each invocation. A URI in text, an external URL and
    an opaque provider envelope cannot yield image bytes. *)
val read
  :  Agent_store.Session_store.t
  -> handle:Agent_store.Session_store.Handle.t
  -> current:(unit -> (Session_state.t, P.Error.t) result)
  -> max_frame_bytes:int
  -> P.Recovery_window.Citation.t
  -> part:int
  -> max_bytes:int
  -> (P.Inline_image.t, Recovery_source_reader.error) result
