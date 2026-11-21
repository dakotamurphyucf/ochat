(** Role-scoped neutral text from one exact history occurrence. A host must
    reauthorize the original caller and the supplied selection and view on
    every [current] call. A selected developer message or tool result requires
    its own positive permission. *)
open! Core

module P = Agent_protocol

type selection =
  | Canonical of P.Recovery_window.Citation.t
  | Prepared of
      { citation : P.Recovery_window.Citation.t
      ; source_revision : int64
      }

type view = P.Recovery_history_request.view =
  | Ordinary
  | Selected_developer
  | Selected_tool_result
[@@deriving equal]

type role = P.Recovery_history_request.role =
  | User
  | Assistant
  | Developer
  | Tool_result
[@@deriving equal, sexp_of]

type t

val role : t -> role

(** Text retains its original zero-based part ordinal. Other content, provider
    metadata, invocation arguments and private result fields are never exposed. *)
val parts : t -> P.History_text.t list

(** Original neutral image slots; presence is no claim that the host retained
    an uploaded original or that the active model can accept it. *)
val image_parts : t -> int list

(** Bounds apply before decoding the neutral payload. [current] is called
    before source I/O and after projection. [max_text_bytes] lowers the 1 MiB
    aggregate across all text parts of the item. A host passes its one
    [allowance] to all reads in a call. The projection charges source work and
    all examined selected text, including the original of a replacement and
    parts that produce no match. A later transport handoff must refresh the original authority at its own write boundary. *)
val resolve
  :  ?allowance:Recovery_source_reader.Allowance.t
  -> ?max_text_bytes:int
  -> Agent_store.Session_store.t
  -> handle:Agent_store.Session_store.Handle.t
  -> current:(selection -> view -> (Session_state.t, P.Error.t) result)
  -> max_frame_bytes:int
  -> max_payload_bytes:int
  -> selection
  -> view:view
  -> (t, Recovery_source_reader.error) result

(** [None] is a successfully verified exact source whose semantic role or
    provenance does not enter this view. Physical source errors stay distinct. *)
val resolve_selected
  :  ?allowance:Recovery_source_reader.Allowance.t
  -> ?max_text_bytes:int
  -> Agent_store.Session_store.t
  -> handle:Agent_store.Session_store.Handle.t
  -> current:(selection -> view -> (Session_state.t, P.Error.t) result)
  -> max_frame_bytes:int
  -> max_payload_bytes:int
  -> selection
  -> view:view
  -> (t option, Recovery_source_reader.error) result

(** Capture the exact visibility/source proof for an already verified ordinary
    part. The caller must still hold physical source custody and check the
    original recipient at the actual disclosure boundary. *)
val check_ordinary_release
  :  Session_state.t
  -> selection
  -> checkpoint:(unit -> unit)
  -> (Session_state.t -> bool, Recovery_source_reader.error) result
