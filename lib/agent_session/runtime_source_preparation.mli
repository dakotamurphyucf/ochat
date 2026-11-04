open! Core

(** Immutable captured source selection before native/resource construction.
    This is data custody, never an initialized runtime, live tool ABI, invocation
    capability or permission to execute. *)
type t

module Moderator : sig
  (** Legacy identity remains distinct from the versioned invocation contract.
      No_moderator is explicit; it cannot authorize an authored Run startup. *)
  type t =
    | No_moderator
    | Legacy of Agent_protocol.Invocation.observer
    | Extension of Agent_protocol.Invocation.observer
  [@@deriving equal, sexp_of]
end

(** Validate the actual revision's retained parser, invocation role and complete
    selected program custody under the existing bounded startup policy. Source
    identity uses the same single-file/linked-program recipe as the live Registry.
    No original source lookup, native/MCP setup, tool grant, Manager construction,
    initial content conversion, history allocation or initializer occurs.

    The caller owns source admission and artifact/tree verification. This does
    not establish a Session_actor or Runtime_owner binding. The actual target
    preparation must bind this value before held Run admission. Full live tool
    admission and compilation remain required before any initializer, after the
    separate execution barrier. Cancellation and checkpoint exceptions propagate.
    No borrowed startup context is retained. *)
val prepare
  :  Prompt_revision.t
  -> script_preparation:Runtime_script_preparation.t option
  -> invocation:Runtime_script_preparation.Invocation.t
  -> env:Eio_unix.Stdenv.base
  -> checkpoint:(unit -> unit)
  -> (t, Agent_protocol.Error.t) result

val revision : t -> Prompt_revision.t
val invocation : t -> Runtime_script_preparation.Invocation.t
val moderator : t -> Moderator.t

(** Exact immutable restored custody; no live capabilities or compiler deadline
    are retained. None means validated absence of an active captured program graph, including
    ordinary single-file sources. *)
val programs : t -> Chatml_prompt_programs.Restored.t option
