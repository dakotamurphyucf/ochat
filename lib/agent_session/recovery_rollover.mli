(** Durable intent and terminal receipts for a host-owned recovery rollover. *)
open! Core

module P = Agent_protocol

module Expected : sig
  type t = private
    { scope : P.Recovery_window.Scope.t
    ; revision : int64
    }
  [@@deriving equal, sexp_of]

  val create : P.Recovery_window.Scope.t -> revision:int64 -> (t, P.Error.t) result
end

module Requester : sig
  type t =
    | Operator of
        { principal : P.Id.Principal.t
        ; key : P.Idempotency_key.t
        ; force : bool
        }
    | Native of
        { principal : P.Id.Principal.t
        ; operation : P.Id.Operation.t
        ; invocation : P.Id.Invocation.t
        }
  [@@deriving equal, sexp_of]
end

module Intent : sig
  type t = private
    { id : P.Id.Operation.t
    ; expected : Expected.t
    ; requester : Requester.t
    ; admitted_at : P.Timestamp.t
    }
  [@@deriving equal, sexp_of]

  val create
    :  id:P.Id.Operation.t
    -> expected:Expected.t
    -> requester:Requester.t
    -> admitted_at:P.Timestamp.t
    -> t
end

module Receipt : sig
  type supersession =
    | Basis_changed
    | Cancelled
    | Checkpoint_required
  [@@deriving equal, sexp_of]

  type committed =
    { successor : P.Recovery_window.Scope.t
    ; cut_revision : int64
    ; archive : Chatml_source.Digest.t
    }
  [@@deriving equal, sexp_of]

  type terminal =
    | Committed of committed
    | Superseded of supersession
  [@@deriving equal, sexp_of]

  type status =
    | Pending
    | Terminal of terminal
  [@@deriving equal, sexp_of]

  type t = private
    { intent : Intent.t
    ; status : status
    }
  [@@deriving equal, sexp_of]

  (** Body-free projection for an authorized caller. Requester keys and native
      ownership are private to the durable representation. *)
  val to_public_json : t -> Jsonaf.t
end

type t

val find : t -> id:P.Id.Operation.t -> Receipt.t option
val pending : t -> Intent.t option

(** Exact normalized retries are resolved before comparing current head or
    revision. The caller separately checks current authority before lookup. *)
val previous
  :  t option
  -> requester:Requester.t
  -> expected:Expected.t
  -> (Receipt.t option, P.Error.t) result

module Change : sig
  type t =
    | Admit of Intent.t
    | Settle of
        { id : P.Id.Operation.t
        ; terminal : Receipt.terminal
        }

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, P.Error.t) result
end

(** Journal reducer. Admission compares exact window and revision. Settlement
    requires the matching pending identity. The publishing actor checks genuine
    archive and successor ownership in the same transaction. *)
val apply
  :  t option
  -> active:P.Recovery_window.Scope.t
  -> revision:int64
  -> Change.t
  -> (t, P.Error.t) result

val validate_state
  :  t
  -> windows:Recovery_window_ledger.t option
  -> revision:int64
  -> (unit, P.Error.t) result

val check_unchanged : t option -> incoming:t option -> (unit, P.Error.t) result
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, P.Error.t) result
