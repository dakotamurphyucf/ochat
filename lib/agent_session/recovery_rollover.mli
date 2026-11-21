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

module Checkpoint : sig
  (** Owner-selected note revisions and the exact canonical history they cover.
      Capture alone confers no authority. Publication requires the same proof in
      the current journal ledger and rechecks its history and note revisions. *)
  type t [@@deriving equal, sexp_of]

  type recipe = Owner_selection of P.Id.Principal.t

  val capture
    :  Expected.t
    -> history:P.History.entry list
    -> catalog:Recovery_notes.Catalog.t option
    -> notes:Recovery_notes.Catalog.selection list
    -> recipe:recipe
    -> (t, P.Error.t) result
end

module Receipt : sig
  type supersession =
    | Basis_changed
    | Cancelled
    | Checkpoint_required
  [@@deriving equal, sexp_of]

  type archive_only_reason =
    | Checkpoint_missing
    | Checkpoint_unavailable
  [@@deriving equal, sexp_of]

  type checkpoint_quality =
    | Checkpointed of Checkpoint.t
    | Archive_only of archive_only_reason
  [@@deriving equal, sexp_of]

  type continuation =
    | Awaiting_after_commit of P.Id.Operation.t
    | Awaiting_settled_foreground of
        { owner : P.Id.Operation.t
        ; predecessor : P.Id.Operation.t
        }
    | Scheduled_foreground of Recovery_rollover_continuation.t
  [@@deriving equal, sexp_of]

  type committed =
    { successor : P.Recovery_window.Scope.t
    ; cut_revision : int64
    ; archive : Chatml_source.Digest.t
    ; checkpoint : checkpoint_quality
    ; continuation : continuation
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

module Commit : sig
  (** The ordered publication delta. Retained IDs select exact existing live
      entries in successor order. No copied history or note body travels here. *)
  type t = private
    { id : P.Id.Operation.t
    ; expected : Expected.t
    ; successor : P.Recovery_window.Scope.t
    ; archive : Chatml_source.Digest.t
    ; checkpoint : Receipt.checkpoint_quality
    ; retain : P.History.Id.t list
    ; batch : Recovery_rollover_batch.t option
    ; schedule_foreground : bool
    }

  val create
    :  ?batch:Recovery_rollover_batch.t
    -> ?schedule_foreground:bool
    -> Intent.t
    -> expected:Expected.t
    -> successor:P.Recovery_window.Scope.t
    -> archive:Chatml_source.Digest.t
    -> checkpoint:Receipt.checkpoint_quality
    -> retain:P.History.Id.t list
    -> (t, P.Error.t) result

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, P.Error.t) result
end

module Settlement : sig
  type t =
    | Cancel
    | Publish of
        { checkpoint : Checkpoint.t option
        ; retain : P.History.Id.t list
        }
end

type t

val find : t -> id:P.Id.Operation.t -> Receipt.t option
val pending : t -> Intent.t option
val checkpoint : t -> Checkpoint.t option

val foreground
  :  t
  -> scope:P.Recovery_window.Scope.t
  -> Recovery_rollover_continuation.t option

val awaiting_foreground : t -> scope:P.Recovery_window.Scope.t -> bool

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
    | Record_checkpoint of Checkpoint.t
    | Settle of
        { id : P.Id.Operation.t
        ; terminal : Receipt.terminal
        }
    | Schedule of
        { id : P.Id.Operation.t
        ; expected : Expected.t
        ; change : Recovery_rollover_continuation.change
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
  -> history:P.History.entry list
  -> catalog:Recovery_notes.Catalog.t option
  -> Change.t
  -> (t, P.Error.t) result

val check_checkpoint
  :  ?checkpoint_history:P.History.entry list
  -> t
  -> Checkpoint.t
  -> expected:Expected.t
  -> history:P.History.entry list
  -> catalog:Recovery_notes.Catalog.t option
  -> (unit, P.Error.t) result

val commit
  :  ?checkpoint_history:P.History.entry list
  -> t
  -> Commit.t
  -> active:P.Recovery_window.Scope.t
  -> revision:int64
  -> history:P.History.entry list
  -> catalog:Recovery_notes.Catalog.t option
  -> (t, P.Error.t) result

val validate_state
  :  t
  -> windows:Recovery_window_ledger.t option
  -> revision:int64
  -> has_archive:(id:P.Id.Operation.t -> revision:int64 -> Chatml_source.Digest.t -> bool)
  -> (unit, P.Error.t) result

val check_unchanged : t option -> incoming:t option -> (unit, P.Error.t) result
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, P.Error.t) result
