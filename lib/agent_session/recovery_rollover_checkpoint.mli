(** Private evidence for the history and immutable local note revisions selected
    by an authorized checkpoint workflow. It carries no current authority. *)
open! Core

module P = Agent_protocol

module Expected : sig
  type t = private
    { scope : P.Recovery_window.Scope.t
    ; revision : int64
    }
  [@@deriving equal, sexp_of]

  val create : P.Recovery_window.Scope.t -> revision:int64 -> (t, P.Error.t) result
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, P.Error.t) result
end

type t [@@deriving equal, sexp_of]

type recipe =
  | Owner_selection of P.Id.Principal.t
  | Authored of Recovery_authored_checkpoint.t

val capture
  :  Expected.t
  -> history:P.History.entry list
  -> catalog:Recovery_notes.Catalog.t option
  -> notes:Recovery_notes.Catalog.selection list
  -> recipe:recipe
  -> (t, P.Error.t) result

val expected : t -> Expected.t
val recipe : t -> recipe
val notes : t -> Recovery_notes.Catalog.selection list

(** Bounded fingerprint of the ordered canonical source and its content revisions. *)
val history_digest : P.History.entry list -> (Chatml_source.Digest.t, P.Error.t) result

val check_contents
  :  t
  -> history:P.History.entry list
  -> catalog:Recovery_notes.Catalog.t option
  -> (unit, P.Error.t) result

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, P.Error.t) result
