open! Core

module Id : sig
  type t [@@deriving compare, equal, hash, sexp]

  val canonical : History_entry.Id.t -> t
  val local : namespace:string -> local_id:string -> (t, string) result
  val to_string : t -> string
end

type provenance =
  | Canonical
  | Moderator_inserted of { change_id : int }
  | Moderator_replacement of
      { target_id : History_entry.Id.t
      ; change_id : int
      }
  | Streaming
  | Pending_approval
  | Placeholder
[@@deriving equal, sexp]

type source =
  | Canonical of { entry_id : History_entry.Id.t }
  | Moderator_inserted of
      { entry_id : History_entry.Id.t
      ; change_id : int
      }
  | Moderator_replacement of
      { target_id : History_entry.Id.t
      ; change_id : int
      }
  | Streaming of
      { entry_id : History_entry.Id.t
      ; provider_item_id : string option
      ; call_id : string option
      }
  | Pending_approval of { local_id : string }
  | Placeholder of
      { local_id : string
      ; kind : string
      }
[@@deriving equal, sexp]

type t =
  { id : Id.t
  ; entry_id : History_entry.Id.t option
  ; message : Types.message
  ; provenance : provenance
  ; source : source
  ; revision : int
  }

(** [render_equal a b] returns [true] when all render-affecting fields except
    revision are equal. *)
val render_equal : t -> t -> bool

(** [reconcile ~previous row] preserves revision for unchanged rows and
    increments it when the same ID has changed. *)
val reconcile : previous:t option -> t -> t

val canonical_row : entry_id:History_entry.Id.t -> Types.message -> t
