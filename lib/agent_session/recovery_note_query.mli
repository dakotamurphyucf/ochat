open! Core

(** Bounded queries over one admitted, immutable local catalog. Plans retain
    only revision coordinates and a request, never catalog contents. *)
module Catalog = Recovery_notes.Catalog

module Range : sig
  type t

  (** Half-open UTF-8 byte offsets. Both endpoints must be scalar boundaries
      in the selected revision. Responses contain at most 4 KiB. *)
  val create : start:int -> stop:int -> (t, Agent_protocol.Error.t) result

  val read : t -> Catalog.Content.t -> (Jsonaf.t, Agent_protocol.Error.t) result
end

module Page : sig
  type t

  type cursor =
    { revision : Catalog.Catalog_revision.t
    ; after : Catalog.Note_path.t
    ; query : Chatml_source.Digest.t
    }

  (** At most 16 entries. The cursor is valid only for the exact catalog
      revision and next page of the same query. *)
  val create : limit:int -> cursor:cursor option -> (t, Agent_protocol.Error.t) result
end

type t =
  | Read of Catalog.selection * Range.t
  | List of Page.t
  | Search of Agent_protocol.Search_term.t * Page.t

type plan

val prepare : t -> Catalog.t -> (plan, Agent_protocol.Error.t) result
val current : plan -> Catalog.t -> bool
val render : plan -> Catalog.t -> (Jsonaf.t, Agent_protocol.Error.t) result
