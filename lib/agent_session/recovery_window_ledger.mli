open! Core
module P = Agent_protocol

(** Immutable canonical membership and local source coordinates. The ledger
    contains no transcript bodies, file paths, authorization, or provider
    submission claim. Prepared/effective members need a separate admitted
    source contract before they can be located. *)
module Source = Recovery_window_location.Source

module Member = Recovery_window_location.Member
module Window = Recovery_window_record

type t

(** Admit the complete chain from its first root to its single open window.
    Scope, predecessor, generation, operation, and occurrence uniqueness do
    not depend on file listing, allocation order, or provider identifiers. *)
val create : owner:P.Session_ref.t -> Window.t list -> (t, P.Error.t) result

val owner : t -> P.Session_ref.t
val windows : t -> Window.t list
val active : t -> Window.t

(** Validate against the owning committed state and its exact summary archive
    references. [has_summary_archive] runs once per distinct archived root.
    This validates retained root identity; verified frame I/O and authorization
    remain with the actual archive read. *)
val validate_state
  :  t
  -> session_id:P.Id.Session.t
  -> generation:int
  -> revision:int64
  -> history:P.History.entry list
  -> has_summary_archive:(Source.archive -> bool)
  -> has_summary_operation:(P.Id.Operation.t -> bool)
  -> (unit, P.Error.t) result

(** Versioned named fields. Identity-keyed shapes carry unknown members
    through the session document. The 2 MiB encoded, 64-window and 8192-member
    aggregate ceilings apply to enabled sessions only. *)
val shape : Document_schema.Shape.t

val to_jsonaf : t -> Jsonaf.t
val of_jsonaf : Jsonaf.t -> (t, P.Error.t) result
val validate_encoded : Jsonaf.t -> (unit, Document_schema.Error.t) result
