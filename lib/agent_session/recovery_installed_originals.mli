(** Private address-only receipts for exact initial canonical parts. These are
    lookup coordinates. They grant neither source custody nor recovery coverage. *)

module P = Agent_protocol
module Occurrence = Agent_store.Installed_document_originals.Occurrence

module Binding : sig
  type t = private
    { row : P.History.Id.t
    ; revision : P.History.Content_revision.t
    ; part : int
    ; original : Occurrence.t
    }
end

module Initial : sig
  type t

  (** From the actual allocated rows and their emitted part witnesses. The
      canonical bodies are borrowed only until initialization admission. *)
  val create
    :  definition:P.Id.Prompt_definition.t
    -> revision:P.Id.Prompt_revision.t
    -> (History_entry.t * (int * Occurrence.t) list) list
    -> (t option, P.Error.t) result

  val matches_source
    :  t
    -> definition:P.Id.Prompt_definition.t option
    -> revision:P.Id.Prompt_revision.t
    -> bool
end

type t

(** Admit only unchanged allocated rows in the accepted initial prefix. An
    omitted or changed canonical row keeps its normal rendering and gains
    no original binding. *)
val admit
  :  Initial.t
  -> owner:P.Session_ref.t
  -> generation:int
  -> initial_rows:P.History.entry list
  -> (t option, P.Error.t) result

val validate
  :  t
  -> session_id:P.Id.Session.t
  -> generation:int
  -> (unit, P.Error.t) result

val definition : t -> P.Id.Prompt_definition.t
val revision : t -> P.Id.Prompt_revision.t
val bindings : t -> Binding.t list
val belongs_to : t -> owner:P.Session_ref.t -> generation:int -> bool
val equal : t -> t -> bool
val to_jsonaf : t -> Jsonaf.t
val of_jsonaf : Jsonaf.t -> (t, P.Error.t) result
val shape : Document_schema.Shape.t
val validate_encoded : Jsonaf.t -> (unit, Document_schema.Error.t) result
