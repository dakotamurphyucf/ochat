open! Core

(** Explicit legacy runtime projection. Canonical validation never calls this
    module. Legacy DTO snapshots are Reconstructed, never original wire captures.
    Unsupported neutral/captured items remain valid durable history. *)
val of_item
  :  ?call_relation:History_entry.Payload.Call_relation.t
  -> Responses.Item.t
  -> (History_entry.Payload.t, string) Result.t

(** Local tool results have authored semantics. This boundary retains every output
    part and the supplied call occurrence without retaining a duplicate DTO body.
    The producer supplies no provider item/response ID or provider status. *)
val authored_output
  :  kind:History_entry.Payload.Call_kind.t
  -> call_id:string
  -> call_relation:History_entry.Payload.Call_relation.t
  -> output:Responses.Tool_output.Output.t
  -> (History_entry.Payload.t, string) Result.t

val of_wire_item
  :  ?call_relation:History_entry.Payload.Call_relation.t
  -> Responses_wire.Item.t
  -> (History_entry.Payload.t, string) Result.t

(** Actual wire captures require the neutral runtime adapter. This explicit
    rejection prevents legacy request lowering from losing opaque replay data. *)
val to_item : History_entry.Payload.t -> (Responses.Item.t, string) Result.t

(** Readable presentation projection only. Does not authorize replay or tools. *)
val to_presentation_item : History_entry.Payload.t -> (Responses.Item.t, string) Result.t

val create
  :  allocator:History_entry.Allocator.t
  -> Responses.Item.t
  -> (History_entry.t, string) Result.t

val create_with_id_exn
  :  ?call_relation:History_entry.Payload.Call_relation.t
  -> id:History_entry.Id.t
  -> Responses.Item.t
  -> History_entry.t

val item_exn : History_entry.t -> Responses.Item.t
val items_exn : History_entry.t list -> Responses.Item.t list

(** A host edit preserves identity and deliberately creates authored semantics,
    discarding the old capture's replay eligibility. *)
val with_item_exn : History_entry.t -> Responses.Item.t -> History_entry.t

(** Resolve only the nearest actual call occurrence under existing family/provider
    metadata pairing semantics. Missing/already-result occurrences stay Unresolved. *)
val relation_for_item
  :  history:History_entry.t list
  -> Responses.Item.t
  -> History_entry.Payload.Call_relation.t

val of_items
  :  preceding:History_entry.t list
  -> allocator:History_entry.Allocator.t
  -> Responses.Item.t list
  -> (History_entry.t list, string) Result.t

(** Provider producer projection of actual output parts; no inferred call kind,
    JSON decode, canonical admission or provider replay. *)
val output_to_neutral : Responses.Tool_output.Output.t -> History_entry.Payload.Output.t
