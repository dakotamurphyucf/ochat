(** Actual invoking session's host service. Installing it grants no selection
    or authorization. The actor validates the original admission. *)
type delivery =
  { custody : Inference_runtime.Protected_delivery.custody
  ; materialize :
      unit
      -> ( History_entry.Payload.Output.t
           , Inference_runtime.Protected_delivery.error )
           result
  ; refresh : scope:Transcript.Scope.t -> unit -> bool
  }

type t =
  { prepare :
      access:Recovery_note_access.t
      -> Recovery_note_query.t
      -> (delivery, Agent_protocol.Error.t) result
  ; mutate :
      access:Recovery_note_access.t
      -> Recovery_notes.Catalog.mutation
      -> (Recovery_notes.Catalog.Receipt.t, Agent_protocol.Error.t) result
  }
