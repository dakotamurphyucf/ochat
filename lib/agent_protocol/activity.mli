(** Neutral transient tool activity. A call alias is scoped to an actual source
    and attempt; it never substitutes for a host history or invocation ID. *)
module Key : sig
  type parent =
    { scope : Transcript.Scope.Key.t
    ; call_alias : string
    }
  [@@deriving compare, equal, hash, sexp_of]

  type t = private
    { scope : Transcript.Scope.Key.t
    ; call_alias : string
    ; parent : parent option
    }
  [@@deriving compare, equal, hash, sexp_of]

  val create
    :  scope:Transcript.Scope.Key.t
    -> call_alias:string
    -> parent:parent option
    -> (t, Error.t) result
end

module Progress : sig
  type channel =
    | Assistant
    | Reasoning
    | Stdout
    | Stderr
    | Activity
  [@@deriving compare, equal, sexp_of]

  type update =
    | Append of string
    | Replace of string
  [@@deriving equal, sexp_of]

  type t =
    { channel : channel
    ; update : update
    }
  [@@deriving equal, sexp_of]
end

module Tool : sig
  type classification =
    | Subagent
    | Shell_script
  [@@deriving equal, sexp_of]

  type outcome =
    | Returned
    | Raised
    | Cancelled
  [@@deriving equal, sexp_of]

  type descriptor = private
    { key : Key.t
    ; call_entry_id : History_entry.Id.t option
    ; name : string
    ; kind : History_entry.Payload.Call_kind.t
    ; input : string
    ; classification : classification option
    }
  [@@deriving sexp_of]

  val descriptor
    :  Key.t
    -> call_entry_id:History_entry.Id.t option
    -> name:string
    -> kind:History_entry.Payload.Call_kind.t
    -> input:string
    -> classification:classification option
    -> (descriptor, Error.t) result

  type event =
    | Started of descriptor
    | Progress of
        { key : Key.t
        ; progress : Progress.t
        }
    | Finished of
        { key : Key.t
        ; outcome : outcome
        ; output : History_entry.Payload.Output.t option
        }
  [@@deriving sexp_of]

  (** Nested activity records the actual parent scope and call alias. Only existing
      actual parent attribution is supplied; no guessed parent or host ID. *)
  val key : event -> Key.t

  val to_json : event -> Jsonaf.t
  val of_json : Jsonaf.t -> (event, Error.t) result

  type channel_text =
    { channel : Progress.channel
    ; text : string
    ; complete : bool
    }
  [@@deriving sexp_of]

  type state =
    | Running
    | Finished of
        { outcome : outcome
        ; output : History_entry.Payload.Output.t option
        }
  [@@deriving sexp_of]

  type summary = private
    { key : Key.t
    ; descriptor : descriptor option
    ; channels : channel_text list
    ; state : state
    }
  [@@deriving sexp_of]

  (** Missing descriptors and incomplete channel prefixes remain explicit after
      a live gap. The client ordering owner performs bounded accumulation. *)
  val summary
    :  Key.t
    -> descriptor:descriptor option
    -> channels:channel_text list
    -> state:state
    -> (summary, Error.t) result

  val summary_to_json : summary -> Jsonaf.t
  val summary_of_json : Jsonaf.t -> (summary, Error.t) result
end
