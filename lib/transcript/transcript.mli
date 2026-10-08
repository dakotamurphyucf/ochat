open! Core

(** Pure neutral transcript read views and transient draft reduction. No protocol
    IDs, OpenAI, Eio or UI dependency. No event authorizes canonical append. *)
module Admission : sig
  (** Shared presentation-envelope limits: depth 160, one million object fields
      and two million nodes. Canonical payload limits remain unchanged; these
      bounds allow wrapper headroom and multiple payloads in a public snapshot.
      [max_bytes] remains the caller's negotiated compact JSON byte ceiling. *)
  val limits
    :  max_bytes:int
    -> (Document_schema.Limits.t, Document_schema.Error.t) Result.t

  (** [limits ~max_bytes:(16 * 1024 * 1024)]. *)
  val default : Document_schema.Limits.t
end

module Header : sig
  type t =
    | Message of History_entry.Payload.Role.t
    | Call of History_entry.Payload.Call_kind.t
    | Result of History_entry.Payload.Call_kind.t
    | Reasoning
    | Unknown of string
  [@@deriving equal, sexp_of]

  val of_semantic : History_entry.Payload.Semantic.t -> t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, string) Result.t
end

module Source_id : sig
  type t [@@deriving compare, equal, hash, sexp_of]

  include Comparator.S with type t := t

  val of_string : string -> (t, string) Result.t
  val to_string : t -> string
end

module Attempt_id : sig
  type t [@@deriving compare, equal, hash, sexp_of]

  include Comparator.S with type t := t

  val of_string : string -> (t, string) Result.t
  val to_string : t -> string
end

module Item_id : sig
  type t [@@deriving compare, equal, hash, sexp_of]

  include Comparator.S with type t := t

  val of_string : string -> (t, string) Result.t
  val to_string : t -> string
end

module Part_id : sig
  type t [@@deriving compare, equal, hash, sexp_of]

  include Comparator.S with type t := t

  val of_string : string -> (t, string) Result.t
  val to_string : t -> string
end

module Scope : sig
  module Key : sig
    type t =
      { source : Source_id.t
      ; attempt : Attempt_id.t
      }
    [@@deriving compare, equal, hash, sexp_of]

    include Comparator.S with type t := t
  end

  type parent =
    { scope : Key.t
    ; call_entry_id : History_entry.Id.t option
    ; call_alias : string option
    }

  type relation =
    | Root
    | Nested of parent

  type t = private
    { key : Key.t
    ; relation : relation
    }

  val create
    :  source:Source_id.t
    -> attempt:Attempt_id.t
    -> relation:relation
    -> (t, string) Result.t

  val key : t -> Key.t

  (** Includes the parent relation and its actual call binding, not just [key]. *)
  val equal : t -> t -> bool

  val to_json : t -> Jsonaf.t

  (** Admits the complete JSON tree under [limits], including unknown fields,
      before decoding identities and validating the parent relation. *)
  val of_json : Jsonaf.t -> limits:Document_schema.Limits.t -> (t, string) Result.t
end

module Item : sig
  module Key : sig
    type t =
      { scope : Scope.Key.t
      ; item : Item_id.t
      }
    [@@deriving compare, equal, hash, sexp_of]

    include Comparator.S with type t := t
  end

  type t = private
    { scope : Scope.t
    ; id : Item_id.t
    ; entry_id : History_entry.Id.t option
    ; header : Header.t option
    ; call_name : string option
    }

  (** Item IDs are local to scope; optional entry_id is an actual reserved/owned
      host ID, not evidence of admission. Nonempty UTF8 identities/names; names
      only for Call (or unavailable header None). No provider role default. *)
  val create
    :  scope:Scope.t
    -> id:Item_id.t
    -> entry_id:History_entry.Id.t option
    -> header:Header.t option
    -> call_name:string option
    -> (t, string) Result.t

  (** Fills missing information; rejects a changed known host ID/classification.
      Header None may become known, call_name None may become known. *)
  val refine : t -> t -> (t, string) Result.t

  val key : t -> Key.t
end

module Part : sig
  module Key : sig
    type t =
      { item : Item.Key.t
      ; part : Part_id.t
      }
    [@@deriving compare, equal, hash, sexp_of]

    include Comparator.S with type t := t
  end

  type kind =
    | Text
    | Refusal
    | Reasoning_summary
    | Reasoning_text
    | Image
    | Unknown of string
  [@@deriving equal, sexp_of]

  type t = private
    { item : Item.t
    ; id : Part_id.t
    ; index : int option
    ; kind : kind
    }

  (** Index is an actual nonnegative provider/host position, or unavailable.
      No index=0 default. IDs local to the item; no inferred media/file body. *)
  val create
    :  item:Item.t
    -> id:Part_id.t
    -> index:int option
    -> kind:kind
    -> (t, string) Result.t

  val key : t -> Key.t
end

module Stream : sig
  module Target : sig
    type t =
      | Content of Part.t
      | Call_input of Item.t
  end

  type change =
    | Append of string
    | Replace of string

  type completion =
    | Complete
    | Incomplete
    | Failed
    | Cancelled

  type view =
    | Source_started of
        { scope : Scope.t
        ; origin : History_entry.Payload.Origin.t
        }
    | Item_announced of Item.t
    | Part_announced of Part.t
    | Changed of
        { target : Target.t
        ; change : change
        }
    | Item_finalized of
        { item : Item.t
        ; entry : History_entry.t
        }
    | Source_finished of
        { scope : Scope.t
        ; completion : completion
        }
    | Unknown_event of
        { scope : Scope.t
        ; provider_kind : string
        ; raw : Jsonaf.t
        }

  type t

  (** Checks native invariants plus complete JSON admission under limits. Changed
      carries a self-describing scoped item/part. Content Append/Replace only
      applies to text/refusal/reasoning kinds; media is announced/finalized only.
      Finalized requires descriptor.entry_id = Some(actual entry.id), compatible
      derived header/call name and canonical payload validation, and preserves
      EXACT immutable entry.payload. Presentation headroom cannot admit a payload
      that exceeds its own canonical bounds. *)
  val create : view -> limits:Document_schema.Limits.t -> (t, string) Result.t

  val sexp_of_t : t -> Sexp.t
  val view : t -> view
  val scope : t -> Scope.t
  val encoded_bytes : t -> int
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> limits:Document_schema.Limits.t -> (t, string) Result.t
end

module Draft : sig
  module Limits : sig
    type t

    val create
      :  max_scopes:int
      -> max_items:int
      -> max_parts:int
      -> max_unknown_events:int
      -> max_retained_bytes:int
      -> document_limits:Document_schema.Limits.t
      -> (t, string) Result.t
  end

  type completeness =
    | Prefix_observed
    | Missing_prefix

  type text = private
    { value : string
    ; completeness : completeness
    }

  type part_view = private
    { descriptor : Part.t
    ; text : text option
    }

  type partial = private
    { parts : part_view list
    ; call_input : text option
    ; completeness : completeness
    }

  type state =
    | Partial of partial
    | Finalized of History_entry.t

  type item_view = private
    { descriptor : Item.t
    ; state : state
    }

  type unknown_view = private
    { scope : Scope.t
    ; provider_kind : string
    ; raw : Jsonaf.t
    }

  type source_view = private
    { scope : Scope.t
    ; origin : History_entry.Payload.Origin.t
    ; completion : Stream.completion option
    }

  type change =
    | Item_changed of item_view
    | Source_changed of source_view
    | Unknown_observed of unknown_view
    | Scope_cleared of Scope.Key.t

  type t

  val create : limits:Limits.t -> t

  (** Pure atomic update; expected malformed/conflict/limit errors leave receiver
      unchanged. No retained event log, provider decode or canonical append.
      Caller override is capped by configured Limits. Checked against the
      candidate retained charge BEFORE concatenate/final/raw retention, not a
      post-allocation limit. Byte charge is cached across immutable updates. *)
  val apply
    :  t
    -> ?max_retained_bytes:int
    -> Stream.t
    -> (t * change list, string) Result.t

  val retained_bytes : t -> int
  val mark_gap : t -> scope:Scope.Key.t option -> t
  val clear_scope : t -> Scope.Key.t -> t
  val remove_item : t -> Item.Key.t -> t

  (** Retained items in first-admission order, independent of opaque provider
      aliases. Refinement, replacement and finalization preserve their position;
      removing and admitting an item again gives it a new position. Ordering
      retains only the bounded set of currently admitted scoped identities. *)
  val items : t -> item_view list

  val sources : t -> source_view list
  val unknown_events : t -> unknown_view list
end
