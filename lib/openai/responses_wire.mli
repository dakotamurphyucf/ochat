(** Pure, lossless capture boundary for selected Responses output and SSE JSON.
    No network, auth, tool execution or legacy [Responses.Item] dependency.
    Raw accessors return the immutable original JSON value (including unknown
    keys), not its original lexical encoding. Captures reject duplicate object
    keys, invalid JSON number literals, depth above 128 and more than 100000 nodes. Exact call strings are unchanged.
    Treat raw captures as private conversation data, never automatic diagnostics. *)
open Core

module Presence : sig
  type 'a t =
    | Absent
    | Null
    | Value of 'a
  [@@deriving equal, sexp]
end

module Decode_error : sig
  type reason =
    | Missing_field
    | Wrong_type of string
    | Invalid_value of string
    | Duplicate_field of string
    | Limit_exceeded
  [@@deriving equal, sexp]

  type t =
    { path : string
    ; reason : reason
    }
  [@@deriving equal, sexp]
end

module Origin : sig
  type t

  (** Identity is supplied by the caller; nonempty nonsecret compatibility
      references, not inferred from response JSON or ambient configuration.
      [None] is an explicitly unknown account. Identity labels correlation;
      they do not authorize opaque replay or tool execution. *)
  val create
    :  provider:string
    -> account:string option
    -> endpoint:string
    -> (t, Decode_error.t) Result.t

  val provider : t -> string
  val account : t -> string option
  val endpoint : t -> string
  val equal : t -> t -> bool
end

module Status : sig
  type t =
    | In_progress
    | Completed
    | Incomplete
    | Failed
    | Cancelled
    | Queued
    | Other of string
  [@@deriving equal, sexp]
end

module Phase : sig
  type t =
    | Commentary
    | Final_answer
    | Other of string
  [@@deriving equal, sexp]
end

module Part : sig
  type view =
    | Output_text of
        { text : string
        ; annotations : Jsonaf.t list
        ; logprobs : Jsonaf.t Presence.t
        }
    | Refusal of string
    | Summary_text of string
    | Reasoning_text of string
    | Unknown of string

  type t

  val decode : Jsonaf.t -> origin:Origin.t -> (t, Decode_error.t) Result.t
  val view : t -> view
  val raw : t -> Jsonaf.t
  val origin : t -> Origin.t
end

module Item : sig
  type call =
    | Function of
        { name : string
        ; namespace : string Presence.t
        ; call_id : string
        ; arguments : string
        ; async : bool Presence.t
        }
    | Custom of
        { name : string
        ; namespace : string Presence.t
        ; call_id : string
        ; input : string
        ; async : bool Presence.t
        }

  type view =
    | Message of
        { content : Part.t list
        ; phase : Phase.t Presence.t
        }
    | Call of call
    | Reasoning of
        { summary : Part.t list
        ; content : Part.t list Presence.t
        ; encrypted_content : string Presence.t
        }
    | Unknown of string

  type t

  (** Selected known kinds are validated; unknown kinds remain opaque, never
      [Call]. Output messages must have assistant role. Function/custom payloads
      stay strings; this decoder never parses or admits tool arguments. *)
  val decode : Jsonaf.t -> origin:Origin.t -> (t, Decode_error.t) Result.t

  val view : t -> view
  val id : t -> string Presence.t
  val status : t -> Status.t Presence.t
  val raw : t -> Jsonaf.t
  val origin : t -> Origin.t

  (** A finalization may expose only selected calls with completed/omitted
      status and absent namespace (namespaced dispatch is not selected here).
      Unknown caller semantics and async=true also require future explicit support. Host admission/schema validation is still required before execution. *)
  val local_call : t -> call option
end

module Usage : sig
  type t

  (** Actual counts, nonnegative int64. Missing usage is unknown, never zero.
      Detail counts are subsets, not additive totals. Unknown keys remain raw. *)
  val input_tokens : t -> int64

  val output_tokens : t -> int64
  val total_tokens : t -> int64
  val cached_tokens : t -> int64 Presence.t
  val cache_write_tokens : t -> int64 Presence.t
  val reasoning_tokens : t -> int64 Presence.t
  val raw : t -> Jsonaf.t
end

module Provider_error : sig
  type t =
    { code : string Presence.t
    ; message : string
    ; param : string Presence.t
    }
end

module Response : sig
  type t

  type outcome =
    | Completed
    | Refused
    | Incomplete of { reason : string Presence.t }
    | Failed of Provider_error.t Presence.t
    | Nonterminal of Status.t Presence.t

  (** Refused is a completed response containing a refusal part; it does not
      turn incomplete/failed into success. Response output retains wire order. *)
  val decode : Jsonaf.t -> origin:Origin.t -> (t, Decode_error.t) Result.t

  val id : t -> string
  val status : t -> Status.t Presence.t
  val output : t -> Item.t list
  val usage : t -> Usage.t Presence.t
  val error : t -> Provider_error.t Presence.t
  val incomplete_reason : t -> string Presence.t
  val outcome : t -> outcome
  val raw : t -> Jsonaf.t
  val origin : t -> Origin.t
end

module Event : sig
  module Delta_kind : sig
    type t =
      | Text
      | Refusal
      | Reasoning_summary
      | Reasoning_text
      | Function_arguments
      | Custom_input
    [@@deriving equal, compare, sexp]
  end

  module Part_space : sig
    type t =
      | Content
      | Summary
    [@@deriving equal, compare, sexp]
  end

  type location =
    { item_id : string
    ; output_index : int
    ; part_index : int option
    }

  type lifecycle =
    | Created
    | In_progress
  [@@deriving equal, sexp]

  type terminal =
    | Completed
    | Incomplete
    | Failed
  [@@deriving equal, sexp]

  type view =
    | Response of
        { lifecycle : lifecycle
        ; response : Response.t
        }
    | Item_added of
        { output_index : int
        ; item : Item.t
        }
    | Item_done of
        { output_index : int
        ; item : Item.t
        }
    | Part_added of
        { location : location
        ; part_space : Part_space.t
        ; part : Part.t
        }
    | Part_done of
        { location : location
        ; part_space : Part_space.t
        ; part : Part.t
        }
    | Delta of
        { location : location
        ; kind : Delta_kind.t
        ; delta : string
        }
    | Text_done of
        { location : location
        ; kind : Delta_kind.t
        ; text : string
        }
    | Annotation_added of
        { location : location
        ; annotation_index : int
        ; annotation : Jsonaf.t
        }
    | Terminal of
        { terminal : terminal
        ; response : Response.t
        }
    | Error of Provider_error.t
    | Unknown of string

  type t

  (** Selected events require nonnegative sequence/index metadata. Unknown
      event names retain their full raw envelope and optional sequence metadata.
      Malformed selected event types fail rather than becoming [Unknown]. *)
  val decode : Jsonaf.t -> origin:Origin.t -> (t, Decode_error.t) Result.t

  val view : t -> view
  val sequence_number : t -> int64 Presence.t
  val raw : t -> Jsonaf.t
  val origin : t -> Origin.t
end

module Tracker : sig
  type t

  type error =
    | Origin_mismatch
    | Sequence_conflict of int64
    | Sequence_regression of
        { previous : int64
        ; received : int64
        }
    | Item_conflict of int
    | Part_conflict of int
    | Response_conflict
    | Event_after_terminal
    | Terminal_mismatch
    | Truncated
  [@@deriving equal, sexp]

  type disposition =
    | Accepted
    | Duplicate
  [@@deriving equal, sexp]

  type transition =
    { tracker : t
    ; disposition : disposition
    ; newly_finalized : (int * Item.t) list
    }

  type completion =
    | Response of
        { terminal : Event.terminal
        ; response : Response.t
        }
    | Error of Provider_error.t

  val create : Origin.t -> t

  (** Functional, single-response state. An exact duplicate finalization is
      suppressed; a conflicting finalization is rejected. Only first validated
      completed/omitted-status calls occur in [newly_finalized]. Identity is the
      output slot/item ID within this response, never global provider call_id.
      This is validation, not tool admission or orchestration. Descriptors stay
      stable from added to done, completed terminals reconcile every observed
      slot/part, and terminal-only incomplete/failed calls are not emitted.
      Deltas are not assembled here; supplied final text/part/annotation snapshots
      are checked against final items. Retention is linear in observed slots and
      parts; retain only the returned tracker if old snapshots are unneeded.
      Equality ignores object key order but preserves array order, exact string
      and numeric representation, and field presence. Unknown annotations have
      no invented value; a null annotation still requires its indexed slot. *)
  val add : t -> Event.t -> (transition, error) Result.t

  (** EOF requires a validated terminal response or explicit provider error.
      Seeing a completed output item alone is not a completed response. *)
  val finish : t -> (completion, error) Result.t
end
