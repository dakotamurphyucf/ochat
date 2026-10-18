(** Bounded REPL presentation and reference-page codecs. Shares the private request limits and IDs; no runtime resources or effect authority. *)

module Limits = Developer_repl_wire_common.Limits
module Wire_id = Developer_repl_wire_common.Wire_id

module View : sig
  type t

  val create
    :  limits:Limits.t
    -> instance:Wire_id.Instance.t
    -> epoch:Wire_id.Epoch.t
    -> revision:Wire_id.Revision.t
    -> run_id:Id.Developer_run.t
    -> capture_fingerprint:Chatml_source.Digest.t
    -> (t, Error.t) result

  val instance : t -> Wire_id.Instance.t
  val epoch : t -> Wire_id.Epoch.t
  val revision : t -> Wire_id.Revision.t
  val run_id : t -> Id.Developer_run.t
  val capture_fingerprint : t -> Chatml_source.Digest.t
  val to_json : t -> Jsonaf.t
  val of_json : limits:Limits.t -> Jsonaf.t -> (t, Error.t) result
end

module Submission : sig
  type t

  module Location : sig
    type t

    (** Byte coordinates and exact source revision for one already owned
        diagnostic. This comparison DTO does not authenticate source bytes or
        grant read access. The server projects a genuine Source.Location. *)
    val create
      :  limits:Limits.t
      -> source:Chatml_source.Id.t
      -> digest:Chatml_source.Digest.t
      -> start_byte:int
      -> end_byte:int
      -> (t, Error.t) result

    val source : t -> Chatml_source.Id.t
    val digest : t -> Chatml_source.Digest.t
    val start_byte : t -> int
    val end_byte : t -> int
    val to_json : t -> Jsonaf.t
    val of_json : limits:Limits.t -> Jsonaf.t -> (t, Error.t) result
  end

  module Rendered_type : sig
    type t

    type kind =
      | Type
      | Signature
    [@@deriving equal, sexp_of]

    type truncation =
      | Node_limit
      | Edge_limit
      | Depth_limit
      | Work_limit
      | Byte_limit
    [@@deriving equal, sexp_of]

    type completeness =
      | Complete
      | Truncated of truncation
    [@@deriving equal, sexp_of]

    val create
      :  limits:Limits.t
      -> kind:kind
      -> completeness:completeness
      -> text:string
      -> (t, Error.t) result

    val kind : t -> kind
    val completeness : t -> completeness
    val text : t -> string
  end

  module Diagnostic : sig
    type t

    val create
      :  limits:Limits.t
      -> message:string
      -> location:Location.t option
      -> (t, Error.t) result

    val message : t -> string
    val location : t -> Location.t option
  end

  module Summary : sig
    (** Bounded scalar outcome, preview, diagnostic, rendered type and Task
        retention. No frozen graph, value or physical Task handle. *)
    type t

    type failure_kind =
      | Initializer
      | Runtime
      | Budget
    [@@deriving equal, sexp_of]

    type outcome =
      | Succeeded
      | Failed of failure_kind
    [@@deriving equal, sexp_of]

    type task_retention =
      | Not_a_task
      | Retained
      | Limit_reached
    [@@deriving equal, sexp_of]

    val create
      :  limits:Limits.t
      -> outcome:outcome
      -> value_preview:string option
      -> diagnostic:Diagnostic.t option
      -> expression_type:Rendered_type.t option
      -> binding_count:int
      -> task_retention:task_retention
      -> text_limited:bool
      -> (t, Error.t) result

    val outcome : t -> outcome
    val value_preview : t -> string option
    val diagnostic : t -> Diagnostic.t option
    val expression_type : t -> Rendered_type.t option
    val binding_count : t -> int
    val task_retention : t -> task_retention
    val text_limited : t -> bool
  end

  type need_more_reason =
    | Parser_eof
    | Comment_eof
    | String_eof

  type status =
    | Need_more of need_more_reason * Location.t option
    | Rejected of Diagnostic.t
    | Finished
    | Stopped of Diagnostic.t option

  type details =
    | Available
    | Output_limited

  val create
    :  limits:Limits.t
    -> status:status
    -> details:details
    -> completed_statements:int
    -> statement_count:int
    -> epoch:Wire_id.Epoch.t
    -> revision:Wire_id.Revision.t
    -> summaries:Summary.t list
    -> task_reference_count:int
    -> task_references:string list
    -> (t, Error.t) result

  val epoch : t -> Wire_id.Epoch.t
  val revision : t -> Wire_id.Revision.t
  val status : t -> status
  val details : t -> details
  val completed_statements : t -> int
  val statement_count : t -> int
  val summaries : t -> Summary.t list
  val task_reference_count : t -> int
  val task_references : t -> string list
  val to_json : t -> Jsonaf.t
  val of_json : limits:Limits.t -> Jsonaf.t -> (t, Error.t) result
end

module Type_inspection : sig
  type t

  val create
    :  limits:Limits.t
    -> revision:Wire_id.Revision.t
    -> binding_name:string option
    -> rendered:Submission.Rendered_type.t
    -> (t, Error.t) result

  val revision : t -> Wire_id.Revision.t
  val binding_name : t -> string option
  val rendered : t -> Submission.Rendered_type.t
  val to_json : t -> Jsonaf.t
  val of_json : limits:Limits.t -> Jsonaf.t -> (t, Error.t) result
end

module Reference_page : sig
  (** Encoded [items] and [next] each count toward [Limits.page], including
      the repeated cursor string. A nonempty remaining page must advance. *)
  type t

  val create
    :  limits:Limits.t
    -> items:string list
    -> next:string option
    -> total:int
    -> (t, Error.t) result

  val items : t -> string list
  val next : t -> string option
  val total : t -> int
  val to_json : t -> Jsonaf.t
  val of_json : limits:Limits.t -> Jsonaf.t -> (t, Error.t) result
end
