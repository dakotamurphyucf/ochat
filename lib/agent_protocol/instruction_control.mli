open! Core

(** Transport-neutral consistency DATA. No type in this module is source,
    native execution, operator, capture-use or writer authority. No bodies,
    host paths, metadata descriptions or unrelated retained captures appear. *)
module Root_id : sig
  type t [@@deriving equal, sexp_of]

  (** Same bounded administrative spelling as Instruction_content.Root_id.
      Protocol cannot depend on that library: it depends on Agent_store.
      The host validates the conversion again; labels confer no root grant. *)
  val of_string : string -> (t, Protocol_error.t) result

  val to_string : t -> string
end

module Member : sig
  type t = private
    { root : Root_id.t
    ; member : Source.Portable_path.t
    ; digest : Chatml_source.Digest.t
    }
  [@@deriving equal, sexp_of]

  val create
    :  root:Root_id.t
    -> member:Source.Portable_path.t
    -> digest:Chatml_source.Digest.t
    -> t
end

module Selection : sig
  type t =
    | Project of
        { root : Root_id.t
        ; scope : Source.Portable_path.t option
        }
    | Skills of Root_id.t list
  [@@deriving sexp_of]

  (** None scope means configured root, not ambient cwd. *)
  val validate : t -> (unit, Protocol_error.t) result
end

module Source_request : sig
  type t [@@deriving sexp_of]

  (** Ordered expected manifest; no contents or asserted host completeness.
      At most 64 members; project may be empty, skills must be nonempty. *)
  val create
    :  selection:Selection.t
    -> members:Member.t list
    -> (t, Protocol_error.t) result

  val selection : t -> Selection.t
  val members : t -> Member.t list
end

module Capture : sig
  type t = private
    { source : Source_request.t
    ; capture_id : Chatml_source.Digest.t
    }
  [@@deriving sexp_of]

  (** Host projects an actual Port.capture result. A wire-decoded instance is
      only a future comparison request; it is never Port.Captured authority. *)
  val create : source:Source_request.t -> capture_id:Chatml_source.Digest.t -> t
end

module Action : sig
  type t =
    | Inspect
    | Refresh
    | Capture
    | Activate
    | Remove
  [@@deriving equal, sexp_of]
end

module View : sig
  type t = private
    { session_id : Id.Session.t
    ; generation : int
    ; summary : Instruction_status.t
    ; project : Source_request.t option
    ; skills : Member.t list
    ; permitted_actions : Action.t list
    }
  [@@deriving sexp_of]

  (** Stored selection, not provider activation. Selected identities require
      current transcript/session visibility and current grants for their roots.
      Permitted actions are a checked present observation, not future authority
      or a capacity/CAS promise. Mutating actions require the optional actual
      connection attachment to pass the Actor's existing writer check. *)
  val create
    :  session_id:Id.Session.t
    -> generation:int
    -> summary:Instruction_status.t
    -> project:Source_request.t option
    -> skills:Member.t list
    -> permitted_actions:Action.t list
    -> (t, Protocol_error.t) result
end

module Discovery : sig
  type t [@@deriving sexp_of]

  module Issue : sig
    type code =
      | Skipped
      | Unavailable
    [@@deriving equal, sexp_of]

    type t =
      { root : Root_id.t
      ; member : Source.Portable_path.t
      ; code : code
      }
    [@@deriving sexp_of]
  end

  (** Only granted administrative root IDs and qualified portable member
      manifests/digests. Reuse existing bounded discovery, not a filesystem scan
      performed by a client. Issues have a fixed safe tag and portable identity,
      no raw error/details or host path. *)
  val roots : t -> Root_id.t list

  val members : t -> Member.t list
  val issues : t -> Issue.t list

  val create
    :  roots:Root_id.t list
    -> members:Member.t list
    -> issues:Issue.t list
    -> (t, Protocol_error.t) result
end

module Receipt : sig
  type t = private
    { principal_id : Id.Principal.t
    ; key : Idempotency_key.t
    ; request_sha256 : string
    ; session_id : Id.Session.t
    ; generation : int
    ; revision : int64
    ; accepted_at : Timestamp.t
    }
  [@@deriving equal, sexp_of]

  (** Exact projection of Instruction_command.Receipt, with validated decoding.
      Not another receipt store or generic command-audit record. *)
  val create
    :  principal_id:Id.Principal.t
    -> key:Idempotency_key.t
    -> request_sha256:string
    -> session_id:Id.Session.t
    -> generation:int
    -> revision:int64
    -> accepted_at:Timestamp.t
    -> (t, Protocol_error.t) result
end

module Request : sig
  module Inspect : sig
    type t =
      { session_id : Id.Session.t
      ; attachment_id : Id.Attachment.t option
      }
    [@@deriving sexp_of]
  end

  module Refresh : sig
    type t =
      { session_id : Id.Session.t
      ; selection : Selection.t option
      }
    [@@deriving sexp_of]
  end

  module Capture : sig
    type t =
      { session_id : Id.Session.t
      ; source : Source_request.t
      }
    [@@deriving sexp_of]
  end

  module Activate : sig
    type t =
      { session_id : Id.Session.t
      ; attachment_id : Id.Attachment.t
      ; expected_generation : int
      ; expected_revision : int64
      ; source : Source_request.t
      ; capture_id : Chatml_source.Digest.t
      ; key : Idempotency_key.t
      }
    [@@deriving sexp_of]
  end

  module Remove : sig
    type change =
      | All
      | Project
      | Skills of Member.t list
    [@@deriving sexp_of]

    type t =
      { session_id : Id.Session.t
      ; attachment_id : Id.Attachment.t
      ; expected_generation : int
      ; expected_revision : int64
      ; change : change
      ; key : Idempotency_key.t
      }
    [@@deriving sexp_of]
  end

  type t =
    | Inspect of Inspect.t
    | Refresh of Refresh.t
    | Capture of Capture.t
    | Activate of Activate.t
    | Remove of Remove.t
  [@@deriving sexp_of]

  (** Closed, fully bounded JSON admission before field/list projection;
      unknown/duplicate fields and malformed IDs/digests/revisions reject.
      Parameter bound is the accepted 128 KiB native profile. *)
  val methods : string list

  val session_id : t -> Id.Session.t
  val method_name : t -> string
  val params : t -> Jsonaf.t

  val of_method_and_params
    :  method_:string
    -> params:Jsonaf.t
    -> (t, Protocol_error.t) result

  (** Tagged codec is for ordinary tooling/data, not a second dispatcher. *)
  val to_json : t -> Jsonaf.t

  val of_json : Jsonaf.t -> (t, Protocol_error.t) result
end

module Result : sig
  type t =
    | Inspect of View.t
    | Refresh of Discovery.t
    | Capture of Capture.t
    | Activate of Receipt.t
    | Remove of Receipt.t
  [@@deriving sexp_of]

  val method_name : t -> string
  val payload : t -> Jsonaf.t

  (** Complete result admission is capped at 512 KiB to cover 64 bounded
      1024-byte portable paths even with JSON escaping, plus safe identities.
      No input bytes or provider fields are included. *)
  val of_method_and_result
    :  method_:string
    -> result:Jsonaf.t
    -> (t, Protocol_error.t) result

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Protocol_error.t) result
end
