open! Core

type protocol_error = Protocol_error.t

(** Nonsecret provider administration DTOs. No filesystem paths, credential bytes,
    arbitrary environment names, or inferred login authority occur in requests. *)
module Profile_id : sig
  type t [@@deriving compare, equal, sexp]

  val of_string : string -> (t, protocol_error) result
  val to_string : t -> string
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, protocol_error) result
end

module Source_id : module type of Profile_id
module Flow_id : module type of Profile_id
module Revision : module type of Profile_id

module Limits : sig
  type t

  val create
    :  max_flows:int
    -> max_profiles:int
    -> max_flow_seconds:int
    -> (t, protocol_error) result

  val default : t
  val max_flows : t -> int
  val max_profiles : t -> int
  val max_flow_seconds : t -> int
end

module Operation : sig
  type t =
    | Setup
    | Status
    | Login
    | Challenge
    | Cancel
    | Logout
    | Select
    | Configure_environment
  [@@deriving equal, sexp]
end

module Error : sig
  type t =
    | Denied
    | Missing_profile
    | Invalid_request
    | Busy
    | Closed
    | Flow_expired
    | Flow_interrupted
    | Challenge_unavailable
    | Submission_uncertain
    | Network
    | Account_denied
    | Model_denied
    | Store_unavailable
    | Unsupported
  [@@deriving equal, sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, protocol_error) result
end

module Login_mode : sig
  type t =
    | Browser
    | Device
  [@@deriving equal, sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, protocol_error) result
end

module Flow_ref : sig
  type t =
    { server_id : Id.Server.t
    ; profile : Profile_id.t
    ; flow_id : Flow_id.t
    ; expires_at : Timestamp.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, protocol_error) result
end

module Flow_result : sig
  type phase =
    | Pending
    | Completed
    | Failed of Error.t
    | Cancelled
    | Interrupted
    | Expired
  [@@deriving equal, sexp]

  type t =
    { flow : Flow_ref.t
    ; phase : phase
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, protocol_error) result
end

module Private_challenge : sig
  (** Secret-bearing owner-only live challenge. Debug sexp is always redacted;
      sexp decoding is forbidden. Never store in receipts, events, audit or status. *)
  type t [@@deriving sexp]

  val browser : authorization_uri:Uri.t -> (t, protocol_error) result
  val device : verification_uri:Uri.t -> user_code:string -> (t, protocol_error) result
  val with_browser_uri : t -> f:(Uri.t -> 'a) -> 'a option

  val with_device_prompt
    :  t
    -> f:(verification_uri:Uri.t -> user_code:string -> 'a)
    -> 'a option

  module Authorized_transport : sig
    (** Only after current scope AND exact flow ownership/expiry authorization. *)
    val to_json : t -> Jsonaf.t

    val of_json : Jsonaf.t -> (t, protocol_error) result
  end
end

module Setup_request : sig
  type t = { idempotency_key : Idempotency_key.t } [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, protocol_error) result
end

(** Setup revision is the nonsecret host registry incarnation, not the default
    selection revision. Selection CAS comes only from Status_result.selection. *)
module Setup_result : sig
  type t =
    { server_id : Id.Server.t
    ; revision : Revision.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, protocol_error) result
end

module Status_request : sig
  type t = { profile : Profile_id.t option } [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, protocol_error) result
end

module Selection_result : sig
  type t =
    { profile : Profile_id.t
    ; revision : Revision.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, protocol_error) result
end

module Status_result : sig
  type availability =
    | Missing
    | Configured
    | Disabled
    | Renewal_required
    | Renewal_uncertain
    | Secret_unavailable
    | Store_unavailable
  [@@deriving equal, sexp]

  type failure =
    | Network
    | Account_denied
    | Model_denied
    | Submission_uncertain
  [@@deriving equal, sexp]

  type profile =
    { profile : Profile_id.t
    ; account : string option
    ; availability : availability
    ; last_failure : failure option
    ; auth_epoch : int64 option
    ; credential_revision : Revision.t option
    }
  [@@deriving sexp]

  type t =
    { server_id : Id.Server.t
    ; setup_required : bool
    ; profiles : profile list
    ; flows : Flow_result.t list
    ; selection : Selection_result.t option
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, protocol_error) result
end

module Login_request : sig
  type t =
    { profile : Profile_id.t
    ; mode : Login_mode.t
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, protocol_error) result
end

module Challenge_request : sig
  type t = { flow : Flow_ref.t } [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, protocol_error) result
end

module Cancel_request : sig
  type t =
    { flow : Flow_ref.t
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, protocol_error) result
end

module Logout_request : sig
  type t =
    { profile : Profile_id.t
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, protocol_error) result
end

module Logout_result : sig
  type drain =
    | Drained
    | Pending
  [@@deriving equal, sexp]

  type t =
    { profile : Profile_id.t
    ; auth_epoch : int64
    ; drain : drain
    ; cleanup_pending : bool
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, protocol_error) result
end

module Select_request : sig
  type t =
    { profile : Profile_id.t
    ; expected_revision : Revision.t
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, protocol_error) result
end

module Environment_request : sig
  type t =
    { profile : Profile_id.t
    ; source : Source_id.t
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, protocol_error) result
end

module Configuration_result : sig
  type t =
    { profile : Profile_id.t
    ; auth_epoch : int64
    ; revision : Revision.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, protocol_error) result
end
