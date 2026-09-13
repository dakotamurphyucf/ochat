(** Connection initialization and protocol capability negotiation. *)

module Implementation : sig
  type t =
    { name : string
    ; version : string
    }
  [@@deriving sexp]

  val create : name:string -> version:string -> (t, Error.t) result
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

type event_encoding =
  | Json
  | Ndjson
[@@deriving compare, equal, sexp]

module Request : sig
  type t =
    { implementation : Implementation.t
    ; protocol_min : Version.t
    ; protocol_max : Version.t
    ; features : string list
    ; event_encodings : event_encoding list
    ; max_inbound_event_bytes : int
    ; client_instance_id : string option
    }
  [@@deriving sexp]

  val create
    :  implementation:Implementation.t
    -> protocol_min:Version.t
    -> protocol_max:Version.t
    -> features:string list
    -> event_encodings:event_encoding list
    -> max_inbound_event_bytes:int
    -> ?client_instance_id:string
    -> unit
    -> (t, Error.t) result

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Limits : sig
  type t =
    { max_request_bytes : int
    ; max_event_bytes : int
    ; max_page_size : int
    ; max_attachments_per_connection : int
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Event_retention : sig
  type t =
    { minimum_age_ms : int
    ; maximum_events : int
    ; oldest_replayable_sequence : int64 option
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Timing : sig
  type t =
    { heartbeat_interval_ms : int
    ; owner_lease_duration_ms : int
    ; owner_renew_after_ms : int
    ; disconnect_grace_default_ms : int
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Response : sig
  type t =
    { protocol_name : string
    ; selected_version : Version.t
    ; implementation : Implementation.t
    ; server_id : Id.Server.t
    ; enabled_features : string list
    ; extensions : Extension_capabilities.t option [@sexp.option]
    ; principal : Principal.t
    ; limits : Limits.t
    ; event_retention : Event_retention.t
    ; timing : Timing.t
    ; server_time : Timestamp.t
    }
  [@@deriving sexp]

  val create
    :  protocol_name:string
    -> selected_version:Version.t
    -> implementation:Implementation.t
    -> server_id:Id.Server.t
    -> enabled_features:string list
    -> extensions:Extension_capabilities.t option
    -> principal:Principal.t
    -> limits:Limits.t
    -> event_retention:Event_retention.t
    -> timing:Timing.t
    -> server_time:Timestamp.t
    -> (t, Error.t) result

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end
