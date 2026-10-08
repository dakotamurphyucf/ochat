open Core

(** Current locally owned Responses protocol boundary. Transport drivers should
    encode [Request.t] and consume these captured responses/events; the older
    [Responses] runtime records do not retain unknown fields. This module neither
    authenticates a caller nor grants endpoint/model/tool capabilities. *)
module Request = Responses_request

module Wire = Responses_wire

type failure =
  | Framing of string
  | Decode of
      { raw : Jsonaf.t
      ; error : Wire.Decode_error.t
      }
  | Protocol of
      { event : Wire.Event.t option
      ; error : Wire.Tracker.error
      }

val protocol_violation
  :  stage:Inference.Observation.Diagnostic.Protocol_violation.stage
  -> failure
  -> Inference.Observation.Diagnostic.Protocol_violation.t

(** Failed known objects remain available in [Decode.raw]. Captures contain
    sensitive conversation data and must not be logged automatically. *)
val decode_response
  :  Jsonaf.t
  -> origin:Wire.Origin.t
  -> (Wire.Response.t, failure) Result.t

module Stream : sig
  type t

  type update =
    { event : Wire.Event.t
    ; disposition : Wire.Tracker.disposition
    ; newly_finalized : (int * Wire.Item.t) list
    }

  (** One owner per inference attempt. [newly_finalized] is validation evidence,
      not authorization to run tools. Host admission still applies. *)
  val create : ?max_frame_bytes:int -> Wire.Origin.t -> t Or_error.t

  (** Retains only finite redacted detail from this parser's original rejected
      event. No private failure payload is serialized. *)
  val protocol_violation
    :  t
    -> stage:Inference.Observation.Diagnostic.Protocol_violation.stage
    -> failure
    -> Inference.Observation.Diagnostic.Protocol_violation.t

  (** Consume newline-stripped SSE lines. Frames may span multiple lines.
      Errors poison the stream and retain the offending JSON/event where known.
      Unknown valid events are delivered with their raw fields intact.
      [[DONE]] requires a semantic terminal; it cannot manufacture success. *)
  val feed_line : t -> string -> (update option, failure) Result.t

  (** No terminal at EOF is [Protocol Truncated]. Incomplete, refusal and
      provider failure remain inspectable completion values. Repeated finish
      returns the same outcome. No transport retry or fallback is performed. *)
  val finish : t -> (Wire.Tracker.completion, failure) Result.t
end
