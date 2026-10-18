open! Core

(** Pure absolute deadline and remaining execution ceiling for one decoded
    developer background request. This carries no Claim, source or Store
    authority. The caller supplies its actual saved job and Root clock; the
    physical Claim path still re-decodes and verifies the payload before any
    effect. Tool and Script use this same arithmetic so an expired request
    cannot initialize a selected issuer merely to discover expiry later. *)
type t

type disposition =
  | Expired
  | Active of Chatml_execution.limits

val create
  :  now:Agent_protocol.Timestamp.t
  -> job:Agent_protocol.Developer_job.t
  -> request:Chat_response.Background_request.t
  -> (t, Agent_protocol.Error.t) result

val deadline : t -> Agent_protocol.Timestamp.t
val disposition : t -> disposition
