open! Core
module P = Agent_protocol
module DTO = P.Provider_operator

type route =
  | Http
  | Socket
  | Stdio
[@@deriving sexp_of]

type role =
  | Owner
  | Renewed_owner
  | Foreign
  | Viewer

type t

module Client : sig
  type t

  val request : t -> P.Command.t -> (P.Public.Result.t, P.Error.t) result
  val close : t -> unit
end

(** Uses the actual production runtime-host factory and daemon; only the OAuth
    network boundary is scripted. Each route uses its real server/framing. *)
val with_fixture : ?flow_seconds:int -> route -> (t -> unit) -> unit

val connect : t -> role -> Client.t
val await_poll : t -> unit
val release_poll : t -> unit
val expire_owner : t -> unit
val expire_flow : t -> unit
val poll_exits : t -> int
val exchanges : t -> int
val login_starts : t -> int
val assert_inference_unavailable : t -> unit
val wait_terminal : t -> Client.t -> DTO.Flow_ref.t -> DTO.Flow_result.t
val profile : DTO.Profile_id.t
val key : string -> P.Idempotency_key.t
