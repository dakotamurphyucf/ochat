open Core

module Pack : sig
  type t =
    | Stock
    | Empty
    | Malformed
end

module Shallow : sig
  type t =
    | Stock
    | Excessive
end

(** Finite independent protocol-v0 response family. Preserve the stock advert and
    depth negotiation; selected final pack/shallow payloads are hand framed. *)
type t

val create : pin:Chatml_git_host.Pin.t -> pack:Pack.t -> shallow:Shallow.t -> t
val supplied_pack : t -> bool
val supplied_shallow : t -> bool

val response
  :  t
  -> stock:(Https_fixture.Request.t -> Https_fixture.Response.t)
  -> Https_fixture.Request.t
  -> Https_fixture.Response.t
