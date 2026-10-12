open Core

(** Independently framed bounded localhost HTTP fixture over real TLS. The test
    certificate is deliberately explicit; production TLS policy is unchanged. *)
module Request : sig
  type t

  val method_ : t -> string
  val target : t -> string
  val header : t -> string -> string option
  val body : t -> string
end

module Response : sig
  type t =
    { status : string
    ; headers : (string * string) list
    ; body : string
    }
end

val start : _ Eio.Net.t -> sw:Eio.Switch.t -> handle:(Request.t -> Response.t) -> string
