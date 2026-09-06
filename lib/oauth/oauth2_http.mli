(** [protect f] turns operational exceptions into [Error] and preserves Eio
    cancellation. *)
val protect : (unit -> ('a, string) result) -> ('a, string) result

(** [get_json ~env ~sw url] decodes a successful HTTP response. Transport,
    non-2xx status, and malformed JSON failures return [Error]. Cancellation
    propagates. *)
val get_json
  :  env:Eio_unix.Stdenv.base
  -> sw:Eio.Switch.t
  -> string
  -> (Jsonaf.t, string) result

(** [post_form ~env ~sw url params] posts URL-encoded fields with the same
    error and cancellation contract as [get_json]. *)
val post_form
  :  env:Eio_unix.Stdenv.base
  -> sw:Eio.Switch.t
  -> string
  -> (string * string) list
  -> (Jsonaf.t, string) result

(** [post_json ~env ~sw url json] posts JSON with the same error and cancellation
    contract as [get_json]. *)
val post_json
  :  env:Eio_unix.Stdenv.base
  -> sw:Eio.Switch.t
  -> string
  -> Jsonaf.t
  -> (Jsonaf.t, string) result
