open! Core

(** [get ~net url] fetches the raw HTML of [url].
    - Returns [Ok html] when the document was successfully retrieved and
      decoded (gzip/deflate supported).
    - Returns [Error msg] on network failure, wrong content-type or when the
      document exceeds the 1 MB safety limit. *)
val get : net:_ Eio.Net.t -> string -> (string, string) Result.t
