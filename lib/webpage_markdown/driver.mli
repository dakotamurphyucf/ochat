module Markdown : sig
  type t [@@deriving sexp, bin_io, hash, compare]

  val to_string : t -> string
end

(** [fetch_and_convert ~net url] fetches the content at [url] and converts it to
      a Markdown document. It uses the provided network interface to perform the
      HTTP request. *)
val fetch_and_convert
  :  env:Eio_unix.Stdenv.base
  -> net:_ Eio.Net.t
  -> string
  -> Markdown.t

(** [convert_html_file path] reads an HTML file from [path] and converts it to
          a Markdown document. *)
val convert_html_file : _ Eio.Path.t -> Markdown.t
