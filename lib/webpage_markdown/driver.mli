(** Lightweight Markdown value. *)
module Markdown : sig
  (** Opaque Markdown document.

      The concrete representation is [(string)], but callers should rely only
      on the exposed interface.  The type derives the usual Core kernels for
      serialization and container use. *)
  type t [@@deriving sexp, bin_io, hash, compare]

  (** [to_string md] returns the raw Markdown source contained in [md] without
      any transformation.  The result is suitable for writing to disk or for
      feeding into Markdown renderers such as
      {{:https://github.com/ocaml/omd}Omd}. *)
  val to_string : t -> string
end

(** [fetch_and_convert ~env ~net url] downloads the resource located at [url]
    and returns it as Markdown.

    Behaviour rules:
    • For *GitHub* URLs of the form
      [github.com/owner/repo/blob/branch/path#Lx-Ly] the function shortcuts to
      the Raw-content endpoint, extracts the requested line range (if any) and
      wraps the result in a fenced code-block whose language is inferred from
      the file extension.  Markdown sources ([*.md], [*.markdown]…) are
      returned verbatim.

    • All other schemes are fetched with {!Webpage_markdown.Fetch.get}.  The
      response body is assumed to be HTML and is converted to Markdown using
      {!Webpage_markdown.Html_to_md} followed by {!Webpage_markdown.Md_render}.

    • When HTML parsing fails or produces an empty string, a last-chance
      fallback spawns the external [chrome-dump] CLI (headless Chrome) and
      retries the conversion.  The call is wrapped in a 60-second timeout so
      the function never blocks indefinitely.

    Size limits and content-type checks are enforced upstream by
    {!Webpage_markdown.Fetch.get}.  Therefore [fetch_and_convert] never raises
    on network errors — instead it returns a one-line Markdown string with the
    diagnostic message.

    Example – mirroring *OCaml.org* home-page:
    {[
      Eio_main.run @@ fun env ->
      let markdown =
        Webpage_markdown.Driver.fetch_and_convert
          ~env
          ~net:(Eio.Stdenv.net env)
          "https://ocaml.org"
      in
      Eio.Path.save ~create:(`Or_truncate 0o644)
        (Eio.Stdenv.cwd env / "ocaml_org.md")
        (Webpage_markdown.Driver.Markdown.to_string markdown)
    ]}
*)
val fetch_and_convert
  :  env:Eio_unix.Stdenv.base
  -> net:_ Eio.Net.t
  -> string
  -> Markdown.t

(** [convert_html_file path] loads the file at [path] (assumed to contain raw
    HTML) and converts it to Markdown using the exact same pipeline as
    {!fetch_and_convert}.  This is useful for offline processing or unit
    testing. *)
val convert_html_file : _ Eio.Path.t -> Markdown.t
