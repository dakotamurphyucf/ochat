(** Blocking document-retrieval helpers.

    The helpers in this module are *synchronous* by design and therefore
    appropriate only for small inputs (≈ \< 1 MiB).  They are primarily
    used by the ChatMarkdown → OpenAI conversion pipeline when a prompt
    references an external file or a remote URL.

    {1 API at a glance}

    {v
      (* Raw content *)
      let body   = Fetch.get      ~ctx "README.md"            ~is_local:true  in
      let remote = Fetch.get      ~ctx "https://ocaml.org"     ~is_local:false in

      (* Human-readable text extracted from HTML *)
      let text   = Fetch.get_html ~ctx "docs.html"             ~is_local:true  in

      (* Utility for embedding multi-line strings into ChatMarkdown *)
      let indented = Fetch.tab_on_newline "First\nSecond";
    v}

    The implementation relies on {!Io}, {!Ezgzip},
    {!module:lambdasoup.Soup} and the network stack provided by {!Ctx}.
    Each helper raises on IO failure and never follows redirects.

    @param ctx Immutable execution context providing [`net`], [`dir`] and
               [`cwd`] capabilities. *)

(** [get ~ctx url ~is_local] returns the raw body located at [url].

    • If [is_local] = [true] the helper treats [url] as a path on the
      host file-system and tries to resolve it in two steps:
      {ol
      {- first against {!Ctx.dir};}
      {- then – only if the path is still unresolved and {b relative} –
         against the caller’s current working directory ([ctx#cwd]).}}

    • If [is_local] = [false] a blocking HTTP GET is issued using the
      [`net`] object stored in [ctx].  Only status code 200 is
      accepted; every other response triggers an exception.

    The helper never follows redirects and performs no gzip
    decompression.  Use {!get_html} when you expect HTML. *)
val get
  :  ctx:
       < cwd : Eio.Fs.dir_ty Eio.Path.t
       ; fs : Eio.Fs.dir_ty Eio.Path.t
       ; net : [> [> `Generic ] Eio.Net.ty ] Eio.Resource.t
       ; .. >
         Ctx.t
  -> string
  -> is_local:bool
  -> string

(** Same as {!get} but sanitises HTML.

    Markup is stripped and consecutive whitespace collapsed, leaving
    only visible text separated by newlines.  Before parsing, a
    best-effort gzip decompression step is attempted because many hosts
    deliver compressed payloads even when the client did not advertise
    support.

    Use this helper whenever you need {e human-readable} text rather
    than the raw HTML source. *)
val get_html
  :  ctx:
       < cwd : Eio.Fs.dir_ty Eio.Path.t
       ; fs : Eio.Fs.dir_ty Eio.Path.t
       ; net : [> [> `Generic ] Eio.Net.ty ] Eio.Resource.t
       ; .. >
         Ctx.t
  -> string
  -> is_local:bool
  -> string

(** [tab_on_newline s] inserts two tab characters ("\t\t") every time a
    newline character is encountered in [s].  The transformation is
    purely cosmetic and is useful when embedding multi-line strings into
    indented ChatMarkdown blocks (for instance inside a raw
    `<assistant>` payload). *)
val tab_on_newline : string -> string
