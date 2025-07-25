open Core

(** Document fetching helpers.

    The helpers in this module turn a URL – either local (relative path
    on the host filesystem) or remote (*http://*, *https://*) – into the
    desired textual representation.  The functions are intentionally
    minimal and synchronous because they are only used from short-lived
    tasks inside a completion request.

    • {!get} returns the content *as-is*.
    • {!get_html} additionally strips HTML tags and whitespace, producing
      a plain-text version that is easier for large-language models to
      digest.

    The implementation relies on {!Io}, {!Soup}, {!Ezgzip} and the Eio
    network stack that lives in {!Ctx}.  Performance is adequate for
    small (<1 MiB) resources; larger downloads should be streamed or
    paginated instead.
*)

(* Internal helpers --------------------------------------------------- *)

(** [clean_html raw] removes markup and compresses whitespace from an HTML
    document held in [raw].  The returned string is plain UTF-8 text with
    newline separators between logical blocks.

    A best-effort decompression step is attempted first because a number
    of web servers deliver gzip content even when the caller did not
    explicitly request it (this is especially true for GitHub raw
    endpoints).

    The function keeps the transformation lightweight on purpose – no
    entity decoding or CSS/JS removal beyond what {!Soup.texts} already
    performs.  Use a dedicated HTML sanitizer if stronger guarantees are
    required.
*)
let clean_html raw =
  let decompressed = Option.value ~default:raw (Result.ok (Ezgzip.decompress raw)) in
  let soup = Soup.parse decompressed in
  soup
  |> Soup.texts
  |> List.map ~f:String.strip
  |> List.filter ~f:(Fn.non String.is_empty)
  |> String.concat ~sep:"\n"
;;

(** [tab_on_newline input] inserts two tab characters after every newline
    of [input].  The helper is used when embedding multi-line strings into
    ChatMarkdown blocks where indentation matters (e.g. `<assistant>` raw
    payload).  No other transformation is performed.  *)
let tab_on_newline (input : string) : string =
  let buffer = Buffer.create (String.length input) in
  String.iter
    ~f:(fun c ->
      Buffer.add_char buffer c;
      if Char.(c = '\n')
      then (
        Buffer.add_char buffer '\t';
        Buffer.add_char buffer '\t'))
    input;
  Buffer.contents buffer
;;

(** [get_remote ?gzip ~net url] performs a blocking HTTP GET and returns the
    response body.  Header [Accept: */*] is always sent; when [gzip] is
    [true] the function advertises gzip support so that servers may
    compress their payload.  No redirects are followed.  Raise on network
    errors. *)
let get_remote ?(gzip = false) ~net url =
  let host = Io.Net.get_host url
  and path = Io.Net.get_path url in
  let headers =
    Http.Header.of_list
      (if gzip
       then [ "Accept", "*/*"; "Accept-Encoding", "gzip" ]
       else [ "Accept", "*/*" ])
  in
  Io.Net.get Io.Net.Default ~net ~host ~headers path
;;

(* Shared implementation --------------------------------------------- *)

(** Shared implementation for {!get} and {!get_html}.  [url] is resolved
    against the call-site as either a local file or an HTTP(S) resource
    depending on the [is_local] flag that originates from the parser.

    If [cleanup_html] is [true], the function assumes the server returned
    (X)HTML and sanitises the document with {!clean_html}. *)
let get_impl ~(ctx : _ Ctx.t) url ~is_local ~cleanup_html =
  if is_local
  then Io.load_doc ~dir:(Ctx.dir ctx) url
  else (
    let net = Ctx.net ctx in
    let raw = get_remote ~net url in
    if cleanup_html then clean_html raw else raw)
;;

(* Public helpers ----------------------------------------------------- *)
(** [get ~ctx url ~is_local] returns the raw content located at [url].
    The resource is fetched locally when [is_local] is [true], otherwise
    an HTTP request is issued using the network stack from [ctx]. *)
let get ~ctx url ~is_local = get_impl ~ctx url ~is_local ~cleanup_html:false

(** [get_html ~ctx url ~is_local] is like {!get} but applies
    {!clean_html} to the result so that only human-readable text is kept. *)
let get_html ~ctx url ~is_local = get_impl ~ctx url ~is_local ~cleanup_html:true
