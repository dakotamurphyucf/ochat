(** Fetch a type-ahead completion suffix.

    The returned string is the exact text that should be inserted at the cursor
    position in the current draft buffer. It must be a suffix (i.e. it must not
    repeat the draft prefix before the cursor).

    The provider is designed for low-latency interactive use:
    {ul
    {- it caps output tokens;}
    {- it disables tools;}
    {- it encodes the cursor position explicitly via a marker inserted into the
       draft excerpt; and}
    {- it limits the excerpted draft/history context.}}

    Cancellation: the request runs under [sw]. Failing [sw] will cancel the
    request, typically raising the exception used with [Switch.fail].

    The returned text is sanitised with {!Chat_tui.Util.sanitize} using
    [~strip:false] so it is safe to render and insert.

    When credentials are missing (no [OPENAI_API_KEY]), returns the empty
    string.

    Cursor semantics: [cursor] is a byte index into [draft]. Out-of-range
    cursors are clamped. *)
val complete_suffix
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> dir:Eio.Fs.dir_ty Eio.Path.t
  -> cfg:Chat_response.Config.t
  -> history_items:Openai.Responses.Item.t list
  -> draft:string
  -> cursor:int
  -> string
