(** Convert a web page to Markdown and expose the operation as a GPT-tool.

    {1 Overview}

    This module couples the declarative specification
    {!Definitions.Webpage_to_markdown} with a concrete implementation that
    fetches the page, converts it to Markdown, and caches the result.  The
    single entry-point {!register} produces a {!Gpt_function.t} value ready to
    be passed to {!Gpt_function.functions} (and ultimately to
    [Openai.Completions.post_chat_completion]).

    {1 Behaviour}

    • Downloads the resource identified by the [url] argument supplied by the
      model.  GitHub *blob* URLs are handled specially and resolved to their
      raw counterparts.
    • Converts HTML (or source code) to GitHub-flavoured Markdown using
      {!Webpage_markdown.Driver.fetch_and_convert}.
    • Caches up to 128 previously seen URLs in a TTL/LRU cache for 5 minutes
      to avoid redundant network traffic.

    {1 Example}

    {[
      let env = Eio_main.run @@ fun env ->
        let net = Eio.Stdenv.net env in
        let dir = Eio.Path.cwd (Eio.Stdenv.fs env) in
        let tool = Webpage_markdown.Tool.register ~env ~dir ~net in

        (* integrate the tool with other functions *)
        let tools_json, dispatch = Gpt_function.functions [ tool ] in
        (* send [tools_json] to OpenAI and later use [dispatch] to satisfy the
           callback *)
    ]}
*)

(** [register ~env ~dir ~net] returns the concrete implementation of the
    "webpage_to_markdown" GPT-tool.

    Arguments:
    • [env] – Eio standard environment used for spawning helper processes and
      measuring timeouts.
    • [dir] – currently unused; present for interface consistency across
      tool factories.
    • [net] – network capability required by {!Webpage_markdown.Fetch}.

    The function wraps the real work in a small in-memory cache.  Re-invoking
    the tool on the same URL within five minutes returns the cached Markdown
    rather than re-downloading the page.

    Failures are surfaced to the model as a string starting with
    ["Error fetching"]. *)
val register
  :  env:Eio_unix.Stdenv.base
  -> dir:_ Eio.Path.t
  -> net:_ Eio.Net.t
  -> Gpt_function.t
