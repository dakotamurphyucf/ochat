(** Throttled access to the OpenAI *Embeddings* endpoint.

    The returned [embed] function may be invoked concurrently from any fibre.
    Internally, requests are serialised through a single background daemon so
    that at most [rate_per_sec] HTTP calls are issued.  Each request is
    retried up to three times with 1&nbsp;s exponential back-off before the
    error is propagated to the caller.

    {1 Guarantees}

    •  {b Concurrency-safe}: no global locks are required at call-sites.
    •  {b Rate-limited}: wall-clock throughput never exceeds
       [rate_per_sec] requests/second.
    •  {b Resilient}: transient failures are transparently retried.

    {1 Example}
{[
  let embed =
    Embed_service.create
      ~sw
      ~clock
      ~net
      ~codec:Tikitoken.Cl100k_base.codec
      ~rate_per_sec:10
      ~get_id:Digest.string
  in
  let snippets = [ ("module.ml", "let x = 1") ] in
  let (_meta, _text, vec) :: _ = embed snippets in
  Float.equal (Array.length vec.vector |> Float.of_int) 1536.
]}
*)

(** [create ~sw ~clock ~net ~codec ~rate_per_sec ~get_id] returns an
    embedding function.

    [embed snippets] maps each [(meta, text)] pair in [snippets] to a triple
    [(meta, text, vec)] where [vec] is the unnormalised embedding returned by
    {!Openai.Embeddings.post_openai_embeddings}.

    Parameters:
    •  [sw] – parent {!Eio.Switch.t} for the background daemon.
    •  [clock] – wall clock for throttling and back-off.
    •  [net] – network capability used for HTTPS calls.
    •  [codec] – {!Tikitoken.codec} used to count tokens locally.
    •  [rate_per_sec] – maximum number of requests per second (> 0).
    •  [get_id] – maps the caller-supplied metadata to a stable identifier.

    Behaviour:
    •  The call returns immediately after enqueuing; the actual HTTP request
       runs in the daemon fibre.
    •  Token length is computed locally and copied into the resulting
       {!Vector_db.Vec.t}.
    •  If the remote endpoint fails, the call is retried up to three times;
       the final failure is re-raised.

    @raise Failure if [rate_per_sec] ≤ 0
*)
val create
  :  sw:Eio.Switch.t
  -> clock:'a Eio.Time.clock
  -> net:'b Eio.Net.t
  -> codec:Tikitoken.codec
  -> rate_per_sec:int
  -> get_id:('meta -> string)
  -> ('meta * string) list
  -> ('meta * string * Vector_db.Vec.t) list
