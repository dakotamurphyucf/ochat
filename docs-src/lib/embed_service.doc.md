# Embed_service: batched embedding requests

`Embed_service.create` returns a function that accepts a list of metadata/text
pairs and waits for the corresponding vectors. Calls can come from concurrent
Eio fibers under the owning switch. This is not a cross-domain thread-safety
guarantee.

## Contract

```ocaml
val create :
  sw:Eio.Switch.t ->
  clock:_ Eio.Time.clock ->
  net:_ Eio.Net.t ->
  codec:Tikitoken.codec ->
  rate_per_sec:int ->
  get_id:('meta -> string) ->
  ('meta * string) list -> ('meta * string * Vector_db.Vec.t) list
```

Supply a positive `rate_per_sec`. The caller batches and bounds texts for the
chosen model; the service does not split oversized inputs.

The queue holds up to 100 requests. A dispatcher spaces initial request starts
according to `rate_per_sec` and forks each request in the supplied switch, so
requests may overlap. The limit belongs to this service instance, not all
processes or all instances. Retries run inside the request fiber and do not
pass back through the dispatcher throttle.

A failed request is retried up to three times after its first attempt (four
attempts total), with a one-second delay. The catch covers raised exceptions,
not a classifier restricted to HTTP 5xx errors. Exhausted errors are propagated
to the waiting caller.

Response indices associate each embedding with its original metadata and text.
`get_id` supplies the vector record ID; token counting fills its `len` metadata,
not the embedding dimension. Embedding dimensions come from the provider or stub.

## Example

This function requires an existing Eio environment, switch, and tokenizer codec:

```ocaml
let embed_text ~sw ~env ~codec ~id ~text =
  let embed =
    Embed_service.create
      ~sw
      ~clock:(Eio.Stdenv.clock env)
      ~net:(Eio.Stdenv.net env)
      ~codec
      ~rate_per_sec:10
      ~get_id:(fun id -> id)
  in
  embed [ id, text ]
```

The service has no persistent embedding cache. See
[embedding configuration](../guide/search-and-indexing.md#embedding-configuration)
for real versus stub vectors and model compatibility, and the
[source](../../lib/embed_service.ml) for concurrency and retry details.
