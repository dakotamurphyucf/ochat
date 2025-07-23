(** A throttled wrapper around the OpenAI Embeddings endpoint.

    The returned [embed] function can be called concurrently from any fibre.
    Internally, all requests are serialised and rate-limited so that no more
    than [rate_per_sec] HTTP calls are issued.  The function retries transient
    failures up to three times with exponential back-off. *)

val create
  :  sw:Eio.Switch.t
  -> clock:'a Eio.Time.clock
  -> net:'b Eio.Net.t
  -> codec:Tikitoken.codec
  -> rate_per_sec:int
  -> get_id:('meta -> string)
  -> ('meta * string) list
  -> ('meta * string * Vector_db.Vec.t) list
[@@ocaml.warning "-32"]
