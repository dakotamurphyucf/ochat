(** Vector embeddings via OpenAI.

    This module provides a thin wrapper around OpenAI’s
    [/v1/embeddings] REST endpoint.  It converts a list of UTF-8
    strings into high-dimensional vectors that can later be used for
    semantic search, clustering, or as inputs to large-language models
    such as GPT-4.

    {1 Quick-start}

    {[
      Eio_main.run @@ fun env ->
        let net   = Eio.Stdenv.net env in
        let input = [ "The quick brown fox"; "jumps over"; "the lazy dog" ] in
        let { Embeddings.data } = Embeddings.post_openai_embeddings net ~input in
        List.iteri data ~f:(fun idx e ->
          Fmt.pr "Embedding %d has length %d@." idx (List.length e.embedding))
    ]}

    The [OPENAI_API_KEY] environment variable must be set to a valid
    secret key before invoking any function in this module.

    {1 Types}

    Each embedding is returned as an [{!embedding}] record whose field
    [embedding] contains the raw vector in row-major order and [index]
    matches the corresponding input sentence.

    {1 Error handling}

    Network failures or JSON decoding errors are re-raised as
    exceptions.  Callers are encouraged to wrap the request with
    {!Io.to_res} or similar helpers if a plain-data error channel is
    preferred. *)

type embeddings_input =
  { model : string (** Identifier of the model used to generate the embeddings. *)
  ; input : string list (** Batch of input texts (UTF-8, maximum 8192 tokens each). *)
  }
[@@deriving jsonaf, sexp, bin_io]

type response =
  { data : embedding list (** Ordered list of embeddings, one per input sentence. *) }
[@@deriving jsonaf, sexp, bin_io]

and embedding =
  { embedding : float list
    (** Raw vector components.  Dimensionality depends on [model]. *)
  ; index : int (** Index of the originating text in the input batch. *)
  }
[@@deriving jsonaf, sexp, bin_io]

(** [post_openai_embeddings net ~input] sends [input] to OpenAI’s
    embeddings endpoint using the capability-safe network handle
    [net] and returns the parsed server response.

    * A maximum of 2048 input strings can be sent in one call (OpenAI
      limit).
    * When [input] is empty, the function raises [Invalid_argument].

    Example requesting a single embedding:

    {[
      let run net =
        let prompt = [ "Two roads diverged in a yellow wood." ] in
        let ({ data = [ first ] } : response) =
          Embeddings.post_openai_embeddings net ~input:prompt
        in
        Printf.printf "Vector length: %d\n" (List.length first.embedding)
    ]} *)
val post_openai_embeddings : _ Eio.Net.t -> input:string list -> response
