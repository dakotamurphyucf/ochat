type embeddings_input =
  { model : string
  ; input : string list
  }
[@@deriving jsonaf, sexp, bin_io]

(** Type definition for the response from the embeddings API. *)
type response = { data : embedding list }

(** Type definition for an individual embedding in the response. *)
and embedding =
  { embedding : float list
  ; index : int
  }
[@@deriving jsonaf, sexp, bin_io]

(** [post_openai_embeddings ~input net] makes an HTTP POST request to the OpenAI API embeddings endpoint with the given [input] and [net].

  It returns the parsed response as a [response] record. *)
val post_openai_embeddings : _ Eio.Net.t -> input:string list -> response
