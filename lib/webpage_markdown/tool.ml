(** Webpage-to-Markdown tool implementation.

    {1 Synopsis}

    The module exports a single helper {!register} that turns the declarative
    description {!Definitions.Webpage_to_markdown} into a runnable
    {!Ochat_function.t}.  The implementation:

    • Uses {!Webpage_markdown.Driver.fetch_and_convert} to download a web page
      (or raw GitHub blob) and convert it to Markdown.
    • Stores up to 128 recent results in a TTL-augmented LRU cache
      ({!Ttl_lru_cache}) so that repeated calls for the same URL within
      5 minutes are answered instantly.
    • Reports exceptions as a human-readable string so the calling model can
      surface the error to the user.
*)

open Core

module Url_key = struct
  type t = string [@@deriving sexp, bin_io, hash, compare]

  let invariant (_ : t) = ()
end

module Cache = Ttl_lru_cache.Make (Url_key)

let cache : string Cache.t = Cache.create ~max_size:128 ()
let ttl = Time_ns.Span.of_int_sec 300 (* 5 minutes *)

let register ~env ~dir:_ ~net : Ochat_function.t =
  let run url =
    try
      Cache.find_or_add cache url ~ttl ~default:(fun () ->
        Driver.(fetch_and_convert ~env ~net url |> Markdown.to_string))
    with
    | exn -> Printf.sprintf "Error fetching %s: %s\n" url (Exn.to_string exn)
  in
  Ochat_function.create_function (module Definitions.Webpage_to_markdown) run
;;
