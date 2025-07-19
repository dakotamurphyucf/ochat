open Core

(* -------------------------------------------------------------------------- *)
(* Small in-memory TTL cache to avoid refetching the same URL repeatedly       *)
(* -------------------------------------------------------------------------- *)

module Url_key = struct
  type t = string [@@deriving sexp, bin_io, hash, compare]

  let invariant (_ : t) = ()
end

module Cache = Ttl_lru_cache.Make (Url_key)

let cache : string Cache.t = Cache.create ~max_size:128 ()
let ttl = Time_ns.Span.of_int_sec 300 (* 5 minutes *)

let register ~dir:_ ~net : Gpt_function.t =
  let run url =
    try
      Cache.find_or_add cache url ~ttl ~default:(fun () ->
        Driver.(fetch_and_convert ~net url |> Markdown.to_string))
    with
    | exn -> Printf.sprintf "Error fetching %s: %s\n" url (Exn.to_string exn)
  in
  Gpt_function.create_function (module Definitions.Webpage_to_markdown) run
;;
