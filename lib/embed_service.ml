open! Core
open Eio

(**************************************************************************)
(* Token counting helper                                                    *)
(**************************************************************************)

let token_count ~codec text =
  match Tikitoken.encode ~codec ~text with
  | tokens -> List.length tokens
  | exception _ ->
    (* Fallback: approximate by ASCII whitespace splitting – sufficient for
       rate-limiting and vector length metadata. *)
    String.split_on_chars text ~on:[ ' '; '\n'; '\t'; '\r' ]
    |> List.filter ~f:(fun s -> not (String.is_empty s))
    |> List.length

(**************************************************************************)
(* Low-level embedding call                                                 *)
(**************************************************************************)

let get_vectors ~net ~codec ~get_id (snippets : ('m * string) list) =
  (* 1. Issue HTTP call – the caller is responsible for batching so the
     payload stays within model limits. *)
  let inputs = List.map snippets ~f:(fun (_meta, text) -> text) in
  let response =
    Openai.Embeddings.post_openai_embeddings net ~input:inputs
  in
  (* 2. Re-assemble results in original order and convert to [Vector_db.Vec]. *)
  let tbl = Hashtbl.create (module Int) in
  List.iteri snippets ~f:(fun idx (meta, text) ->
      Hashtbl.add_exn tbl ~key:idx ~data:(meta, text));
  List.map response.data ~f:(fun item ->
      let meta, text = Hashtbl.find_exn tbl item.index in
      let len = token_count ~codec text in
      let id = get_id meta in
      let vector = Array.of_list item.embedding in
      meta, text, Vector_db.Vec.{ id; len; vector })

(**************************************************************************)
(* Public factory – concurrent, rate-limited embedder                       *)
(**************************************************************************)

let create
    ~(sw : Switch.t)
    ~(clock : _ Time.clock)
    ~(net : _ Net.t)
    ~codec
    ~rate_per_sec
    ~get_id
  :  ('meta * string) list
  -> ('meta * string * Vector_db.Vec.t) list
  =
  (* Stream used to serialise requests to the background daemon. *)
  let stream :
    ( ('meta * string) list
    * (('meta * string * Vector_db.Vec.t) list, exn) result Promise.u )
    Stream.t
    = Stream.create 100
  in

  (* Helper: run OpenAI call with up to 3 retries and constant back-off. *)
  let rec fetch_with_retries attempts snippets =
    try get_vectors ~net ~codec ~get_id snippets with
    | exn when attempts < 3 ->
      traceln "embed retry %d/3 due to %a" (attempts + 1) Eio.Exn.pp exn;
      Time.sleep clock 1.0;
      fetch_with_retries (attempts + 1) snippets
  in

  (* Background daemon that enforces [rate_per_sec]. *)
  Fiber.fork_daemon ~sw (fun () ->
      let last_call = ref 0.0 in
      let min_interval = 1.0 /. Float.of_int rate_per_sec in
      let rec loop () =
        let snippets, resolver = Stream.take stream in
        (* Throttle if the previous call was inside the interval. *)
        let now = Time.now clock in
        let elapsed = now -. !last_call in
        if Float.(elapsed < min_interval) then (
          let to_sleep = min_interval -. elapsed in
          if Float.(to_sleep > 0.0) then Time.sleep clock to_sleep);
        last_call := Time.now clock;
        Fiber.fork ~sw (fun () ->
            let result =
              try Ok (fetch_with_retries 0 snippets) with
              | ex -> Error ex
            in
            Promise.resolve resolver result);
        loop ()
      in
      loop ());

  (* Returned [embed] function – enqueue request and await the promise. *)
  fun snippets ->
    let promise, resolver = Promise.create () in
    Stream.add stream (snippets, resolver);
    match Promise.await promise with
    | Ok res -> res
    | Error ex -> raise ex

