open! Core

(*--------------------------------------------------------------------------*)
(* Token counting helper                                                    *)
(*--------------------------------------------------------------------------*)

let token_count ~codec text =
  match Tikitoken.encode ~codec ~text with
  | tokens -> List.length tokens
  | exception _ ->
    String.split_on_chars text ~on:[ ' '; '\n'; '\t'; '\r' ]
    |> List.filter ~f:(fun s -> not (String.is_empty s))
    |> List.length

(*--------------------------------------------------------------------------*)
(* Single embedding call                                                     *)
(*--------------------------------------------------------------------------*)

let get_vectors ~net ~codec ~get_id (snippets : ('m * string) list) =
  let inputs = List.map snippets ~f:(fun (_meta, text) -> text) in
  let response = Openai.Embeddings.post_openai_embeddings net ~input:inputs in
  let tbl = Hashtbl.create (module Int) in
  List.iteri snippets ~f:(fun idx (meta, text) ->
      Hashtbl.add_exn tbl ~key:idx ~data:(meta, text));
  List.map response.data ~f:(fun item ->
      let meta, text = Hashtbl.find_exn tbl item.index in
      let len = token_count ~codec text in
      let id = get_id meta in
      let vector = Array.of_list item.embedding in
      meta, text, Vector_db.Vec.{ id; len; vector })

(*--------------------------------------------------------------------------*)
(* Public factory – currently a thin wrapper (rate limit TBD)                *)
(*--------------------------------------------------------------------------*)

let create
    ~sw
    ~clock
    ~net
    ~codec
    ~rate_per_sec
    ~get_id
  =
  let _unused = sw, clock, rate_per_sec in
  (* TODO: implement proper request throttling & retries.  For now we issue
     the HTTP call synchronously which is sufficient for test fixtures. *)
  fun snippets -> get_vectors ~net ~codec ~get_id snippets

