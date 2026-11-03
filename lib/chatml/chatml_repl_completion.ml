open Core
module Scope = Chatml_typechecker.Value_scope
module Inferred = Chatml_typechecker
module Snapshot = Chatml_typechecker.Type_snapshot

type status =
  | Complete
  | Limited
  | Unavailable
[@@deriving equal, sexp_of]

type t =
  { names : string list
  ; status : status
  }

let names t = t.names
let status t = t.status
let max_bindings = 4096

exception Traversal_limit

let valid_name name =
  let start = function
    | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
    | _ -> false
  in
  String.length name > 0
  && String.length name <= 128
  && start name.[0]
  && String.for_all name ~f:(fun c -> start c || Char.is_digit c)
;;

let select names ~prefix ~limit ~max_bytes ~limited ~checkpoint =
  let limited = ref limited in
  let sorted =
    List.filter names ~f:(fun name ->
      checkpoint ();
      if not (String.is_prefix name ~prefix)
      then false
      else if valid_name name
      then true
      else (
        limited := true;
        false))
    |> List.dedup_and_sort ~compare:String.compare
  in
  let rec take remaining count acc = function
    | [] -> { names = List.rev acc; status = (if !limited then Limited else Complete) }
    | name :: rest ->
      checkpoint ();
      if count >= limit || String.length name > remaining
      then { names = List.rev acc; status = Limited }
      else take (remaining - String.length name) (count + 1) (name :: acc) rest
  in
  take max_bytes 0 [] sorted
;;

let module_fields snapshot root ~step =
  let visited = Int.Hash_set.create () in
  let rec module_row id =
    step ();
    let index = Snapshot.Id.index id in
    if Hash_set.mem visited index
    then [], true
    else (
      Hash_set.add visited index;
      match Snapshot.find snapshot id with
      | Some (Bound_link child) -> module_row child
      | Some (Module row) -> row_fields row String.Map.empty
      | Some _ | None -> [], false)
  and row_fields id fields =
    step ();
    let index = Snapshot.Id.index id in
    if Hash_set.mem visited index
    then Map.to_alist fields, true
    else (
      Hash_set.add visited index;
      match Snapshot.find snapshot id with
      | Some (Bound_link child) -> row_fields child fields
      | Some (Row (layer, rest)) ->
        let fields =
          List.fold layer ~init:fields ~f:(fun fields (name, value) ->
            step ();
            (* Outer row layers override names in their tails. *)
            if Map.mem fields name then fields else Map.set fields ~key:name ~data:value)
        in
        row_fields rest fields
      | Some Empty_row -> Map.to_alist fields, false
      | Some _ | None -> Map.to_alist fields, true)
  in
  module_row root
;;

let qualified scheme ~path ~prefix ~limit ~max_bytes ~checkpoint =
  let limits =
    Snapshot.Limits.create ~max_nodes:4096 ~max_edges:8192 ~max_label_bytes:65536
    |> Or_error.ok_exn
  in
  match
    Snapshot.freeze [ Snapshot.Input_group.create [ scheme ] ] ~limits ~checkpoint
  with
  | Error _ -> { names = []; status = Limited }
  | Ok snapshot ->
    let work = ref 0 in
    let step () =
      checkpoint ();
      Int.incr work;
      if !work > 8192 then raise Traversal_limit
    in
    (try
       let rec descend root = function
         | [] ->
           let fields, limited = module_fields snapshot root ~step in
           select (List.map fields ~f:fst) ~prefix ~limit ~max_bytes ~limited ~checkpoint
         | component :: rest ->
           let fields, limited = module_fields snapshot root ~step in
           (match List.Assoc.find fields component ~equal:String.equal with
            | Some child -> descend child rest
            | None -> { names = []; status = (if limited then Limited else Complete) })
       in
       match Snapshot.roots snapshot with
       | [ [ root ] ] -> descend root path
       | _ -> { names = []; status = Limited }
     with
     | Traversal_limit -> { names = []; status = Limited })
;;

let query
      lexical
      ~configured
      ~configured_limited
      ~path
      ~prefix
      ~limit
      ~max_bytes
      ~checkpoint
  =
  let configured = List.take configured (max_bindings + 1) in
  let configured_limited = configured_limited || List.length configured > max_bindings in
  let configured = List.take configured max_bindings in
  match path with
  | [] ->
    let lexical_names, lexical_limited =
      match Scope.Roots.capture lexical ~max_bindings ~checkpoint with
      | Ok roots -> Scope.Roots.names roots ~checkpoint, false
      | Error `Binding_limit -> [], true
    in
    select
      (lexical_names @ List.map configured ~f:fst)
      ~prefix
      ~limit
      ~max_bytes
      ~limited:(lexical_limited || configured_limited)
      ~checkpoint
  | first :: rest ->
    (match Scope.find lexical ~name:first with
     | Some binding ->
       qualified
         (Scope.Binding.scheme binding)
         ~path:rest
         ~prefix
         ~limit
         ~max_bytes
         ~checkpoint
     | None ->
       (match List.Assoc.find configured first ~equal:String.equal with
        | Some (Some scheme) ->
          qualified scheme ~path:rest ~prefix ~limit ~max_bytes ~checkpoint
        | Some None ->
          { names = []; status = (if configured_limited then Limited else Unavailable) }
        | None ->
          { names = []; status = (if configured_limited then Limited else Complete) }))
;;
