open Core
module Prepared = Chatml_prepared_program
module Reference = Chatml_configured_lookup.Reference
module Lookup = Chatml_configured_lookup
module Captured = Chatml_source
module Diagnostic = Chatml_diagnostic

module Edge = struct
  type t = Reference.t

  let of_reference reference = reference
  let owner = Reference.owner
  let dependency = Reference.dependency
  let name = Reference.name
  let reason = Reference.reason
  let phase = Reference.phase
  let site = Reference.site
  let binding_site = Reference.binding_site
  let input_fingerprint = Reference.input_fingerprint
  let member_path = Reference.member_path

  let is_runtime_dependency t =
    match reason t with
    | Lookup.Reason.Value_reference -> true
    | Open | Qualified_type -> false
  ;;

  let location_key site =
    let location = Diagnostic.Site.location site in
    let span = Captured.Location.span location in
    ( Captured.Location.source location
    , Captured.Digest.to_hex (Captured.Location.source_digest location)
    , span.left.offset
    , span.right.offset )
  ;;

  type key =
    Prepared.Module_id.t
    * Prepared.Module_id.t
    * string
    * Lookup.Reason.t
    * Lookup.Phase.t
    * string list
    * (Captured.Id.t * string * int * int)
    * (Captured.Id.t * string * int * int)
    * string
  [@@deriving compare]

  let key t =
    ( owner t
    , dependency t
    , name t
    , reason t
    , phase t
    , member_path t
    , location_key (site t)
    , location_key (binding_site t)
    , Captured.Digest.to_hex (input_fingerprint t) )
  ;;

  let compare left right = compare_key (key left) (key right)
end

type t =
  { entry : Prepared.Module_id.t
  ; dependency_order : Prepared.Module_id.t list
  ; edges : Edge.t list
  ; input_fingerprint : Captured.Digest.t
  }

let entry t = t.entry
let nodes t = t.dependency_order
let edges t = t.edges
let input_fingerprint t = t.input_fingerprint
let dependency_order t = t.dependency_order
let error message = Error (Diagnostic.create message)

let dependency_closure ~entry ~edges ~select ~checkpoint =
  let adjacency =
    List.fold
      edges
      ~init:(Map.empty (module Prepared.Module_id))
      ~f:(fun adjacency edge ->
        checkpoint ();
        if select edge
        then Map.add_multi adjacency ~key:(Edge.owner edge) ~data:(Edge.dependency edge)
        else adjacency)
  in
  let rec visit visited = function
    | [] -> visited
    | id :: pending ->
      checkpoint ();
      if Set.mem visited id
      then visit visited pending
      else (
        let visited = Set.add visited id in
        let next = Map.find adjacency id |> Option.value ~default:[] in
        visit visited (List.rev_append next pending))
  in
  visit (Set.empty (module Prepared.Module_id)) [ entry ]
;;

let create ~input_fingerprint ~entry ~dependency_order ~edges ~checkpoint =
  let open Result.Let_syntax in
  let%bind positions =
    List.foldi
      dependency_order
      ~init:(Ok (Map.empty (module Prepared.Module_id)))
      ~f:(fun index result id ->
        checkpoint ();
        let%bind positions = result in
        if Map.mem positions id
        then error "Duplicate canonical node in dependency order"
        else Ok (Map.set positions ~key:id ~data:index))
  in
  let%bind () =
    if Map.mem positions entry
    then Ok ()
    else error "Dependency graph omits its entry node"
  in
  let%bind () =
    List.fold_result edges ~init:() ~f:(fun () edge ->
      checkpoint ();
      if not (Captured.Digest.equal input_fingerprint (Edge.input_fingerprint edge))
      then error "Dependency edge belongs to another admitted input"
      else (
        match
          Map.find positions (Edge.owner edge), Map.find positions (Edge.dependency edge)
        with
        | Some owner, Some dependency when dependency < owner -> Ok ()
        | Some _, Some _ ->
          error "Dependency order does not precede its referencing owner"
        | _ -> error "Dependency edge refers to an absent canonical node"))
  in
  let%bind () =
    let reachable =
      dependency_closure ~entry ~edges ~select:(fun _ -> true) ~checkpoint
    in
    if Set.length reachable <> Map.length positions
    then error "Dependency graph contains nodes not reachable from its entry"
    else if Map.find_exn positions entry <> Map.length positions - 1
    then error "Dependency order must finish with its entry"
    else Ok ()
  in
  let edges =
    List.dedup_and_sort edges ~compare:(fun left right ->
      checkpoint ();
      Edge.compare left right)
  in
  checkpoint ();
  Ok { entry; dependency_order; edges; input_fingerprint }
;;

let runtime_dependency_order t ~checkpoint =
  let selected =
    dependency_closure
      ~entry:t.entry
      ~edges:t.edges
      ~select:Edge.is_runtime_dependency
      ~checkpoint
  in
  List.filter t.dependency_order ~f:(fun id ->
    checkpoint ();
    Set.mem selected id)
;;
