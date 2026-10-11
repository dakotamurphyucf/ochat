open Core
module Attempt = Project_prepare_attempt
module Error = Project_prepare_error
module Project = Chatml_project
module Build = Chatml_project_build
module Roots = Chatml_local_roots
module Path = Source.Portable_path
module Catalog = Chatml_catalog
module Captured = Chatml_source
module Accounting = Project_prepare_accounting

module Location = struct
  type t =
    | Local of
        { cursor : Roots.Cursor.t
        ; project : Path.t
        }
    | Git of
        { repository : string
        ; pin : Project.Git_pin.t
        ; package_root : Path.t option
        ; project : Path.t
        }

  let origin = function
    | Local _ -> Project.Lock.Catalog.Local
    | Git { repository; pin; package_root; project = _ } ->
      Project.Lock.Catalog.Git { repository; pin; package_root }
  ;;

  let project = function
    | Local { cursor = _; project }
    | Git { repository = _; pin = _; package_root = _; project } -> project
  ;;
end

type node =
  { number : int
  ; location : Location.t
  ; catalog : Catalog.t
  ; mutable dependencies : Project.Lock.Dependency_selection.t list
  }

type job =
  { location : Location.t
  ; selection : Project.Selection.request
  ; namespace_override : string option
  ; previous_owner : Build.Selection_gate.Owner.t option
  ; parent : (node * Project.Dependency.t) option
  }

type t =
  { attempt : Attempt.t
  ; jobs : job Queue.t
  ; mutable nodes : node list
  ; mutable visited : (int * Roots.Cursor.t option * Captured.Id.t option) list
  ; mutable next_number : int
  }

let create attempt =
  { attempt; jobs = Queue.create (); nodes = []; visited = []; next_number = 0 }
;;

let catalog (node : node) = node.catalog
let location (node : node) = node.location
let charge t steps = Attempt.charge t.attempt { Accounting.zero with work_steps = steps }
let checkpoint t () = Attempt.poll t.attempt

let owned node problem =
  Error.create ~declared_at:(Project.declared_at (Catalog.project node.catalog)) problem
;;

let same_string t left right =
  let length = Int.min (String.length left) (String.length right) in
  let open Result.Let_syntax in
  (* Units count bounded byte-comparison batches, not CPU instructions. *)
  let%bind () = charge t (1 + (length / 1024)) in
  let rec loop offset =
    if offset mod 1024 = 0 then Attempt.poll t.attempt;
    if offset = length
    then Int.equal (String.length left) (String.length right)
    else if Char.equal left.[offset] right.[offset]
    then loop (offset + 1)
    else false
  in
  Ok (loop 0)
;;

let same_path t left right = same_string t (Path.to_string left) (Path.to_string right)

let same_optional f left right =
  match left, right with
  | None, None -> Ok true
  | Some left, Some right -> f left right
  | None, Some _ | Some _, None -> Ok false
;;

let same_location t (left : Location.t) (right : Location.t) =
  let open Result.Let_syntax in
  let%bind () = charge t 1 in
  match left, right with
  | Local left, Local right ->
    if
      Chatml_local_source.Directory_identity.equal
        (Roots.Cursor.identity left.cursor)
        (Roots.Cursor.identity right.cursor)
    then same_path t left.project right.project
    else Ok false
  | Git left, Git right ->
    let%bind repository = same_string t left.repository right.repository in
    if (not repository) || not (Project.Git_pin.equal left.pin right.pin)
    then Ok false
    else (
      let%bind root = same_optional (same_path t) left.package_root right.package_root in
      if root then same_path t left.project right.project else Ok false)
  | Local _, Git _ | Git _, Local _ -> Ok false
;;

let same_id t left right =
  let open Result.Let_syntax in
  let%bind namespace =
    same_string
      t
      (Captured.Id.package_namespace left)
      (Captured.Id.package_namespace right)
  in
  if namespace
  then same_string t (Captured.Id.relative_path left) (Captured.Id.relative_path right)
  else Ok false
;;

let enqueue t ~location ~selection ~namespace_override ~previous_owner ~parent =
  let open Result.Let_syntax in
  let%map () = charge t 1 in
  Queue.enqueue t.jobs { location; selection; namespace_override; previous_owner; parent }
;;

let take t ~repository =
  let open Result.Let_syntax in
  let rec scan remaining =
    if remaining = 0
    then Ok None
    else (
      let%bind () = charge t 1 in
      let job = Queue.dequeue_exn t.jobs in
      let%bind selected =
        match repository, job.location with
        | None, _ -> Ok true
        | Some _, Local _ -> Ok false
        | Some (repository, pin), Git location ->
          let%map same = same_string t repository location.repository in
          same && Project.Git_pin.equal pin location.pin
      in
      if selected
      then Ok (Some job)
      else (
        Queue.enqueue t.jobs job;
        scan (remaining - 1)))
  in
  scan (Queue.length t.jobs)
;;

let find t location =
  let open Result.Let_syntax in
  let rec search = function
    | [] -> Ok None
    | (node : node) :: rest ->
      let%bind same = same_location t node.location location in
      if same then Ok (Some node) else search rest
  in
  search t.nodes
;;

let add t location ~catalog =
  let open Result.Let_syntax in
  let%bind previous = find t location in
  match previous with
  | Some node -> Error (owned node Conflicting_selection)
  | None ->
    if t.next_number = Int.max_value
    then Error (Error.create Accounting_overflow)
    else (
      let%map () = charge t 1 in
      let node = { number = t.next_number; location; catalog; dependencies = [] } in
      t.nodes <- node :: t.nodes;
      t.next_number <- t.next_number + 1;
      node)
;;

let check_selection t node job =
  let project = Catalog.project node.catalog in
  let open Result.Let_syntax in
  (* A selection stage unit plus declaration-byte batches covers the bounded
     domain lookup walk; the checkpoint never doubles as an admission counter. *)
  let%bind () = charge t (1 + Captured.byte_length (Project.source project)) in
  let%bind selected =
    Project.select project ~request:job.selection ~checkpoint:(checkpoint t)
    |> Result.map_error ~f:(fun error ->
      Error.create ~declared_at:(Project.declared_at project) (Declaration error))
  in
  let expected_namespace =
    Option.value job.namespace_override ~default:(Project.namespace project)
  in
  let%bind namespace =
    same_string
      t
      expected_namespace
      (Captured.Id.package_namespace (Captured.id (Project.source project)))
  in
  let%bind environment =
    same_string
      t
      (Project.Scope_spec.name (Project.Selection.environment selected))
      (Project.Scope_spec.name
         (Project.Selection.environment (Catalog.selection node.catalog)))
  in
  if namespace && environment then Ok () else Error (owned node Conflicting_selection)
;;

let connect t job target =
  let open Result.Let_syntax in
  match job.parent with
  | None -> Ok ()
  | Some (owner, dependency) ->
    let name = Project.Dependency.name dependency in
    let declaration = Captured.id (Project.source (Catalog.project target.catalog)) in
    let rec existing = function
      | [] ->
        let%bind () = charge t 1 in
        let%map selection =
          Project.Lock.Dependency_selection.create
            ~name
            ~declaration
            ~checkpoint:(checkpoint t)
          |> Result.map_error ~f:(fun _ ->
            Error.create
              ~declared_at:(Project.Dependency.declared_at dependency)
              Conflicting_selection)
        in
        owner.dependencies <- selection :: owner.dependencies
      | edge :: rest ->
        let%bind same =
          same_string t name (Project.Lock.Dependency_selection.name edge)
        in
        if not same
        then existing rest
        else (
          let%bind same =
            same_id t declaration (Project.Lock.Dependency_selection.declaration edge)
          in
          if same
          then Ok ()
          else
            Error
              (Error.create
                 ~declared_at:(Project.Dependency.declared_at dependency)
                 Conflicting_selection))
    in
    existing owner.dependencies
;;

let visit t node job =
  let route =
    match job.location with
    | Location.Local { cursor; project = _ } -> Some cursor
    | Git _ -> None
  in
  let previous =
    Option.map job.previous_owner ~f:Build.Selection_gate.Owner.declaration
  in
  let open Result.Let_syntax in
  let rec search = function
    | [] ->
      let%map () = charge t 1 in
      t.visited <- (node.number, route, previous) :: t.visited;
      true
    | (number, old_route, old) :: rest ->
      let%bind () = charge t 1 in
      if
        number <> node.number
        || not (Option.equal Roots.Cursor.equal_route route old_route)
      then search rest
      else (
        let%bind same = same_optional (same_id t) previous old in
        if same then Ok false else search rest)
  in
  search t.visited
;;

let input t node =
  let open Result.Let_syntax in
  let%map () = charge t 1 in
  Build.Catalog_input.create
    node.catalog
    ~origin:(Location.origin node.location)
    ~acquisition_key:(Int.to_string node.number)
    ~dependencies:node.dependencies
;;

let inputs t ~root =
  let open Result.Let_syntax in
  let%bind others =
    List.fold_result t.nodes ~init:[] ~f:(fun result node ->
      let%bind () = charge t 1 in
      if node.number = root.number
      then Ok result
      else (
        let%map input = input t node in
        input :: result))
  in
  let%map root = input t root in
  root, others
;;
