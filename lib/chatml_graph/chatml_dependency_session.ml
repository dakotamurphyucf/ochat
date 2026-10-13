open Core
module Prepared = Chatml_prepared_program
module Lookup = Chatml_configured_lookup
module Reference = Lookup.Reference
module Captured = Chatml_source
module Diagnostic = Chatml_diagnostic
module Graph = Chatml_dependency_graph

module Unit = struct
  type t =
    { id : Prepared.Module_id.t
    ; implementation : Lookup.t
    ; interface : Lookup.t option
    }

  let id t = t.id
  let implementation t = t.implementation
  let interface t = t.interface
end

type 'checked t =
  { graph : Graph.t
  ; entry : 'checked
  ; checked :
      (Prepared.Module_id.t, 'checked, Prepared.Module_id.comparator_witness) Map.t
  }

let graph t = t.graph
let entry t = t.entry
let checked t id = Map.find t.checked id
let error ?site ?(related = []) message = Error (Diagnostic.create ?site ~related message)

let same_context left right =
  Prepared.Module_id.equal (Lookup.owner left) (Lookup.owner right)
  && Captured.Id.equal
       (Captured.id (Lookup.source left))
       (Captured.id (Lookup.source right))
  && Captured.Digest.equal
       (Captured.digest (Lookup.source left))
       (Captured.digest (Lookup.source right))
  && Captured.Digest.equal
       (Lookup.input_fingerprint left)
       (Lookup.input_fingerprint right)
  && Lookup.Phase.equal (Lookup.phase left) (Lookup.phase right)
;;

module Traversal = struct
  type limits =
    { max_checked_modules : int option
    ; max_edges : int option
    ; max_dependency_depth : int
    }

  type 'checked state =
    { input : Prepared.t
    ; checkpoint : unit -> unit
    ; limits : limits
    ; completed :
        (Prepared.Module_id.t, 'checked, Prepared.Module_id.comparator_witness) Map.t ref
    ; checked_count : int ref
    ; order_rev : Prepared.Module_id.t list ref
    ; edges_rev : Graph.Edge.t list ref
    ; edge_count : int ref
    ; current : Prepared.Module_id.t option ref
    }

  let create
        input
        ~limits
        ~checkpoint
        ~completed
        ~checked_count
        ~order_rev
        ~edges_rev
        ~edge_count
    =
    { input
    ; checkpoint
    ; limits
    ; completed = ref completed
    ; checked_count = ref checked_count
    ; order_rev = ref order_rev
    ; edges_rev = ref edges_rev
    ; edge_count = ref edge_count
    ; current = ref None
    }
  ;;

  let create_unit t id =
    let open Result.Let_syntax in
    let implementation_id = Prepared.Module_id.implementation id in
    let%bind implementation =
      Lookup.create t.input ~owner:id ~source:implementation_id ~checkpoint:t.checkpoint
    in
    let%map interface =
      match
        Prepared.find_definition t.input id
        |> Option.bind ~f:Prepared.Definition.interface
      with
      | None -> Ok None
      | Some source ->
        Result.map
          (Lookup.create t.input ~owner:id ~source ~checkpoint:t.checkpoint)
          ~f:Option.some
    in
    { Unit.id; implementation; interface }
  ;;

  let validate t unit ~source reference =
    t.checkpoint ();
    let site = Reference.site reference in
    let location = Diagnostic.Site.location site in
    let fingerprint = Prepared.fingerprint t.input in
    let active_matches =
      Option.exists !(t.current) ~f:(Prepared.Module_id.equal (Unit.id unit))
    in
    let source_matches =
      same_context source (Unit.implementation unit)
      || Option.exists (Unit.interface unit) ~f:(same_context source)
    in
    if
      active_matches
      && source_matches
      && Captured.Digest.equal fingerprint (Lookup.input_fingerprint source)
      && Captured.Digest.equal fingerprint (Reference.input_fingerprint reference)
      && Prepared.Module_id.equal (Unit.id unit) (Reference.owner reference)
      && Lookup.Phase.equal (Lookup.phase source) (Reference.phase reference)
      && Captured.Id.equal
           (Captured.id (Lookup.source source))
           (Captured.Location.source location)
      && Captured.Digest.equal
           (Captured.digest (Lookup.source source))
           (Captured.Location.source_digest location)
    then Ok ()
    else
      error
        ~site
        "Dependency reference does not belong to this active input, owner or source phase"
  ;;

  let cycle t active incoming id =
    (* Active stack is newest-first. Exclude the repeated target's incoming
       edge: that belongs to the acyclic prefix, not this cycle. *)
    let rec witness related count = function
      | [] -> related, count
      | (owner, incoming) :: rest ->
        t.checkpoint ();
        if Prepared.Module_id.equal id owner
        then related, count
        else (
          let related, count =
            match incoming with
            | None -> related, count
            | Some site ->
              let related =
                if count < 8
                then
                  Diagnostic.Related.create site ~message:"Cycle dependency reference"
                  :: related
                else related
              in
              related, count + 1
          in
          witness related count rest)
    in
    let related, count = witness [] 0 active in
    let message =
      if count > 8
      then
        "Actual configured module dependency cycle (showing last 8 preceding cycle \
         references)"
      else "Actual configured module dependency cycle"
    in
    error ?site:incoming ~related message
  ;;

  let rec visit t ~check active ~depth incoming id =
    t.checkpoint ();
    match Map.find !(t.completed) id with
    | Some value -> Ok value
    | None ->
      if
        List.exists active ~f:(fun (owner, _) ->
          t.checkpoint ();
          Prepared.Module_id.equal id owner)
      then cycle t active incoming id
      else if depth >= t.limits.max_dependency_depth
      then error ?site:incoming "Configured module dependency depth limit exceeded"
      else if
        Option.exists t.limits.max_checked_modules ~f:(fun limit ->
          let completed = !(t.checked_count) in
          completed >= limit || depth >= limit - completed)
      then error ?site:incoming "Configured module check limit exceeded"
      else
        let open Result.Let_syntax in
        let%bind unit = create_unit t id in
        let active = (id, incoming) :: active in
        let require ~source reference =
          let%bind () = validate t unit ~source reference in
          let site = Reference.site reference in
          if Option.exists t.limits.max_edges ~f:(fun limit -> !(t.edge_count) >= limit)
          then error ~site "Configured module reference limit exceeded"
          else (
            let%bind dependency =
              visit
                t
                ~check
                active
                ~depth:(depth + 1)
                (Some site)
                (Reference.dependency reference)
            in
            t.checkpoint ();
            (* The nested visit may itself have consumed the last edge slot. *)
            if Option.exists t.limits.max_edges ~f:(fun limit -> !(t.edge_count) >= limit)
            then error ~site "Configured module reference limit exceeded"
            else (
              t.edges_rev := Graph.Edge.of_reference reference :: !(t.edges_rev);
              Int.incr t.edge_count;
              Ok dependency))
        in
        let previous = !(t.current) in
        t.current := Some id;
        let result =
          Exn.protect
            ~f:(fun () -> check unit ~require)
            ~finally:(fun () -> t.current := previous)
        in
        let%bind value = result in
        t.checkpoint ();
        t.completed := Map.set !(t.completed) ~key:id ~data:value;
        Int.incr t.checked_count;
        t.order_rev := id :: !(t.order_rev);
        Ok value
  ;;
end

module Roots = struct
  module Limits = struct
    type t = Traversal.limits

    let create ~max_checked_modules ~max_edges ~max_dependency_depth =
      if max_checked_modules <= 0
      then Or_error.error_string "Module check limit must be positive"
      else if max_edges < 0
      then Or_error.error_string "Module reference limit must be nonnegative"
      else if max_dependency_depth <= 0
      then Or_error.error_string "Dependency depth limit must be positive"
      else
        Ok
          { Traversal.max_checked_modules = Some max_checked_modules
          ; max_edges = Some max_edges
          ; max_dependency_depth
          }
    ;;
  end

  module Error = struct
    type t =
      | Invalid_reference of Lookup.Repl_scope.Error.t
      | Invalid_cache_seed
      | Different_input
      | Failed_check of Diagnostic.t
    [@@deriving sexp_of]
  end

  type 'checked t =
    { input : Prepared.t
    ; limits : Limits.t
    ; checked :
        (Prepared.Module_id.t, 'checked, Prepared.Module_id.comparator_witness) Map.t
    ; order : Prepared.Module_id.t list
    ; rows : (Prepared.Module_id.t * 'checked) list
    ; edges : Graph.Edge.t list
    ; checked_count : int
    ; edge_count : int
    }

  let empty input ~limits =
    { input
    ; limits
    ; checked = Map.empty (module Prepared.Module_id)
    ; order = []
    ; rows = []
    ; edges = []
    ; checked_count = 0
    ; edge_count = 0
    }
  ;;

  let input_fingerprint t = Prepared.fingerprint t.input
  let edges t = t.edges
  let checked_in_order t = t.rows

  let reverse_with_checkpoint rows ~checkpoint =
    List.fold rows ~init:[] ~f:(fun reversed row ->
      checkpoint ();
      row :: reversed)
  ;;

  let rebind_checked t ~checked ~checkpoint =
    let rec loop order values rebound =
      checkpoint ();
      match order, values with
      | [], [] -> Ok rebound
      | id :: order, (candidate, value) :: values
        when Prepared.Module_id.equal id candidate ->
        loop order values (Map.set rebound ~key:id ~data:value)
      | _ -> Error Error.Invalid_cache_seed
    in
    Result.map
      (loop t.order checked (Map.empty (module Prepared.Module_id)))
      ~f:(fun rebound -> { t with checked = rebound; rows = checked })
  ;;

  let require_selected t ~scope ~source reference ~checkpoint ~check =
    let open Result.Let_syntax in
    if
      not
        (Captured.Digest.equal
           (input_fingerprint t)
           (Lookup.Repl_scope.input_fingerprint scope))
    then Error Error.Different_input
    else (
      let%bind () =
        Lookup.Repl_scope.validate_reference scope ~context:source reference ~checkpoint
        |> Result.map_error ~f:(fun error -> Error.Invalid_reference error)
      in
      let state =
        Traversal.create
          t.input
          ~limits:t.limits
          ~checkpoint
          ~completed:t.checked
          ~checked_count:t.checked_count
          ~order_rev:(reverse_with_checkpoint t.order ~checkpoint)
          ~edges_rev:(reverse_with_checkpoint t.edges ~checkpoint)
          ~edge_count:t.edge_count
      in
      let%bind value =
        Traversal.visit
          state
          ~check
          []
          ~depth:0
          (Some (Reference.site reference))
          (Reference.dependency reference)
        |> Result.map_error ~f:(fun diagnostic -> Error.Failed_check diagnostic)
      in
      let order = reverse_with_checkpoint !(state.order_rev) ~checkpoint in
      let edges = reverse_with_checkpoint !(state.edges_rev) ~checkpoint in
      let rows =
        List.map order ~f:(fun id ->
          checkpoint ();
          id, Map.find_exn !(state.completed) id)
      in
      checkpoint ();
      let%bind () =
        Lookup.Repl_scope.validate_reference scope ~context:source reference ~checkpoint
        |> Result.map_error ~f:(fun error -> Error.Invalid_reference error)
      in
      (* No callback may yield between final affinity validation and publication. *)
      let next =
        { t with
          checked = !(state.completed)
        ; order
        ; rows
        ; edges
        ; checked_count = !(state.checked_count)
        ; edge_count = !(state.edge_count)
        }
      in
      Ok (next, value))
  ;;
end

let run input ~max_dependency_depth ~checkpoint ~check =
  if max_dependency_depth <= 0
  then error "Dependency depth limit must be positive"
  else
    let open Result.Let_syntax in
    let state =
      Traversal.create
        input
        ~limits:
          { Traversal.max_checked_modules = None; max_edges = None; max_dependency_depth }
        ~checkpoint
        ~completed:(Map.empty (module Prepared.Module_id))
        ~checked_count:0
        ~order_rev:[]
        ~edges_rev:[]
        ~edge_count:0
    in
    let entry_id = Prepared.Module_id.of_implementation (Prepared.entry input) in
    let%bind entry = Traversal.visit state ~check [] ~depth:0 None entry_id in
    let%bind graph =
      Graph.create
        ~input_fingerprint:(Prepared.fingerprint input)
        ~entry:entry_id
        ~dependency_order:(List.rev !(state.order_rev))
        ~edges:!(state.edges_rev)
        ~checkpoint
    in
    checkpoint ();
    Ok { graph; entry; checked = !(state.completed) }
;;
