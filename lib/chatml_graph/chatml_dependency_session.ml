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

let run input ~max_dependency_depth ~checkpoint ~check =
  if max_dependency_depth <= 0
  then error "Dependency depth limit must be positive"
  else
    let open Result.Let_syntax in
    let fingerprint = Prepared.fingerprint input in
    let completed = ref (Map.empty (module Prepared.Module_id)) in
    let order_rev = ref [] in
    let edges_rev = ref [] in
    let current = ref None in
    let create_unit id =
      let implementation_id = Prepared.Module_id.implementation id in
      let%bind implementation =
        Lookup.create input ~owner:id ~source:implementation_id ~checkpoint
      in
      let%map interface =
        match
          Prepared.find_definition input id
          |> Option.bind ~f:Prepared.Definition.interface
        with
        | None -> Ok None
        | Some source ->
          Result.map (Lookup.create input ~owner:id ~source ~checkpoint) ~f:Option.some
      in
      { Unit.id; implementation; interface }
    in
    let validate unit ~source reference =
      checkpoint ();
      let site = Reference.site reference in
      let location = Diagnostic.Site.location site in
      let active_matches =
        Option.exists !current ~f:(Prepared.Module_id.equal (Unit.id unit))
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
          "Dependency reference does not belong to this active input, owner or source \
           phase"
    in
    let rec visit active incoming id =
      checkpoint ();
      match Map.find !completed id with
      | Some value -> Ok value
      | None ->
        if
          List.exists active ~f:(fun (owner, _) ->
            checkpoint ();
            Prepared.Module_id.equal id owner)
        then (
          (* Active stack is newest-first. Exclude the repeated target's incoming
             edge: that belongs to the acyclic prefix, not this cycle. Prepending
             the remaining incoming sites gives chronological cycle order. *)
          let rec witness related count = function
            | [] -> related, count
            | (owner, incoming) :: rest ->
              checkpoint ();
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
                        Diagnostic.Related.create
                          site
                          ~message:"Cycle dependency reference"
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
          error ?site:incoming ~related message)
        else if List.length active >= max_dependency_depth
        then error ?site:incoming "Configured module dependency depth limit exceeded"
        else (
          let%bind unit = create_unit id in
          let active = (id, incoming) :: active in
          let require ~source reference =
            let%bind () = validate unit ~source reference in
            let%bind dependency =
              visit
                active
                (Some (Reference.site reference))
                (Reference.dependency reference)
            in
            edges_rev := Graph.Edge.of_reference reference :: !edges_rev;
            Ok dependency
          in
          let previous = !current in
          current := Some id;
          let result =
            Exn.protect
              ~f:(fun () -> check unit ~require)
              ~finally:(fun () -> current := previous)
          in
          let%bind value = result in
          checkpoint ();
          completed := Map.set !completed ~key:id ~data:value;
          order_rev := id :: !order_rev;
          Ok value)
    in
    let entry_id = Prepared.Module_id.of_implementation (Prepared.entry input) in
    let%bind entry = visit [] None entry_id in
    let%bind graph =
      Graph.create
        ~input_fingerprint:fingerprint
        ~entry:entry_id
        ~dependency_order:(List.rev !order_rev)
        ~edges:!edges_rev
        ~checkpoint
    in
    checkpoint ();
    Ok { graph; entry; checked = !completed }
;;
