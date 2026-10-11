open Core
module F = Fixture
module S = Chatml_source
module P = Chatml_prepared_program
module Lookup = Chatml_configured_lookup
module Session = Chatml_dependency_session
module Graph = Chatml_dependency_graph

let checkpoint () = ()

(* Independent bounded fixed-point model. No traversal stack/cache or checker
   lookup algorithm is copied; all graph facts below are explicit API events. *)
let closure adjacency root =
  let selected = Array.create ~len:4 false in
  selected.(root) <- true;
  for _ = 0 to 3 do
    for owner = 0 to 3 do
      if selected.(owner)
      then
        for dependency = 0 to 3 do
          if adjacency owner dependency then selected.(dependency) <- true
        done
    done
  done;
  List.init 4 ~f:Fn.id |> List.filter ~f:(Array.get selected)
;;

let path index = "node" ^ Int.to_string index ^ ".chatml"
let name index = "Node" ^ Int.to_string index

let node_index id =
  List.find_exn (List.init 4 ~f:Fn.id) ~f:(fun index ->
    String.equal (S.Id.relative_path (P.Module_id.implementation id)) (path index))
;;

let fixture () =
  F.input
    (List.init 4 ~f:(fun index ->
       path index, "scope query", List.init 4 ~f:(fun target -> name target, path target)))
;;

let%test_unit
    "bounded graph model covers canonical reachability, order and entry-rooted value \
     projection"
  =
  let input = fixture () in
  let pairs = [ 0, 1; 0, 2; 0, 3; 1, 2; 1, 3; 2, 3 ] in
  for mask = 0 to 63 do
    List.iter [ false; true ] ~f:(fun reverse ->
      let links owner dependency =
        List.existsi pairs ~f:(fun bit (from, target) ->
          Int.equal from owner
          && Int.equal target dependency
          && mask land (1 lsl bit) <> 0)
      in
      let values owner dependency =
        links owner dependency && (owner + dependency) mod 2 = 0
      in
      let checked_counts = Array.create ~len:4 0 in
      let result =
        Session.run input ~max_dependency_depth:4 ~checkpoint ~check:(fun unit ~require ->
          let owner = node_index (Session.Unit.id unit) in
          checked_counts.(owner) <- checked_counts.(owner) + 1;
          let targets = List.init 4 ~f:Fn.id |> List.filter ~f:(links owner) in
          let targets = if reverse then List.rev targets else targets in
          let source = Session.Unit.implementation unit in
          let open Result.Let_syntax in
          let%map () =
            List.fold_result targets ~init:() ~f:(fun () dependency ->
              let reason =
                if values owner dependency
                then Lookup.Reason.Value_reference
                else Qualified_type
              in
              let%bind reference =
                Lookup.resolve
                  source
                  ~name:(name dependency)
                  ~reason
                  ~span:(F.span (Lookup.source source))
                  ~checkpoint
              in
              Result.map (require ~source reference) ~f:(fun _ -> ()))
          in
          owner)
        |> F.ok
      in
      let graph = Session.graph result in
      let observed =
        List.map (Graph.nodes graph) ~f:node_index |> List.sort ~compare:Int.compare
      in
      [%test_eq: int list] observed (closure links 0);
      [%test_eq: int list]
        (List.map (Graph.runtime_dependency_order graph ~checkpoint) ~f:node_index
         |> List.sort ~compare:Int.compare)
        (closure values 0);
      Array.iteri checked_counts ~f:(fun index count ->
        [%test_eq: int] count (if List.mem observed index ~equal:Int.equal then 1 else 0));
      let order = List.map (Graph.dependency_order graph) ~f:node_index in
      let position index =
        List.findi_exn order ~f:(fun _ value -> Int.equal value index) |> fst
      in
      List.iter (Graph.edges graph) ~f:(fun edge ->
        assert (
          position (node_index (Graph.Edge.dependency edge))
          < position (node_index (Graph.Edge.owner edge)))))
  done
;;

let%test_unit
    "session rejects other-input and cross-phase events before checking the target"
  =
  let files target =
    [ "entry.chatml", "Catalog.value", [ "Catalog", target ]
    ; "a.chatml", "let value = 1", []
    ; "b.chatml", "let value = 2", []
    ]
  in
  let input = F.input ~interface:("entry.chatml", "val value : int") (files "a.chatml") in
  let other = F.input ~interface:("entry.chatml", "val value : int") (files "b.chatml") in
  let source = F.context input "entry.chatml" in
  let foreign = F.context other "entry.chatml" in
  let reference context =
    Lookup.resolve
      context
      ~name:"Catalog"
      ~reason:Lookup.Reason.Value_reference
      ~span:(F.span (Lookup.source context))
      ~checkpoint
    |> F.ok
  in
  let wrong_reference = reference foreign in
  List.iter
    [ source, wrong_reference; foreign, wrong_reference ]
    ~f:(fun (source, event) ->
      let calls = ref 0 in
      let result =
        Session.run input ~max_dependency_depth:4 ~checkpoint ~check:(fun _ ~require ->
          Int.incr calls;
          Result.map (require ~source event) ~f:(fun _ -> ()))
      in
      assert (Result.is_error result);
      [%test_eq: int] !calls 1);
  let interface =
    Lookup.create
      input
      ~owner:(Lookup.owner source)
      ~source:(S.id (F.source input "entry.chatmli"))
      ~checkpoint
    |> F.ok
  in
  let result =
    Session.run input ~max_dependency_depth:4 ~checkpoint ~check:(fun _ ~require ->
      Result.map (require ~source:interface (reference source)) ~f:(fun _ -> ()))
  in
  assert (Result.is_error result);
  assert (
    Result.is_error
      (Lookup.at_use
         foreign
         (Lookup.Reference.origin (reference source))
         ~reason:Lookup.Reason.Value_reference
         ~span:(F.span (Lookup.source foreign))
         ~checkpoint))
;;

let%test_unit
    "borrowed require cannot escape its active unit or run during another owner's \
     callback"
  =
  let input =
    F.input
      [ "entry.chatml", "A.value", [ "A", "a.chatml" ]; "a.chatml", "let value = 1", [] ]
  in
  let escaped = ref None in
  let context = F.context input "entry.chatml" in
  let reference =
    Lookup.resolve
      context
      ~name:"A"
      ~reason:Lookup.Reason.Value_reference
      ~span:(F.span (Lookup.source context))
      ~checkpoint
    |> F.ok
  in
  let result =
    Session.run input ~max_dependency_depth:4 ~checkpoint ~check:(fun unit ~require ->
      if P.Module_id.equal (Session.Unit.id unit) (Lookup.owner context)
      then (
        escaped := Some require;
        Result.map (require ~source:context reference) ~f:(fun _ -> ()))
      else (
        assert (Result.is_error ((Option.value_exn !escaped) ~source:context reference));
        Ok ()))
  in
  ignore (F.ok result : unit Session.t);
  assert (Result.is_error ((Option.value_exn !escaped) ~source:context reference))
;;

exception Cancelled

let%test_unit
    "every observed graph/checker checkpoint preserves exception identity and publishes \
     no partial result"
  =
  let input =
    F.input
      [ ( "entry.chatml"
        , "open Catalog let total = identity(1)"
        , [ "Catalog", "catalog.chatml" ] )
      ; "catalog.chatml", "let identity x = x", []
      ]
  in
  let observed = ref 0 in
  ignore
    (F.checked input ~checkpoint:(fun () -> Int.incr observed) |> F.ok
     : Chatml_configured_check.Checked_unit.t Session.t);
  assert (!observed > 0);
  let exceptions =
    [ Cancelled; Chatml_typechecker.Type_error "checkpoint domain collision" ]
  in
  List.iter exceptions ~f:(fun raised ->
    for stop = 1 to !observed do
      let visited = ref 0 in
      let interrupted =
        try
          ignore
            (F.checked input ~checkpoint:(fun () ->
               Int.incr visited;
               if !visited = stop then raise raised));
          false
        with
        | exn ->
          assert (phys_equal exn raised);
          true
      in
      assert interrupted;
      assert (!visited = stop)
    done);
  ignore (F.checked input |> F.ok : Chatml_configured_check.Checked_unit.t Session.t);
  assert (Result.is_error (F.checked input ~max_dependency_depth:1))
;;

let%test_unit
    "cycle witness agrees with a chain oracle, excluding prefixes and admitting only a \
     bounded chronological suffix"
  =
  List.iter
    [ 0, 1; 1, 2; 3, 4; 2, 12 ]
    ~f:(fun (prefix, length) ->
      let total = prefix + length in
      let files =
        List.init total ~f:(fun index ->
          let target = if index + 1 < total then index + 1 else prefix in
          path index, "scope query", [ name target, path target ])
      in
      let input = F.input files in
      let result =
        Session.run
          input
          ~max_dependency_depth:(total + 1)
          ~checkpoint
          ~check:(fun unit ~require ->
            let source = Session.Unit.implementation unit in
            let owner_path = S.Id.relative_path (S.id (Lookup.source source)) in
            let index =
              List.find_exn (List.init total ~f:Fn.id) ~f:(fun index ->
                String.equal (path index) owner_path)
            in
            let target = if index + 1 < total then index + 1 else prefix in
            let reference =
              Lookup.resolve
                source
                ~name:(name target)
                ~reason:Lookup.Reason.Open
                ~span:(F.span (Lookup.source source))
                ~checkpoint
              |> F.ok
            in
            Result.map (require ~source reference) ~f:(fun _ -> ()))
      in
      match result with
      | Ok _ -> failwith "actual cyclic chain admitted"
      | Error diagnostic ->
        let path_of_site site =
          S.id (Chatml_diagnostic.Site.source site) |> S.Id.relative_path
        in
        [%test_eq: string]
          (path_of_site (Option.value_exn diagnostic.site))
          (path (total - 1));
        let expected =
          List.range prefix (total - 1)
          |> fun indices -> List.drop indices (Int.max 0 (List.length indices - 8))
        in
        [%test_eq: string list]
          (List.map diagnostic.related ~f:(fun related ->
             path_of_site (Chatml_diagnostic.Related.site related)))
          (List.map expected ~f:path);
        [%test_eq: bool]
          (String.is_substring diagnostic.message ~substring:"last 8")
          (length - 1 > 8))
;;

let%test_unit
    "graph construction rejects disconnected compiler nodes and preserves type-only \
     reachability"
  =
  let input =
    F.input
      [ "entry.chatml", "type chosen = A.token", [ "A", "a.chatml" ]
      ; "a.chatml", "type token = int", []
      ; "extra.chatml", "let value = 0", []
      ]
  in
  let result = F.checked input |> F.ok in
  let graph = Session.graph result in
  let make order =
    Graph.create
      ~input_fingerprint:(P.fingerprint input)
      ~entry:(Graph.entry graph)
      ~dependency_order:order
      ~edges:(Graph.edges graph)
      ~checkpoint
  in
  ignore (make (Graph.dependency_order graph) |> F.ok : Graph.t);
  let extra = P.Module_id.of_implementation (S.id (F.source input "extra.chatml")) in
  assert (Result.is_error (make (extra :: Graph.dependency_order graph)));
  assert (Result.is_error (make (Graph.dependency_order graph @ [ extra ])));
  assert (Result.is_error (make (List.rev (Graph.dependency_order graph))))
;;
