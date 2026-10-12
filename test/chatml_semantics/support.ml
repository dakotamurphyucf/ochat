open Core
module A = Chatml_semantics
module Q = A.Snapshot
module S = Chatml_source
module P = Chatml_prepared_program
module Contract = Chatml.Chatml_compiler_contract

let checkpoint () = ()

let contract ?(requirements = []) () =
  Contract.for_static_surface
    (Chatml.Chatml_static_surface.of_runtime_surface
       Chatml.Chatml_builtin_surface.core_surface
       ~checkpoint)
    ~required_bindings:requirements
;;

let id path =
  S.Id.create ~package_namespace:"semantic-query-test" ~relative_path:path
  |> Or_error.ok_exn
;;

let capture path contents =
  S.create (id path) ~contents ~expected_digest:None ~max_bytes:None ~checkpoint
  |> Or_error.ok_exn
;;

let input contract ?(environment = "fixture") ?(interfaces = []) files =
  let config = capture "project.json" "explicit admitted test configuration" in
  let declared_at =
    S.locate config ~start_byte:0 ~end_byte:1 ~checkpoint |> Or_error.ok_exn
  in
  let scope path bindings =
    P.Scope.create
      (id path)
      ~bindings:
        (List.map bindings ~f:(fun (name, target) ->
           P.Scope.
             { name; module_id = P.Module_id.of_implementation (id target); declared_at }))
      ~selected_at:(Some declared_at)
      ~checkpoint
    |> Or_error.ok_exn
  in
  let sources =
    config
    :: (List.map files ~f:(fun (path, body, _) -> capture path body)
        @ List.map interfaces ~f:(fun (path, body, _) -> capture (path ^ "i") body))
  in
  let definitions =
    List.map files ~f:(fun (path, _, _) ->
      P.Definition.create
        ~implementation:(id path)
        ~interface:
          (if List.exists interfaces ~f:(fun (p, _, _) -> String.equal p path)
           then Some (id (path ^ "i"))
           else None)
        ~declared_at
      |> Or_error.ok_exn)
  in
  let scopes =
    List.map files ~f:(fun (p, _, bs) -> scope p bs)
    @ List.map interfaces ~f:(fun (p, _, bs) -> scope (p ^ "i") bs)
  in
  let compiler_contract = Contract.fingerprint contract ~checkpoint in
  let environment =
    P.Environment.create
      ~name:environment
      ~required_contract:compiler_contract
      ~declared_at:(Some declared_at)
    |> Or_error.ok_exn
  in
  match
    P.create
      ~max_input_bytes:None
      ~compiler_contract
      ~environment
      ~provenance:(Project { declaration = S.id config; lock = None })
      ~entry:
        (id
           (let p, _, _ = List.hd_exn files in
            p))
      ~sources
      ~definitions
      ~scopes
      ~checkpoint
      ()
  with
  | Ok input -> input
  | Error error -> failwith error.message
;;

let limits
      ?(facts = 200_000)
      ?(edges = 400_000)
      ?(labels = 4_000_000)
      ?(work = 4_000_000)
      ?(input_bytes = 8_000_000)
      ?(nodes = 200_000)
      ()
  =
  A.Limits.create
    ~max_input_bytes:input_bytes
    ~recording:
      (Chatml_typechecker.Semantic.Limits.create
         ~max_facts:200_000
         ~max_edges:400_000
         ~max_label_bytes:4_000_000
         ~max_work_steps:4_000_000
       |> Or_error.ok_exn)
    ~type_graph:
      (A.Frozen.Limits.create
         ~max_nodes:nodes
         ~max_edges:400_000
         ~max_label_bytes:4_000_000
       |> Or_error.ok_exn)
    ~max_published_facts:facts
    ~max_published_edges:edges
    ~max_published_label_bytes:labels
    ~max_work_steps:work
  |> Or_error.ok_exn
;;

let analyze ?(limits = limits ()) ?(checkpoint = checkpoint) contract input =
  A.analyze input ~contract ~max_dependency_depth:16 ~limits ~checkpoint
;;

let complete contract input =
  match analyze contract input with
  | Ok (Complete snapshot) -> snapshot
  | Ok (Failed partial) -> failwith (List.hd_exn (A.Partial.diagnostics partial)).message
  | Error error -> failwith (Sexp.to_string_hum (A.Error.sexp_of_t error))
;;

let source input path =
  List.find_exn (P.sources input) ~f:(fun s ->
    String.equal (S.Id.relative_path (S.id s)) path)
;;

let point snapshot input path offset =
  let location =
    S.locate (source input path) ~start_byte:offset ~end_byte:offset ~checkpoint
    |> Or_error.ok_exn
  in
  Q.point snapshot location |> Result.ok |> Option.value_exn
;;

let query_limits ?(results = 100_000) ?(work = 2_000_000) () =
  Q.Query_limits.create ~max_results:results ~max_work_steps:work |> Or_error.ok_exn
;;

let references snapshot point =
  match Q.references_at snapshot point ~limits:(query_limits ()) ~checkpoint with
  | Ok (Available page) -> page.items
  | Ok (Unavailable _) | Error _ -> failwith "expected checked reference lookup"
;;

let definition snapshot input path offset =
  match
    Q.definitions_at
      snapshot
      (point snapshot input path offset)
      ~limits:(query_limits ())
      ~checkpoint
  with
  | Ok (Available { items = [ symbol ]; truncated = false }) -> symbol
  | _ -> failwith "expected one actual definition"
;;

let context snapshot point =
  match Q.context_at snapshot point ~limits:(query_limits ()) ~checkpoint with
  | Ok (Selected context) -> context
  | _ -> failwith "expected one positively established context"
;;

let names snapshot bindings =
  List.map bindings ~f:(fun binding ->
    Q.symbol snapshot (Q.Binding.symbol binding)
    |> Result.ok
    |> Option.value_exn
    |> Q.Symbol.name)
;;

let candidates snapshot context namespace =
  Q.candidates snapshot context ~namespace ~limits:(query_limits ()) ~checkpoint
  |> Result.ok
  |> Option.value_exn
  |> fun (page : Q.Binding.t Q.page) -> page.items
;;

let node snapshot id = A.Frozen.find (Q.types snapshot) id |> Option.value_exn

let rec dereference snapshot id =
  match node snapshot id with
  | Bound_link child -> dereference snapshot child
  | _ -> id
;;
