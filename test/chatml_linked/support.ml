open Core
module S = Chatml_source
module P = Chatml_prepared_program
module Contract = Chatml.Chatml_compiler_contract

let checkpoint () = ()

let native_contract ?(requirements = []) () =
  Contract.for_static_surface
    (Chatml.Chatml_static_surface.of_runtime_surface
       Chatml.Chatml_builtin_surface.core_surface
       ~checkpoint)
    ~required_bindings:requirements
;;

let id path =
  S.Id.create ~package_namespace:"linked-test" ~relative_path:path |> Or_error.ok_exn
;;

let capture path contents =
  S.create (id path) ~contents ~expected_digest:None ~max_bytes:None ~checkpoint
  |> Or_error.ok_exn
;;

let input contract ?(interfaces = []) files =
  let config = capture "project.json" "explicit fixture scopes" in
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
  match
    P.create
      ~max_input_bytes:None
      ~compiler_contract
      ~environment:(P.Environment.implicit ~required_contract:compiler_contract)
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

let compiled contract input =
  match
    Chatml_linked_program.compile input ~contract ~max_dependency_depth:16 ~checkpoint
  with
  | Ok program -> program
  | Error failure -> failwith (Chatml_linked_program.Failure.diagnostic failure).message
;;

let paths ids =
  List.map ids ~f:(fun id -> S.Id.relative_path (P.Module_id.implementation id))
;;

let owner diagnostic =
  let site = Option.value_exn diagnostic.Chatml_diagnostic.site in
  S.Id.relative_path (S.Location.source (Chatml_diagnostic.Site.location site))
;;
