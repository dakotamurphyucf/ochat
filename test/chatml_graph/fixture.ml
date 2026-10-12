open Core
module Captured = Chatml_source
module Prepared = Chatml_prepared_program
module Lookup = Chatml_configured_lookup
module Session = Chatml_dependency_session
module Check = Chatml_configured_check
module Parse = Chatml.Chatml_parse

let checkpoint () = ()

let id path =
  Captured.Id.create ~package_namespace:"graph-test" ~relative_path:path
  |> Or_error.ok_exn
;;

let capture path contents =
  Captured.create (id path) ~contents ~expected_digest:None ~max_bytes:None ~checkpoint
  |> Or_error.ok_exn
;;

let ok = function
  | Ok value -> value
  | Error error -> failwith error.Chatml_diagnostic.message
;;

let input ?interface files =
  let configuration = capture "project.json" "explicit fixture scope" in
  let declared_at =
    Captured.locate configuration ~start_byte:0 ~end_byte:1 ~checkpoint |> Or_error.ok_exn
  in
  let sources = List.map files ~f:(fun (path, body, _) -> capture path body) in
  let interface_source =
    Option.map interface ~f:(fun (path, body) -> capture (path ^ "i") body)
  in
  let definitions =
    List.map files ~f:(fun (path, _, _) ->
      let interface =
        Option.bind interface ~f:(fun (implementation, _) ->
          if String.equal path implementation then Some (id (path ^ "i")) else None)
      in
      Prepared.Definition.create ~implementation:(id path) ~interface ~declared_at
      |> Or_error.ok_exn)
  in
  let scope path bindings =
    Prepared.Scope.create
      (id path)
      ~bindings:
        (List.map bindings ~f:(fun (name, path) ->
           Prepared.Scope.
             { name
             ; module_id = Prepared.Module_id.of_implementation (id path)
             ; declared_at
             }))
      ~selected_at:(Some declared_at)
      ~checkpoint
    |> Or_error.ok_exn
  in
  let scopes =
    List.map files ~f:(fun (path, _, bindings) -> scope path bindings)
    @ Option.to_list (Option.map interface ~f:(fun (path, _) -> scope (path ^ "i") []))
  in
  let contract = Captured.Digest.of_contents "fixture contract" ~checkpoint in
  let entry, _, _ = List.hd_exn files in
  match
    Prepared.create
      ~max_input_bytes:None
      ~compiler_contract:contract
      ~environment:(Prepared.Environment.implicit ~required_contract:contract)
      ~provenance:(Project { declaration = Captured.id configuration; lock = None })
      ~entry:(id entry)
      ~sources:((configuration :: sources) @ Option.to_list interface_source)
      ~definitions
      ~scopes
      ~checkpoint
      ()
  with
  | Ok input -> input
  | Error error -> failwith error.message
;;

let source input path = Prepared.find_source input (id path) |> Option.value_exn

let context input path =
  Lookup.create
    input
    ~owner:(Prepared.Module_id.of_implementation (id path))
    ~source:(id path)
    ~checkpoint
  |> ok
;;

let span source =
  Captured.locate source ~start_byte:0 ~end_byte:(Captured.byte_length source) ~checkpoint
  |> Or_error.ok_exn
  |> Captured.Location.span
;;

let checked ?(checkpoint = checkpoint) ?(max_dependency_depth = 16) input =
  let engine = Check.create Chatml.Chatml_builtin_surface.core_surface ~checkpoint in
  Session.run input ~max_dependency_depth ~checkpoint ~check:(fun unit ~require ->
    let source = Session.Unit.implementation unit in
    let open Result.Let_syntax in
    let%bind parsed = Parse.parse_captured (Lookup.source source) ~checkpoint in
    Check.check_unit
      engine
      ~source
      ~require:(fun ~source reference ->
        Result.map (require ~source reference) ~f:Check.Checked_unit.exports)
      parsed)
;;

let paths ids =
  List.map ids ~f:(fun id ->
    Prepared.Module_id.implementation id |> Captured.Id.relative_path)
;;
