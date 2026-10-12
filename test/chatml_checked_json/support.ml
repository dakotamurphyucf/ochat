open Core
module S = Chatml_source
module P = Chatml_prepared_program
module B = Chatml.Chatml_builtin_surface
module C = Chatml.Chatml_compiler_contract
module Host = Chatml_host_runtime

let checkpoint () = ()

let id namespace path =
  S.Id.create ~package_namespace:namespace ~relative_path:path |> Or_error.ok_exn
;;

let capture namespace path contents ~checkpoint =
  S.create
    (id namespace path)
    ~contents
    ~expected_digest:None
    ~max_bytes:(Some 65_536)
    ~checkpoint
  |> Or_error.ok_exn
;;

type t =
  { declaration : S.t
  ; implementation : S.t
  ; interface : S.t
  }

let load () =
  Eio_main.run (fun env ->
    let root = Eio.Stdenv.fs env in
    let source path =
      capture
        "ochat.examples.checked-json"
        path
        (Eio.Path.load Eio.Path.(root / Filename.basename path))
        ~checkpoint
    in
    { declaration = source "chatml.project.json"
    ; implementation = source "src/checked_json.chatml"
    ; interface = source "src/checked_json.chatmli"
    })
;;

let compile t ?(surface = B.core_surface) ~source ~checkpoint () =
  let contract =
    C.for_static_surface
      (Chatml.Chatml_static_surface.of_runtime_surface surface ~checkpoint)
      ~required_bindings:
        [ ("main", Chatml.Chatml_builtin_spec.(TFun ([], task_ty (TVar "decoder_result"))))
        ]
  in
  let fingerprint = C.fingerprint contract ~checkpoint in
  let entry = capture "checked-json-consumer" "consumer.chatml" source ~checkpoint in
  let declared_at =
    S.locate
      t.declaration
      ~start_byte:0
      ~end_byte:(S.byte_length t.declaration)
      ~checkpoint
    |> Or_error.ok_exn
  in
  let definition =
    P.Definition.create
      ~implementation:(S.id t.implementation)
      ~interface:(Some (S.id t.interface))
      ~declared_at
    |> Or_error.ok_exn
  in
  let binding =
    P.Scope.{ name = "Checked_json"; module_id = P.Definition.id definition; declared_at }
  in
  let scope source bindings =
    P.Scope.create (S.id source) ~bindings ~selected_at:(Some declared_at) ~checkpoint
    |> Or_error.ok_exn
  in
  let prepared =
    P.create
      ~max_input_bytes:(Some 262_144)
      ~compiler_contract:fingerprint
      ~environment:(P.Environment.implicit ~required_contract:fingerprint)
      ~provenance:(Project { declaration = S.id t.declaration; lock = None })
      ~entry:(S.id entry)
      ~sources:[ t.declaration; t.implementation; t.interface; entry ]
      ~definitions:[ definition ]
      ~scopes:
        [ scope entry [ binding ]
        ; scope t.implementation [ binding ]
        ; scope t.interface [ binding ]
        ]
      ~checkpoint
      ()
    |> Result.map_error ~f:(fun error -> Chatml_diagnostic.create error.P.Error.message)
  in
  Result.bind prepared ~f:(fun prepared ->
    Chatml_linked_program.compile prepared ~contract ~max_dependency_depth:16 ~checkpoint
    |> Result.map_error ~f:Chatml_linked_program.Failure.diagnostic)
;;

let run t ~source =
  Result.bind (compile t ~source ~checkpoint ()) ~f:(fun program ->
    Host.run_prepared_entrypoint_detailed
      (Host.default_runtime_config ~surface:B.core_surface ())
      program
      ~entrypoint:"main"
      ~arguments:[]
      ())
;;
