open Core
module Model = Project_build_model
module Budget = Project_build_budget
module Scope = Project_build_scope
module Input = Model.Catalog_input
module Project = Chatml_project
module Lock = Project.Lock
module Catalog = Chatml_catalog
module Captured = Chatml_source
module Prepared = Chatml_prepared_program

type t =
  { catalog : Lock.Catalog.t
  ; scopes : Prepared.Scope.t list
  }

let catalog t = t.catalog
let scopes t = t.scopes

let source_entry source =
  (* Captured guarantees a valid identity/digest and nonnegative byte length. *)
  Lock.Source_entry.create
    (Captured.id source)
    ~digest:(Captured.digest source)
    ~byte_length:(Captured.byte_length source)
  |> Or_error.ok_exn
;;

let create budget input ~catalogs ~contract ~fingerprint =
  let checkpoint () = Budget.checkpoint budget in
  let declared_at = Input.declared_at input in
  let invalid result =
    Result.map_error result ~f:(fun _ -> Model.Error.create ~declared_at Lock_mismatch)
  in
  let declaration result =
    Result.map_error result ~f:(fun error -> Model.Error.create (Declaration error))
  in
  let open Result.Let_syntax in
  let%bind () =
    List.fold_result
      (Scope.environments input.catalog ~checkpoint)
      ~init:()
      ~f:(fun () scope ->
        Scope.check_requirement scope ~contract ~fingerprint ~checkpoint)
  in
  let%bind scopes, receipts =
    List.fold_result
      (Catalog.sources input.catalog)
      ~init:([], [])
      ~f:(fun (scopes, receipts) source ->
        checkpoint ();
        match
          Catalog.scope_for ~checkpoint input.catalog ~source:(Captured.id source)
        with
        | None -> Ok (scopes, receipts)
        | Some scope ->
          let%bind bindings = Scope.bindings budget input ~catalogs scope in
          let%bind prepared =
            Prepared.Scope.create
              (Captured.id source)
              ~bindings
              ~selected_at:
                (Option.map
                   (Project.Scope_spec.declared_at scope)
                   ~f:Project.Declared_at.location)
              ~checkpoint
            |> invalid
          in
          let%map receipt =
            Lock.File_scope.create
              (Captured.id source)
              ~environment:(Project.Scope_spec.name scope)
              ~bindings:
                (List.map bindings ~f:(fun binding ->
                   checkpoint ();
                   { Lock.File_scope.name = binding.name; module_id = binding.module_id }))
              ~checkpoint
            |> invalid
          in
          prepared :: scopes, receipt :: receipts)
  in
  let%bind modules =
    List.map (Catalog.definitions input.catalog) ~f:(fun definition ->
      checkpoint ();
      Lock.Module_selection.create
        ~implementation:(Prepared.Definition.implementation definition)
        ~interface:(Prepared.Definition.interface definition)
      |> invalid)
    |> Result.all
  in
  let%map catalog =
    Lock.Catalog.create
      ~declared_at
      ~namespace:(Captured.Id.package_namespace (Input.declaration input))
      ~origin:input.origin
      ~declaration:(Input.declaration input)
      ~selected_environment:
        (Project.Scope_spec.name
           (Project.Selection.environment (Catalog.selection input.catalog)))
      ~dependencies:input.dependencies
      ~sources:
        (List.map (Catalog.sources input.catalog) ~f:(fun source ->
           checkpoint ();
           source_entry source))
      ~modules
      ~scopes:receipts
      ~checkpoint
    |> declaration
  in
  { catalog; scopes = List.rev scopes }
;;
