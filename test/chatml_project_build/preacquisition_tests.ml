open Core
module B = Chatml_project_build
module P = Chatml_project
module S = Chatml_source
module Contract = Chatml.Chatml_compiler_contract

module Fixture = struct
  type t =
    { root : B.Catalog_input.t
    ; catalogs : B.Catalog_input.t list
    ; leaf_declaration : S.t
    }

  let contract =
    let surface =
      Chatml.Chatml_static_surface.of_runtime_surface
        Chatml.Chatml_builtin_surface.empty
        ~checkpoint:ignore
    in
    Contract.for_static_surface surface ~required_bindings:[]
  ;;

  let owner_declaration pin =
    Support.capture
      ~namespace:"owner"
      "owner.json"
      (sprintf
         {|{"schema_version":1,"namespace":"owner","modules":{"Front":{"implementation":"front.chatml"}},"dependencies":{"leaf":{"source":{"kind":"git","repository":"https://example.invalid/leaf.git","object_format":"sha1","commit":"%s"},"project":"leaf.json"}},"environments":{"default":{"modules":{"Front":"self.Front","Imported":"leaf.Util"}}}}|}
         (String.make 40 pin))
  ;;

  let decode_owner pin =
    match
      P.decode
        (owner_declaration pin)
        ~limits:Document_schema.Limits.default
        ~checkpoint:ignore
    with
    | Ok owner -> owner
    | Error _ -> failwith "valid owner declaration rejected"
  ;;

  let make
        ?(selected_pin = 'a')
        ?captured_pin
        ?(code = "let value = 1")
        ?(companion = false)
        ()
    =
    let root_declaration =
      Support.capture
        ~namespace:"root"
        "root.json"
        {|{"schema_version":1,"namespace":"root","dependencies":{"owner":{"source":{"kind":"local","path":"./owner"},"project":"owner.json"}},"environments":{"default":{"modules":{"Front":"owner.Front"}}}}|}
    in
    let leaf_declaration =
      Support.capture
        ~namespace:"leaf"
        "leaf.json"
        {|{"schema_version":1,"namespace":"leaf","modules":{"Util":{"implementation":"util.chatml"}}}|}
    in
    let root_catalog =
      Support.catalog
        root_declaration
        ~request:(Library { environment = None })
        ~members:[]
    in
    let owner_catalog =
      Support.catalog
        (owner_declaration selected_pin)
        ~request:(Library { environment = None })
        ~members:[ Support.capture ~namespace:"owner" "front.chatml" "let value = 0" ]
    in
    let leaf_members =
      Support.capture ~namespace:"leaf" "util.chatml" code
      ::
      (if companion
       then [ Support.capture ~namespace:"leaf" "util.chatmli" "val value : int" ]
       else [])
    in
    let leaf_catalog =
      Support.catalog
        leaf_declaration
        ~request:(Library { environment = None })
        ~members:leaf_members
    in
    let association name declaration =
      P.Lock.Dependency_selection.create ~name ~declaration ~checkpoint:ignore
      |> Or_error.ok_exn
    in
    let local catalog dependencies key =
      B.Catalog_input.create catalog ~origin:Local ~acquisition_key:key ~dependencies
    in
    let pin =
      P.Git_pin.create
        ~object_format:Sha1
        ~commit:(String.make 40 (Option.value captured_pin ~default:selected_pin))
      |> Or_error.ok_exn
    in
    { root =
        local
          root_catalog
          [ association "owner" (S.id (P.source (Chatml_catalog.project owner_catalog))) ]
          "root-key"
    ; catalogs =
        [ local owner_catalog [ association "leaf" (S.id leaf_declaration) ] "owner-key"
        ; B.Catalog_input.create
            leaf_catalog
            ~origin:
              (Git
                 { repository = "https://example.invalid/leaf.git"
                 ; pin
                 ; package_root = None
                 })
            ~acquisition_key:"leaf-key"
            ~dependencies:[]
        ]
    ; leaf_declaration
    }
  ;;

  let compose fixture operation =
    let limits = Support.limits () in
    let open Result.Let_syntax in
    let%bind gate, _ = B.Selection_gate.create operation ~limits ~checkpoint:ignore in
    B.compose
      fixture.root
      ~catalogs:fixture.catalogs
      ~entry:(Captured (Support.capture ~namespace:"host" "inline.chatml" "let main = 0"))
      ~selection_gate:gate
      ~contract
      ~lock_id:(Support.id ~namespace:"root" "selected.lock.json")
      ~limits
      ~checkpoint:ignore
  ;;
end

let%test_unit
    "nested full-pin change rejects at public gate before acquiring target facts"
  =
  let original = Fixture.compose (Fixture.make ()) Initial_prepare |> Support.ok in
  let previous_lock = B.lock_source original in
  let limits = Support.limits () in
  let build =
    B.Selection_gate.create (Build { previous_lock }) ~limits ~checkpoint:ignore
    |> Support.ok
    |> fst
  in
  (* Only decode the changed Local owner's declaration here: no proposed new
     leaf catalog exists before the early host-facing gate is checked. *)
  let owner = Fixture.decode_owner 'b' in
  let dependency = List.hd_exn (P.dependencies owner) in
  [%test_eq: string] (P.Dependency.name dependency) "leaf";
  let check gate checkpoint =
    let previous_root, _ =
      B.Selection_gate.check_root
        gate
        ~origin:Local
        ~project:(Source.Portable_path.create "root.json" |> Or_error.ok_exn)
        ~limits
        ~checkpoint
      |> Support.ok
    in
    let root =
      Chatml_prepared_program.find_source
        (B.prepared original)
        (Support.id ~namespace:"root" "root.json")
      |> Option.value_exn
    in
    let root_project =
      match P.decode root ~limits:Document_schema.Limits.default ~checkpoint with
      | Ok project -> project
      | Error _ -> failwith "valid captured root rejected"
    in
    let previous_owner, _ =
      B.Selection_gate.check_dependency
        gate
        ~previous_owner:previous_root
        ~owner:root_project
        ~owner_origin:Local
        (List.hd_exn (P.dependencies root_project))
        ~limits
        ~checkpoint
      |> Support.ok
    in
    B.Selection_gate.check_dependency
      gate
      ~previous_owner
      ~owner
      ~owner_origin:Local
      dependency
      ~limits
      ~checkpoint
  in
  let rejected = check build ignore |> Support.reject in
  (match B.Error.problem rejected with
   | Needs_update -> ()
   | _ -> failwith "nested pin change classification");
  let declared =
    B.Error.declared_at rejected |> Option.value_exn |> P.Declared_at.source
  in
  Support.assert_same_source (P.source owner) declared;
  let update =
    B.Selection_gate.create
      (Update { previous_lock = Some previous_lock })
      ~limits
      ~checkpoint:ignore
    |> Support.ok
    |> fst
  in
  ignore
    (check update ignore |> Support.ok : B.Selection_gate.Owner.t option * B.Accounting.t);
  Support.assert_checkpoint_identity ~f:(fun checkpoint ->
    let error = check build checkpoint |> Support.reject in
    match B.Error.problem error with
    | Needs_update -> ()
    | _ -> failwith "gate changed during interruption");
  ignore
    (Fixture.compose
       (Fixture.make ~selected_pin:'b' ())
       (Update { previous_lock = Some previous_lock })
     |> Support.ok
     : B.t);
  let mismatch =
    Fixture.compose
      (Fixture.make ~selected_pin:'b' ~captured_pin:'a' ())
      (Update { previous_lock = Some previous_lock })
    |> Support.reject
  in
  match B.Error.problem mismatch with
  | Invalid_dependency "leaf" -> ()
  | _ -> failwith "actual captured pin mismatch admitted"
;;

let%test_unit "Build preserves overlapping Git bytes while selected inventory may change" =
  let initial = Fixture.make () in
  let original = Fixture.compose initial Initial_prepare |> Support.ok in
  let previous_lock = B.lock_source original in
  Support.assert_same_source
    previous_lock
    (B.lock_source (Fixture.compose initial (Build { previous_lock }) |> Support.ok));
  List.iter
    [ Fixture.make ~code:"let value = 9" () ]
    ~f:(fun changed ->
      let error = Fixture.compose changed (Build { previous_lock }) |> Support.reject in
      (match B.Error.problem error with
       | Lock_mismatch -> ()
       | _ -> failwith "Git captured selection drift classification");
      let owner = B.Error.declared_at error |> Option.value_exn |> P.Declared_at.source in
      Support.assert_same_source changed.Fixture.leaf_declaration owner);
  let companion =
    Fixture.compose (Fixture.make ~companion:true ()) (Build { previous_lock })
    |> Support.ok
  in
  assert (
    Option.is_some
      (Chatml_prepared_program.find_source
         (B.prepared companion)
         (Support.id ~namespace:"leaf" "util.chatmli")));
  Support.assert_same_source
    previous_lock
    (B.lock_source (Fixture.compose initial (Build { previous_lock }) |> Support.ok))
;;
