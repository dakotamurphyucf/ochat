open Core
module B = Chatml_project_build
module P = Chatml_project
module S = Chatml_source
module C = Chatml_catalog
module R = Chatml_prepared_program

let path value = Source.Portable_path.create value |> Or_error.ok_exn

let%test_unit "Git root configuration changes preserve code selection, not namespace IDs" =
  let original = Gate_tests.compose (Gate_tests.git 'a') Initial_prepare |> Support.ok in
  let previous_lock = B.lock_source original in
  List.iter [ "owner"; "renamed" ] ~f:(fun namespace ->
    List.iter [ "default"; "alternate" ] ~f:(fun environment ->
      let changed =
        Gate_tests.compose
          ~namespace
          ~environment
          (Gate_tests.git 'a')
          (Build { previous_lock })
        |> Support.ok
      in
      [%test_eq: string]
        (R.Environment.name (R.environment (B.prepared changed)))
        environment;
      [%test_eq: string] (S.Id.package_namespace (P.Lock.root (B.lock changed))) namespace;
      let source =
        R.find_source (B.prepared changed) (P.Lock.root (B.lock changed))
        |> Option.value_exn
      in
      let previous =
        R.find_source (B.prepared original) (P.Lock.root (B.lock original))
        |> Option.value_exn
      in
      assert (S.Digest.equal (S.digest source) (S.digest previous))))
;;

module Fixture = struct
  type package =
    { project : P.t
    ; input : B.Catalog_input.t
    }

  type t =
    { root : package
    ; owners : package list
    ; libraries : package list
    }

  let association name namespace =
    P.Lock.Dependency_selection.create
      ~name
      ~declaration:(Support.id ~namespace "project.json")
      ~checkpoint:ignore
    |> Or_error.ok_exn
  ;;

  let package ~namespace ~wire ~environment ~members ~origin ~dependencies ~key =
    let catalog =
      Support.catalog
        (Support.capture ~namespace "project.json" wire)
        ~request:(Library { environment = Some environment })
        ~members
    in
    { project = C.project catalog
    ; input = B.Catalog_input.create catalog ~origin ~dependencies ~acquisition_key:key
    }
  ;;

  let repository = "https://example.invalid/shared.git"

  let git pin =
    P.Lock.Catalog.Git
      { repository
      ; pin =
          P.Git_pin.create ~object_format:Sha1 ~commit:(String.make 40 pin)
          |> Or_error.ok_exn
      ; package_root = None
      }
  ;;

  let make
        ?(renamed = false)
        ?(environment = "blue")
        ?(merge = false)
        ?(left_pin = 'b')
        ()
    =
    let root_namespace = if renamed then "root.new" else "root" in
    let owner_namespace side =
      if merge then "merged.owner" else side ^ if renamed then ".owner.new" else ".owner"
    in
    let library_namespace side =
      if merge then "merged.lib" else side ^ if renamed then ".lib.new" else ".lib"
    in
    let make_branch side pin =
      let library_namespace = library_namespace side in
      let library =
        package
          ~namespace:library_namespace
          ~wire:
            {|{"schema_version":1,"namespace":"authored.library","modules":{"Util":{"implementation":"util.chatml"}},"environments":{"blue":{},"green":{}}}|}
          ~environment
          ~members:
            [ Support.capture ~namespace:library_namespace "util.chatml" "let value = 1" ]
          ~origin:(git pin)
          ~dependencies:[]
          ~key:("git-" ^ library_namespace)
      in
      let owner =
        package
          ~namespace:(owner_namespace side)
          ~wire:
            (sprintf
               {|{"schema_version":1,"namespace":"authored.owner","modules":{"Front":{"implementation":"front.chatml"}},"dependencies":{"lib":{"source":{"kind":"git","repository":"%s","object_format":"sha1","commit":"%s"},"project":"project.json","namespace":"%s","environment":"%s"}},"environments":{"blue":{"modules":{"Front":"self.Front","Imported":"lib.Util"}},"green":{"modules":{"Front":"self.Front","Imported":"lib.Util"}}}}|}
               repository
               (String.make 40 pin)
               library_namespace
               environment)
          ~environment
          ~members:
            [ Support.capture
                ~namespace:(owner_namespace side)
                "front.chatml"
                "let value = 0"
            ]
          ~origin:Local
          ~dependencies:[ association "lib" library_namespace ]
          ~key:("local-" ^ owner_namespace side)
      in
      owner, library
    in
    let left, left_library = make_branch "left" (if merge then 'a' else left_pin) in
    let right, right_library =
      if merge then left, left_library else make_branch "right" 'a'
    in
    let root =
      package
        ~namespace:root_namespace
        ~wire:
          (sprintf
             {|{"schema_version":1,"namespace":"authored.root","dependencies":{"A":{"source":{"kind":"local","path":"./left"},"project":"project.json","namespace":"%s","environment":"%s"},"B":{"source":{"kind":"local","path":"%s"},"project":"project.json","namespace":"%s","environment":"%s"}},"environments":{"blue":{"modules":{"AFront":"A.Front","BFront":"B.Front"}},"green":{"modules":{"AFront":"A.Front","BFront":"B.Front"}}}}|}
             (owner_namespace "left")
             environment
             (if merge then "./left" else "./right")
             (owner_namespace "right")
             environment)
        ~environment
        ~members:[]
        ~origin:Local
        ~dependencies:
          [ association "A" (owner_namespace "left")
          ; association "B" (owner_namespace "right")
          ]
        ~key:"root"
    in
    { root
    ; owners = (if merge then [ left ] else [ left; right ])
    ; libraries = (if merge then [ left_library ] else [ left_library; right_library ])
    }
  ;;

  let compose fixture operation =
    let limits = Support.limits () in
    let open Result.Let_syntax in
    let%bind gate, _ = B.Selection_gate.create operation ~limits ~checkpoint:ignore in
    B.compose
      fixture.root.input
      ~catalogs:
        (List.map (fixture.owners @ fixture.libraries) ~f:(fun package -> package.input))
      ~entry:(Captured (Support.capture ~namespace:"host" "entry.chatml" "let main = 0"))
      ~selection_gate:gate
      ~contract:Gate_tests.contract
      ~lock_id:
        (Support.id ~namespace:(P.namespace fixture.root.project) "selected.lock.json")
      ~limits
      ~checkpoint:ignore
  ;;
end

let selection_gate build =
  B.Selection_gate.create
    (Build { previous_lock = B.lock_source build })
    ~limits:(Support.limits ())
    ~checkpoint:ignore
  |> Support.ok
  |> fst
;;

let root_owner gate =
  B.Selection_gate.check_root
    gate
    ~origin:Local
    ~project:(path "project.json")
    ~limits:(Support.limits ())
    ~checkpoint:ignore
  |> Support.ok
  |> fst
;;

let dependency owner name =
  List.find_exn (P.dependencies owner) ~f:(fun dependency ->
    String.equal (P.Dependency.name dependency) name)
;;

let check gate previous_owner owner name checkpoint =
  B.Selection_gate.check_dependency
    gate
    ~previous_owner
    ~owner
    ~owner_origin:Local
    (dependency owner name)
    ~limits:(Support.limits ())
    ~checkpoint
;;

let%expect_test "renamed nested owners preserve their own Git selection routes" =
  let original = Fixture.compose (Fixture.make ()) Initial_prepare |> Support.ok in
  let previous_lock = B.lock_source original in
  List.iter [ false; true ] ~f:(fun renamed ->
    List.iter [ "blue"; "green" ] ~f:(fun environment ->
      let changed = Fixture.make ~renamed ~environment () in
      let built = Fixture.compose changed (Build { previous_lock }) |> Support.ok in
      List.iter changed.libraries ~f:(fun library ->
        let id = S.id (P.source library.Fixture.project) in
        let selected =
          List.find_exn
            (P.Lock.catalogs (B.lock built))
            ~f:(fun catalog -> S.Id.equal (P.Lock.Catalog.declaration catalog) id)
        in
        [%test_eq: string] (P.Lock.Catalog.selected_environment selected) environment);
      let root_catalog =
        List.find_exn
          (P.Lock.catalogs (B.lock built))
          ~f:(fun catalog ->
            S.Id.equal (P.Lock.Catalog.declaration catalog) (P.Lock.root (B.lock built)))
      in
      printf
        "%s/%s: %s\n"
        (if renamed then "renamed" else "original")
        environment
        (P.Lock.Catalog.selected_environment root_catalog)));
  [%expect
    {|original/blue: blue
original/green: green
renamed/blue: blue
renamed/green: green|}]
;;

let%test_unit
    "explicit prior route rejects changed pins and foreign owners before acquisition"
  =
  let original = Fixture.compose (Fixture.make ()) Initial_prepare |> Support.ok in
  let gate = selection_gate original in
  let changed = Fixture.make ~renamed:true ~environment:"green" ~left_pin:'c' () in
  let previous, _ =
    check gate (root_owner gate) changed.root.project "A" ignore |> Support.ok
  in
  let owner = (List.hd_exn changed.owners).project in
  let acquire = ref 0 in
  let result = check gate previous owner "lib" ignore in
  (match result with
   | Ok _ -> incr acquire
   | Error _ -> ());
  [%test_eq: int] !acquire 0;
  (match B.Error.problem (Support.reject result) with
   | Needs_update -> ()
   | _ -> failwith "pin classification");
  let foreign = selection_gate original in
  let foreign_owner = root_owner foreign in
  let rejected =
    check gate foreign_owner changed.root.project "A" ignore |> Support.reject
  in
  (match B.Error.problem rejected with
   | Invalid_dependency "A" -> ()
   | _ -> failwith "foreign affinity");
  Support.assert_checkpoint_identity ~f:(fun checkpoint ->
    ignore (check gate previous owner "lib" checkpoint |> Support.reject))
;;

let%test_unit "converging current aliases validate both distinct prior owner routes" =
  let original = Fixture.compose (Fixture.make ()) Initial_prepare |> Support.ok in
  let merged = Fixture.make ~renamed:true ~environment:"green" ~merge:true () in
  let error =
    Fixture.compose merged (Build { previous_lock = B.lock_source original })
    |> Support.reject
  in
  (match B.Error.problem error with
   | Needs_update -> ()
   | _ -> failwith "prior route merge drift");
  ignore
    (Fixture.compose merged (Update { previous_lock = Some (B.lock_source original) })
     |> Support.ok
     : B.t)
;;

let%test_unit "dependency configuration cross product never relaxes code intent" =
  let original = Fixture.compose (Fixture.make ()) Initial_prepare |> Support.ok in
  let gate = selection_gate original in
  let current = Fixture.make ~renamed:true () in
  let previous, _ =
    check gate (root_owner gate) current.root.project "A" ignore |> Support.ok
  in
  let git repository format commit =
    sprintf
      {|{"kind":"git","repository":"%s","object_format":"%s","commit":"%s"}|}
      repository
      format
      commit
  in
  let same = git Fixture.repository "sha1" (String.make 40 'b') in
  let cases =
    [ same, "project.json", true
    ; git Fixture.repository "sha1" (String.make 40 'c'), "project.json", false
    ; ( git "https://example.invalid/other.git" "sha1" (String.make 40 'b')
      , "project.json"
      , false )
    ; git Fixture.repository "sha256" (String.make 64 'b'), "project.json", false
    ; same, "other.json", false
    ; {|{"kind":"local","path":"./replacement"}|}, "project.json", false
    ]
  in
  List.iter [ "blue"; "green" ] ~f:(fun environment ->
    List.iter [ "left.lib"; "left.lib.new" ] ~f:(fun namespace ->
      List.iter cases ~f:(fun (source, project, admitted) ->
        let wire =
          sprintf
            {|{"schema_version":1,"namespace":"authored.owner","dependencies":{"lib":{"source":%s,"project":"%s","environment":"%s","namespace":"%s"}}}|}
            source
            project
            environment
            namespace
        in
        let owner =
          match
            P.decode
              (Support.capture ~namespace:"left.owner.new" "project.json" wire)
              ~limits:Document_schema.Limits.default
              ~checkpoint:ignore
          with
          | Ok owner -> owner
          | Error _ -> failwith "handwritten declaration rejected"
        in
        let calls = ref 0 in
        let result = check gate previous owner "lib" ignore in
        (match result with
         | Ok _ -> incr calls
         | Error error ->
           (match B.Error.problem error with
            | Needs_update -> ()
            | _ -> failwith "code-intent gate classification"));
        [%test_eq: int] !calls (if admitted then 1 else 0))))
;;

let%test_unit "newly selected Git cannot borrow another prior owner's selection" =
  let original = Fixture.compose (Fixture.make ()) Initial_prepare |> Support.ok in
  let gate = selection_gate original in
  let owner = (List.hd_exn (Fixture.make ~renamed:true ()).owners).project in
  let error = check gate None owner "lib" ignore |> Support.reject in
  match B.Error.problem error with
  | Needs_update -> ()
  | _ -> failwith "new Git selected"
;;

let%test_unit
    "Build declaration cycles and converging routes retain bounded work and cancellation"
  =
  let module Model = Closure_tests.Owner_model in
  let fixture = Model.make ~root_back:true () in
  let original = Model.compose fixture |> Support.ok in
  let gate = selection_gate original in
  let compose limits checkpoint =
    B.compose
      (Closure_tests.Package.input fixture.root)
      ~catalogs:
        (List.map
           [ fixture.a; fixture.b; fixture.a_shared; fixture.b_shared ]
           ~f:Closure_tests.Package.input)
      ~entry:Declared
      ~selection_gate:gate
      ~contract:fixture.contract
      ~lock_id:(Support.id ~namespace:"root" "selected.lock.json")
      ~limits
      ~checkpoint
  in
  let polls = ref 0 in
  let built = compose (Support.limits ()) (fun () -> incr polls) |> Support.ok in
  let work = B.Accounting.work_steps (B.accounting built) in
  [%test_eq: int] !polls work;
  assert (work > 1);
  let exact = compose (Support.limits ~steps:work ()) ignore |> Support.ok in
  Support.assert_same_source (B.lock_source built) (B.lock_source exact);
  let error = compose (Support.limits ~steps:(work - 1) ()) ignore |> Support.reject in
  (match B.Error.problem error with
   | Limit_exceeded -> ()
   | _ -> failwith "pair-walk work bound");
  (* The same cycle fixture is reusable after interruption at independently
     chosen checkpoints before, during and after the observed traversal. *)
  List.iter
    [ 1; work / 2; work ]
    ~f:(fun stop ->
      let exception Stop of unit ref in
      let token = ref () in
      let count = ref 0 in
      match
        compose (Support.limits ()) (fun () ->
          incr count;
          if !count = stop then raise (Stop token))
      with
      | _ -> failwith "checkpoint swallowed"
      | exception Stop actual -> assert (phys_equal token actual));
  Support.assert_same_source
    (B.lock_source built)
    (B.lock_source (compose (Support.limits ()) ignore |> Support.ok))
;;
