open Core
module S = Chatml_source
module P = Chatml_prepared_program
module Lookup = Chatml_configured_lookup
module Session = Chatml_dependency_session
module Contract = Chatml.Chatml_compiler_contract
module Parse = Chatml.Chatml_parse
module Typing = Chatml_repl_typing
module Registry = Chatml_linked_runtime.Repl_modules
module Surface = Chatml.Chatml_extension_surface
module Execution = Chatml_execution
module Lang = Chatml.Chatml_lang
module B = Chatml.Chatml_builtin_spec

exception Interrupted of int ref

let checkpoint () = ()

let id path =
  S.Id.create ~package_namespace:"repl-runtime-test" ~relative_path:path
  |> Or_error.ok_exn
;;

let capture path contents =
  S.create (id path) ~contents ~expected_digest:None ~max_bytes:None ~checkpoint
  |> Or_error.ok_exn
;;

let ok_typing = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Typing.Error.t)]
;;

let ok_registry = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Registry.Error.t)]
;;

let owner ~catalog_body ~contract ?before_body () =
  let digest = Contract.fingerprint contract ~checkpoint in
  let configuration = capture "project.json" "selected scope" in
  let declared_at =
    S.locate configuration ~start_byte:0 ~end_byte:1 ~checkpoint |> Or_error.ok_exn
  in
  let entry = capture "entry.chatml" "let main = 1 / 0" in
  let catalog = capture "catalog.chatml" catalog_body in
  let before = Option.map before_body ~f:(capture "before.chatml") in
  let sources = entry :: catalog :: Option.to_list before in
  let definitions =
    List.map sources ~f:(fun source ->
      P.Definition.create ~implementation:(S.id source) ~interface:None ~declared_at
      |> Or_error.ok_exn)
  in
  let scope source bindings =
    P.Scope.create (S.id source) ~bindings ~selected_at:(Some declared_at) ~checkpoint
    |> Or_error.ok_exn
  in
  let scopes =
    [ scope
        entry
        [ { P.Scope.name = "Catalog"
          ; module_id = P.Module_id.of_implementation (S.id catalog)
          ; declared_at
          }
        ]
    ; scope
        catalog
        (match before with
         | None -> []
         | Some source ->
           [ { P.Scope.name = "Before"
             ; module_id = P.Module_id.of_implementation (S.id source)
             ; declared_at
             }
           ])
    ]
    @ List.map (Option.to_list before) ~f:(fun source -> scope source [])
  in
  let prepared =
    P.create
      ~max_input_bytes:None
      ~compiler_contract:digest
      ~environment:(P.Environment.implicit ~required_contract:digest)
      ~provenance:(Project { declaration = S.id configuration; lock = None })
      ~entry:(S.id entry)
      ~sources:(configuration :: sources)
      ~definitions
      ~scopes
      ~checkpoint
      ()
    |> function
    | Ok input -> input
    | Error error -> failwith error.P.Error.message
  in
  let repl_scope =
    Lookup.Repl_scope.create
      prepared
      ~anchor_owner:(P.Module_id.of_implementation (S.id entry))
      ~anchor_implementation:(S.id entry)
      ~limits:
        (Lookup.Repl_scope.Limits.create ~max_snippets:50 ~max_identity_bytes:5000
         |> Or_error.ok_exn)
      ~checkpoint
    |> function
    | Ok scope -> scope
    | Error error -> raise_s [%sexp (error : Lookup.Repl_scope.Error.t)]
  in
  let parse =
    Parse.Submission.Limits.create
      ~max_source_bytes:10000
      ~max_tokens:10000
      ~max_work_steps:100000
      ~sites:
        (Chatml.Chatml_syntax_sites.Limits.create ~max_sites:10000 ~max_name_bytes:100000
         |> Or_error.ok_exn)
    |> Or_error.ok_exn
  in
  let limits =
    Typing.Limits.create
      ~parse
      ~display:
        (Chatml_typechecker.Type_snapshot.Limits.create
           ~max_nodes:100000
           ~max_edges:100000
           ~max_label_bytes:100000
         |> Or_error.ok_exn)
      ~max_display_name_bytes:100000
      ~max_statements:30
      ~root_copy:
        (Chatml_typechecker.Repl_type_roots.Limits.create
           ~max_bindings:1000
           ~max_type_roots:2000
           ~max_type_nodes:100000
           ~max_variable_cells:10000
           ~max_type_edges:100000
           ~max_copy_work:300000
           ~max_copy_depth:256
           ~max_name_work:300000
           ~max_name_bytes:100000
         |> Or_error.ok_exn)
    |> Or_error.ok_exn
  in
  Typing.create
    ~prepared
    ~scope:repl_scope
    ~contract
    ~root_limits:
      (Session.Roots.Limits.create
         ~max_checked_modules:20
         ~max_edges:50
         ~max_dependency_depth:20
       |> Or_error.ok_exn)
    ~limits
    ~checkpoint
  |> ok_typing
;;

let candidate typing path body =
  let source = capture path body in
  let ready =
    match Typing.admit typing source ~checkpoint |> ok_typing with
    | Ready ready -> ready
    | Need_more _ | Rejected _ -> failwith "fixture was not a complete submission"
  in
  Typing.prepare_next typing ready ~checkpoint |> ok_typing
;;

let finish admission outcome =
  Typing.finalize_execution (Registry.Admission.typing_reservation admission) ~outcome
  |> ok_typing
  |> ignore
;;

let initialized registry admission runner =
  match
    Execution.run_scoped runner (fun () -> Registry.initialize registry admission)
  with
  | Ok result -> ok_registry result
  | Error error -> raise_s [%sexp (error : Execution.error)]
;;

let catalog_id = P.Module_id.of_implementation (id "catalog.chatml")
let before_id = P.Module_id.of_implementation (id "before.chatml")

let label_status = function
  | None -> "absent"
  | Some Registry.Status.Pending -> "pending"
  | Some Initializing -> "initializing"
  | Some Initialized -> "initialized"
  | Some (Faulted _) -> "faulted"
;;

let%expect_test "inert project entry, type-only open and one persistent instance" =
  Eio_main.run (fun env ->
    let typing =
      owner
        ~catalog_body:"let value = ref 0"
        ~contract:(Contract.for_target One_off_v1 ~checkpoint)
        ()
    in
    let runner =
      Execution.create_runner ~env ~policy:(Bounded Execution.default_limits) ()
    in
    let registry =
      Registry.create
        ~typing
        ~surface:Surface.one_off_v1
        ~control:(Execution.runner_control runner)
        ~checkpoint
      |> ok_registry
    in
    print_s [%sexp (Registry.status registry catalog_id : Registry.Status.t option)];
    let opened = candidate typing "open.chatml" "open Catalog" in
    let admission = Registry.reserve registry opened ~checkpoint |> ok_registry in
    let modules = initialized registry admission runner in
    let status = Registry.status registry catalog_id in
    let unavailable =
      Result.is_error
        (Lang.Configured_modules.read
           modules
           (Chatml.Chatml_module_reference.create
              (id "catalog.chatml")
              ~member_path:[ "value" ]
            |> Or_error.ok_exn))
    in
    print_s [%sexp (status : Registry.Status.t option), (unavailable : bool)];
    finish admission Typing.Published.After_success;
    let first = candidate typing "first.chatml" "Catalog.value" in
    let first_admission = Registry.reserve registry first ~checkpoint |> ok_registry in
    let first_modules = initialized registry first_admission runner in
    let reference =
      Chatml.Chatml_module_reference.create (id "catalog.chatml") ~member_path:[ "value" ]
      |> Or_error.ok_exn
    in
    let first_cell =
      match Lang.Configured_modules.read first_modules reference with
      | Ok (Lang.VRef cell) -> cell
      | Ok _ | Error _ -> failwith "expected configured reference"
    in
    first_cell := Lang.VInt 7;
    finish first_admission Typing.Published.After_success;
    let second = candidate typing "second.chatml" "Catalog.value" in
    let second_admission = Registry.reserve registry second ~checkpoint |> ok_registry in
    let second_modules = initialized registry second_admission runner in
    let same_cell =
      match Lang.Configured_modules.read second_modules reference with
      | Ok (Lang.VRef cell) -> phys_equal first_cell cell
      | Ok _ | Error _ -> false
    in
    let status = Registry.status registry catalog_id in
    print_s [%sexp (status : Registry.Status.t option), (same_cell : bool)];
    finish second_admission Typing.Published.After_success;
    Registry.close registry |> ok_registry);
  [%expect
    {|
    ()
    ((Pending) true)
    ((Initialized) true)
    |}]
;;

let%expect_test "completed predecessor survives failed initializer; fault never replays" =
  Eio_main.run (fun env ->
    let log = ref [] in
    let surface =
      Runtime_tests.marked_surface log Chatml.Chatml_builtin_surface.core_surface
    in
    let contract = Support.native_contract ~surface () in
    let typing =
      owner
        ~catalog_body:
          "let prior = Before.value let mark_failed = mark(\"catalog\") let value = 1 / 0"
        ~before_body:"let mark_before = mark(\"before\") let value = 3"
        ~contract
        ()
    in
    let runner =
      Execution.create_runner ~env ~policy:(Bounded Execution.default_limits) ()
    in
    let registry =
      Registry.create
        ~typing
        ~surface
        ~control:(Execution.runner_control runner)
        ~checkpoint
      |> ok_registry
    in
    let first = candidate typing "fault-first.chatml" "Catalog.value" in
    let admission = Registry.reserve registry first ~checkpoint |> ok_registry in
    (match
       Execution.run_scoped runner (fun () -> Registry.initialize registry admission)
     with
     | Ok (Error (Registry.Error.Runtime { module_id; diagnostic })) ->
       let correct_module = P.Module_id.equal module_id catalog_id in
       let catalog_source =
         P.find_source (Typing.prepared typing) (id "catalog.chatml") |> Option.value_exn
       in
       let source_owned =
         Option.value_map diagnostic.site ~default:false ~f:(fun site ->
           let location = Chatml_diagnostic.Site.location site in
           S.Id.equal (S.Location.source location) (S.id catalog_source)
           && S.Digest.equal (S.Location.source_digest location) (S.digest catalog_source))
       in
       print_s [%sexp (correct_module : bool), (source_owned : bool)]
     | Ok _ | Error _ -> failwith "expected source-owned catalog runtime failure");
    finish admission Typing.Published.Constrained_before_runtime_failure;
    let observed = List.rev !log in
    let before_status = label_status (Registry.status registry before_id) in
    let catalog_status = label_status (Registry.status registry catalog_id) in
    print_s
      [%sexp
        (observed : string list), (before_status : string), (catalog_status : string)];
    let repeated = candidate typing "fault-repeat.chatml" "Catalog.value" in
    let repeated_admission =
      Registry.reserve registry repeated ~checkpoint |> ok_registry
    in
    (match
       Execution.run_scoped runner (fun () ->
         Registry.initialize registry repeated_admission)
     with
     | Ok (Error (Registry.Error.Faulted { module_id; _ })) ->
       assert (P.Module_id.equal module_id catalog_id)
     | Ok _ | Error _ -> failwith "expected prior module fault");
    finish repeated_admission Typing.Published.Constrained_before_runtime_failure;
    let pure = candidate typing "fault-unrelated.chatml" "42" in
    let pure_admission = Registry.reserve registry pure ~checkpoint |> ok_registry in
    ignore (initialized registry pure_admission runner : Lang.Configured_modules.t);
    finish pure_admission Typing.Published.After_success;
    let observed = List.rev !log in
    print_s [%sexp (observed : string list)];
    Registry.close registry |> ok_registry);
  [%expect
    {|
    (true true)
    ((before catalog) initialized faulted)
    (before catalog)
    |}]
;;

let%test_unit "cancellation before a unit begins keeps it pending" =
  Eio_main.run (fun env ->
    let typing =
      owner
        ~catalog_body:"let value = ref 0"
        ~contract:(Contract.for_target One_off_v1 ~checkpoint)
        ()
    in
    let runner =
      Execution.create_runner ~env ~policy:(Bounded Execution.default_limits) ()
    in
    let registry =
      Registry.create
        ~typing
        ~surface:Surface.one_off_v1
        ~control:(Execution.runner_control runner)
        ~checkpoint
      |> ok_registry
    in
    let selected = candidate typing "cancel-before.chatml" "Catalog.value" in
    let admission = Registry.reserve registry selected ~checkpoint |> ok_registry in
    let marker = Failure "cancel before initializer" in
    (match
       Eio.Cancel.sub (fun cancellation ->
         Eio.Cancel.cancel cancellation marker;
         Execution.run_scoped runner (fun () -> Registry.initialize registry admission))
     with
     | exception Eio.Cancel.Cancelled actual -> assert (phys_equal actual marker)
     | exception _ -> assert false
     | Ok _ | Error _ -> failwith "cancelled runner unexpectedly completed");
    assert (String.equal (label_status (Registry.status registry catalog_id)) "pending");
    finish admission Typing.Published.Constrained_before_runtime_failure;
    Registry.close registry |> ok_registry)
;;

let%test_unit "registry binds exact typing owner and refuses finalized admission effects" =
  Eio_main.run (fun env ->
    let contract = Contract.for_target One_off_v1 ~checkpoint in
    let typing = owner ~catalog_body:"let value = 1" ~contract () in
    let foreign = owner ~catalog_body:"let value = 1" ~contract () in
    let runner =
      Execution.create_runner ~env ~policy:(Bounded Execution.default_limits) ()
    in
    let registry =
      Registry.create
        ~typing
        ~surface:Surface.one_off_v1
        ~control:(Execution.runner_control runner)
        ~checkpoint
      |> ok_registry
    in
    let wrong = candidate foreign "foreign.chatml" "Catalog.value" in
    (match Registry.reserve registry wrong ~checkpoint with
     | Error (Registry.Error.Typing Typing.Error.Foreign_candidate) -> ()
     | Ok _ | Error _ -> failwith "foreign typing owner was accepted");
    let selected = candidate typing "proper.chatml" "Catalog.value" in
    let admission = Registry.reserve registry selected ~checkpoint |> ok_registry in
    (match Registry.close registry with
     | Error Registry.Error.Busy -> ()
     | Ok () | Error _ -> failwith "pending admission allowed close");
    finish admission Typing.Published.Constrained_before_runtime_failure;
    (match
       Execution.run_scoped runner (fun () -> Registry.initialize registry admission)
     with
     | Ok (Error Registry.Error.Finalized_admission) -> ()
     | Ok _ | Error _ -> failwith "finalized admission evaluated");
    assert (String.equal (label_status (Registry.status registry catalog_id)) "pending");
    Registry.close registry |> ok_registry)
;;

let%test_unit "interruption after native effect faults unit without replay" =
  List.iter [ `Raised; `Cancelled ] ~f:(fun mode ->
    Eio_main.run (fun env ->
      let log = ref [] in
      let marker = ref 1 in
      let reason = Interrupted marker in
      let interrupt = ref (fun () -> raise reason) in
      let surface =
        Chatml.Chatml_builtin_surface.merge
          { Chatml.Chatml_builtin_surface.empty with
            globals =
              [ { B.name = "mark"
                ; scheme = TFun ([ TString ], TUnit)
                ; impl =
                    (function
                      | [ Lang.VString name ] ->
                        log := name :: !log;
                        !interrupt ();
                        Lang.VUnit
                      | _ -> assert false)
                }
              ]
          }
          Chatml.Chatml_builtin_surface.core_surface
      in
      let typing =
        owner
          ~catalog_body:"let started = mark(\"catalog\") let value = 3"
          ~contract:(Support.native_contract ~surface ())
          ()
      in
      let runner =
        Execution.create_runner ~env ~policy:(Bounded Execution.default_limits) ()
      in
      let registry =
        Registry.create
          ~typing
          ~surface
          ~control:(Execution.runner_control runner)
          ~checkpoint
        |> ok_registry
      in
      let selected = candidate typing "cancel-after.chatml" "Catalog.value" in
      let admission = Registry.reserve registry selected ~checkpoint |> ok_registry in
      (match
         Eio.Cancel.sub (fun cancellation ->
           (match mode with
            | `Raised -> ()
            | `Cancelled ->
              interrupt
              := fun () ->
                   Eio.Cancel.cancel cancellation reason;
                   Eio.Fiber.yield ());
           Execution.run_scoped runner (fun () -> Registry.initialize registry admission))
       with
       | exception Interrupted actual ->
         (match mode with
          | `Raised -> assert (phys_equal actual marker)
          | `Cancelled -> assert false)
       | exception Eio.Cancel.Cancelled actual ->
         (match mode with
          | `Raised -> assert false
          | `Cancelled -> assert (phys_equal actual reason))
       | exception _ -> assert false
       | Ok _ | Error _ -> failwith "interrupted initializer unexpectedly completed");
      [%test_eq: string list] (List.rev !log) [ "catalog" ];
      if not (String.equal (label_status (Registry.status registry catalog_id)) "faulted")
      then
        raise_s
          [%sexp
            "after-effect interruption left unit unfaulted"
          , (mode : [ `Raised | `Cancelled ])
          , (Registry.status registry catalog_id : Registry.Status.t option)];
      finish admission Typing.Published.Constrained_before_runtime_failure;
      let repeat = candidate typing "cancel-after-repeat.chatml" "Catalog.value" in
      let repeat_admission =
        Registry.reserve registry repeat ~checkpoint |> ok_registry
      in
      (match
         Execution.run_scoped runner (fun () ->
           Registry.initialize registry repeat_admission)
       with
       | Ok (Error (Registry.Error.Faulted _)) -> ()
       | Ok _ | Error _ -> failwith "interrupted initializer replayed");
      [%test_eq: string list] (List.rev !log) [ "catalog" ];
      finish repeat_admission Typing.Published.Constrained_before_runtime_failure;
      Registry.close registry |> ok_registry))
;;
