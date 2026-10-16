open Core
module S = Chatml_source
module P = Chatml_prepared_program
module Lookup = Chatml_configured_lookup
module Session = Chatml_dependency_session
module Contract = Chatml.Chatml_compiler_contract
module Parse = Chatml.Chatml_parse
module Typing = Chatml_repl_typing
module Registry = Chatml_linked_runtime.Repl_modules
module Repl = Chatml_repl_execution
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

let repl_limits ?(max_preview_bytes = 128) () =
  Repl.Limits.create
    ~max_preview_bytes
    ~max_preview_nodes:64
    ~max_preview_depth:16
    ~max_diagnostic_bytes:128
    ~max_retained_tasks:1
  |> Or_error.ok_exn
;;

let ok_repl = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Repl.Error.t)]
;;

let receipt_label receipt =
  let status =
    match Repl.Receipt.outcome receipt with
    | Succeeded -> "succeeded"
    | Failed Runtime -> "runtime"
    | Failed Initializer -> "initializer"
    | Failed Budget -> "budget"
  in
  let task =
    match Repl.Receipt.task_retention receipt with
    | Not_a_task -> "none"
    | Retained _ -> "retained"
    | Limit_reached -> "limit"
  in
  status, Repl.Receipt.value_preview receipt, task
;;

let%expect_test "portable REPL stages names, preserves old cells and keeps Tasks inert" =
  Eio_main.run (fun env ->
    let typing =
      owner
        ~catalog_body:"let value = ref(0)"
        ~contract:(Contract.for_target One_off_v1 ~checkpoint)
        ()
    in
    let repl =
      Repl.create
        ~typing
        ~surface:Surface.one_off_v1
        ~env
        ~policy:(Bounded Execution.default_limits)
        ~limits:(repl_limits ())
        ~checkpoint
      |> ok_repl
    in
    let execute_candidate candidate =
      Repl.execute_next repl candidate ~checkpoint |> ok_repl
    in
    let execute path body = execute_candidate (candidate typing path body) in
    let setup_source = capture "setup.chatml" "let cell = ref(0)\nlet old () = !cell" in
    let setup_ready =
      match Typing.admit typing setup_source ~checkpoint |> ok_typing with
      | Ready ready -> ready
      | Need_more _ | Rejected _ -> failwith "setup was not a complete submission"
    in
    let first =
      execute_candidate (Typing.prepare_next typing setup_ready ~checkpoint |> ok_typing)
    in
    let closure =
      execute_candidate (Typing.prepare_next typing setup_ready ~checkpoint |> ok_typing)
    in
    let before = execute "before.chatml" "old()" in
    let failed = execute "failed.chatml" "let doomed = (cell := 9; 1 / 0)" in
    let failed_binding_absent =
      let source = capture "missing.chatml" "doomed" in
      let ready =
        match Typing.admit typing source ~checkpoint |> ok_typing with
        | Ready ready -> ready
        | Need_more _ | Rejected _ -> failwith "missing-name probe was incomplete"
      in
      match Typing.prepare_next typing ready ~checkpoint with
      | Error (Typing.Error.Type_diagnostic _) -> true
      | Ok _ | Error _ -> false
    in
    let after = execute "after.chatml" "old()" in
    let shadow = execute "shadow.chatml" "let cell = ref(42)" in
    let old = execute "old.chatml" "old()" in
    let current = execute "current.chatml" "!cell" in
    let opened = execute "inert-open.chatml" "open Catalog" in
    let catalog_before = execute "catalog-before.chatml" "Catalog.value" in
    let catalog_write = execute "catalog-write.chatml" "Catalog.value := 5" in
    let catalog_after = execute "catalog-after.chatml" "Catalog.value" in
    let broken_candidate =
      candidate typing "broken-origin.chatml" "let broken () = 1 / 0"
    in
    let broken_source = Typing.Candidate.source broken_candidate in
    let broken_binding = execute_candidate broken_candidate in
    let broken_call = execute "broken-call.chatml" "broken()" in
    let original_failure_site =
      match Repl.Receipt.outcome broken_call, Repl.Receipt.diagnostic broken_call with
      | Failed Runtime, Some diagnostic ->
        (match Repl.Diagnostic.location diagnostic with
         | Some location ->
           String.equal (Repl.Diagnostic.message diagnostic) "Division by zero"
           && S.Id.equal (S.Location.source location) (S.id broken_source)
           && S.Digest.equal (S.Location.source_digest location) (S.digest broken_source)
           && String.equal (S.read broken_source location |> Or_error.ok_exn) "1 / 0"
         | None -> false)
      | Succeeded, _ | Failed (Initializer | Budget), _ | Failed Runtime, None -> false
    in
    let task = execute "task.chatml" "Task.fail(\"deferred\")" in
    let handle =
      match Repl.Receipt.task_retention task with
      | Retained handle -> handle
      | Not_a_task | Limit_reached -> failwith "first task was not retained"
    in
    let task_full = execute "task-full.chatml" "Task.pure(8)" in
    let task_current = Repl.current_task_handle repl handle in
    let foreign_typing =
      owner
        ~catalog_body:"let value = ref(0)"
        ~contract:(Contract.for_target One_off_v1 ~checkpoint)
        ()
    in
    let foreign_repl =
      Repl.create
        ~typing:foreign_typing
        ~surface:Surface.one_off_v1
        ~env
        ~policy:(Bounded Execution.default_limits)
        ~limits:(repl_limits ())
        ~checkpoint
      |> ok_repl
    in
    let foreign_handle = Repl.current_task_handle foreign_repl handle in
    let wrong = candidate foreign_typing "foreign-repl.chatml" "1" in
    let foreign =
      match Repl.execute_next repl wrong ~checkpoint with
      | Error (Repl.Error.Typing Typing.Error.Foreign_candidate) -> true
      | Ok _ | Error _ -> false
    in
    let anonymous_int =
      let frozen = Repl.Receipt.frozen_types current in
      let snapshot = Typing.Frozen_types.snapshot frozen in
      let rec reaches_int seen root =
        let index = Chatml_typechecker.Type_snapshot.Id.index root in
        if Set.mem seen index
        then false
        else (
          match Chatml_typechecker.Type_snapshot.find snapshot root with
          | Some Chatml_typechecker.Type_snapshot.Node.Int -> true
          | Some (Chatml_typechecker.Type_snapshot.Node.Bound_link next) ->
            reaches_int (Set.add seen index) next
          | Some _ | None -> false)
      in
      Option.value_map
        (Repl.Receipt.expression_type current)
        ~default:false
        ~f:(reaches_int Int.Set.empty)
    in
    print_s
      [%sexp
        (receipt_label first : string * string option * string)
      , (receipt_label closure : string * string option * string)
      , (receipt_label before : string * string option * string)
      , (receipt_label failed : string * string option * string)
      , (receipt_label after : string * string option * string)
      , (receipt_label shadow : string * string option * string)
      , (receipt_label old : string * string option * string)
      , (receipt_label current : string * string option * string)
      , (receipt_label opened : string * string option * string)
      , (receipt_label catalog_before : string * string option * string)
      , (receipt_label catalog_write : string * string option * string)
      , (receipt_label catalog_after : string * string option * string)
      , (receipt_label broken_binding : string * string option * string)
      , (receipt_label broken_call : string * string option * string)
      , (receipt_label task : string * string option * string)
      , (receipt_label task_full : string * string option * string)
      , (task_current : bool)
      , (foreign_handle : bool)
      , (failed_binding_absent : bool)
      , (foreign : bool)
      , (anonymous_int : bool)];
    assert original_failure_site;
    Repl.close foreign_repl |> ok_repl;
    Repl.close repl |> ok_repl;
    print_s [%sexp (Repl.current_task_handle repl handle : bool)]);
  [%expect
    {|
    ((succeeded () none) (succeeded () none) (succeeded (0) none)
     (runtime () none) (succeeded (9) none) (succeeded () none)
     (succeeded (9) none) (succeeded (42) none) (succeeded () none)
     (succeeded ("ref(0)") none) (succeeded ("()") none)
     (succeeded ("ref(5)") none) (succeeded () none) (runtime () none)
     (succeeded ("fail(deferred)") retained) (succeeded ("pure(8)") limit) true
     false true true true)
    false
    |}]
;;

let%expect_test "retained Task installer rejects elapsed action and ancestor deadlines" =
  Eio_main.run (fun _ ->
    let run ~inherited =
      let clock = Eio_mock.Clock.Mono.make () in
      let timed_env =
        object
          method mono_clock = clock
        end
      in
      let typing =
        owner
          ~catalog_body:"let value = 1"
          ~contract:(Contract.for_target One_off_v1 ~checkpoint)
          ()
      in
      let repl =
        Repl.create
          ~typing
          ~surface:Surface.one_off_v1
          ~env:timed_env
          ~policy:(Bounded Execution.default_limits)
          ~limits:(repl_limits ())
          ~checkpoint
        |> ok_repl
      in
      let receipt =
        Repl.execute_next
          repl
          (candidate typing "deadline-task.chatml" "Task.pure(7)")
          ~checkpoint
        |> ok_repl
      in
      let handle =
        match Repl.Receipt.task_retention receipt with
        | Retained handle -> handle
        | Not_a_task | Limit_reached -> failwith "Task was not retained"
      in
      let prepared = ref false in
      let installed = ref false in
      let prepare_result ~value:_ ~local_effects:_ =
        prepared := true;
        Eio_mock.Clock.Mono.set_time clock (Mtime.of_uint64_ns 11_000_000L);
        Ok (fun () -> installed := true)
      in
      let deadline =
        Execution.Bounded { Execution.default_limits with wall_seconds = 0.01 }
      in
      let drive () =
        Repl.drive_retained
          repl
          handle
          ~operations:[]
          ~prepare_result
          ~action_policy:(if inherited then Execution.Unrestricted else deadline)
          ()
        |> ok_repl
      in
      let result =
        if inherited
        then (
          match
            Execution.with_host_budget ~policy:deadline ~env:timed_env (fun _ -> drive ())
          with
          | Ok result -> Repl.Task_result.outcome result
          | Error error ->
            [%test_eq: string] error.code "chatml.execution_timeout";
            Repl.Task_result.Failed Budget)
        else Repl.Task_result.outcome (drive ())
      in
      [%test_eq: bool] !prepared true;
      [%test_eq: bool] !installed false;
      (match result with
       | Failed Budget -> ()
       | Succeeded | Failed Runtime -> failwith "elapsed deadline was not enforced");
      Repl.close repl |> ok_repl
    in
    run ~inherited:false;
    run ~inherited:true);
  [%expect
    {|
    +mock time is now 0.011
    +mock time is now 0.011
    |}]
;;

let%test_unit
    "retained Tasks use the same live environment and preserve catch and failure"
  =
  Eio_main.run (fun env ->
    let make () =
      let typing =
        owner
          ~catalog_body:"let value = 1"
          ~contract:(Contract.for_target One_off_v1 ~checkpoint)
          ()
      in
      let limits =
        Repl.Limits.create
          ~max_preview_bytes:128
          ~max_preview_nodes:64
          ~max_preview_depth:16
          ~max_diagnostic_bytes:128
          ~max_retained_tasks:3
        |> Or_error.ok_exn
      in
      let repl =
        Repl.create
          ~typing
          ~surface:Surface.one_off_v1
          ~env
          ~policy:(Bounded Execution.default_limits)
          ~limits
          ~checkpoint
        |> ok_repl
      in
      typing, repl
    in
    let typing, repl = make () in
    let execute path body =
      Repl.execute_next repl (candidate typing path body) ~checkpoint |> ok_repl
    in
    let retained path body =
      match Repl.Receipt.task_retention (execute path body) with
      | Retained handle -> handle
      | Not_a_task | Limit_reached -> failwith "Task was not retained"
    in
    ignore (execute "cell.chatml" "let cell = ref(1)" : Repl.Receipt.t);
    let live =
      retained "live.chatml" "Task.bind(Task.pure(()), fun ignored -> Task.pure(!cell))"
    in
    let caught =
      retained
        "caught.chatml"
        "Task.catch(Task.fail(\"deferred\"), fun ignored -> Task.pure(9))"
    in
    let failed = retained "failed-task.chatml" "Task.fail(\"deferred\")" in
    ignore (execute "update.chatml" "cell := 2" : Repl.Receipt.t);
    let drive owner handle =
      Repl.drive_retained
        owner
        handle
        ~operations:[]
        ~action_policy:Execution.Unrestricted
        ()
      |> ok_repl
    in
    let first = drive repl live in
    let second = drive repl live in
    let recovered = drive repl caught in
    let failure = drive repl failed in
    let succeeded preview result =
      match Repl.Task_result.outcome result, Repl.Task_result.value_preview result with
      | Succeeded, Some actual -> [%test_eq: string] actual preview
      | _ -> failwith "Task did not return the expected value"
    in
    succeeded "2" first;
    succeeded "2" second;
    succeeded "9" recovered;
    (match Repl.Task_result.outcome failure with
     | Failed Runtime -> ()
     | Succeeded | Failed Budget -> failwith "Task.fail did not report runtime failure");
    let diagnostic = Option.value_exn (Repl.Task_result.diagnostic failure) in
    [%test_eq: string] (Repl.Diagnostic.message diagnostic) "deferred";
    let marker = Failure "retained callback raised" in
    (match
       Repl.drive_retained
         repl
         live
         ~operations:[]
         ~prepare_result:(fun ~value:_ ~local_effects:_ ->
           (match
              Repl.drive_retained
                repl
                live
                ~operations:[]
                ~action_policy:Execution.Unrestricted
                ()
            with
            | Error Busy -> ()
            | Ok _ | Error _ -> failwith "reentrant Task drive was accepted");
           raise marker)
         ~action_policy:Execution.Unrestricted
         ()
     with
     | exception actual when phys_equal actual marker -> ()
     | Ok _ | Error _ | (exception _) ->
       failwith "retained callback exception identity was lost");
    succeeded "2" (drive repl live);
    let cancellation_marker = Failure "retained callback cancelled" in
    (match
       Eio.Cancel.sub (fun cancellation ->
         Repl.drive_retained
           repl
           live
           ~operations:[]
           ~prepare_result:(fun ~value:_ ~local_effects:_ ->
             Eio.Cancel.cancel cancellation cancellation_marker;
             Eio.Fiber.yield ();
             Ok ignore)
           ~action_policy:Execution.Unrestricted
           ())
     with
     | exception Eio.Cancel.Cancelled actual ->
       assert (phys_equal actual cancellation_marker)
     | Ok _ | Error _ | (exception _) ->
       failwith "retained callback cancellation identity was lost");
    succeeded "2" (drive repl live);
    let foreign_typing, foreign_repl = make () in
    ignore (foreign_typing : Typing.t);
    (match
       Repl.drive_retained
         foreign_repl
         live
         ~operations:[]
         ~action_policy:Execution.Unrestricted
         ()
     with
     | Error Foreign_task_handle -> ()
     | Ok _ | Error _ -> failwith "foreign Task handle was accepted");
    Repl.close foreign_repl |> ok_repl;
    Repl.close repl |> ok_repl;
    match
      Repl.drive_retained
        repl
        live
        ~operations:[]
        ~action_policy:Execution.Unrestricted
        ()
    with
    | Error Closed -> ()
    | Ok _ | Error _ -> failwith "closed Task owner was accepted")
;;

let%test_unit "retained Task keeps catch rollback and prepares one surviving result" =
  Eio_main.run (fun env ->
    let reserve_builtin : B.builtin =
      { name = "reserve"
      ; scheme = B.TFun ([ B.TString ], B.task_ty B.TString)
      ; impl =
          (fun args ->
            Lang.VTask (Lang.Task.create (TPerform { op = "Reservation.reserve"; args })))
      }
    in
    let surface =
      Chatml.Chatml_builtin_surface.merge
        { Chatml.Chatml_builtin_surface.empty with
          modules = [ { name = "Reservation"; exports = [ reserve_builtin ] } ]
        }
        Surface.one_off_v1
    in
    let typing =
      owner
        ~catalog_body:"let value = 1"
        ~contract:(Support.native_contract ~surface ())
        ()
    in
    let repl =
      Repl.create
        ~typing
        ~surface
        ~env
        ~policy:(Bounded Execution.default_limits)
        ~limits:(repl_limits ())
        ~checkpoint
      |> ok_repl
    in
    let receipt =
      Repl.execute_next
        repl
        (candidate
           typing
           "reservation.chatml"
           "Task.bind(Task.catch(Task.bind(Reservation.reserve(\"same\"), fun discarded \
            -> Task.fail(\"undo\")), fun failure -> Task.pure(\"caught\")), fun ignored \
            -> Reservation.reserve(\"same\"))")
        ~checkpoint
      |> ok_repl
    in
    let handle =
      match Repl.Receipt.task_retention receipt with
      | Retained handle -> handle
      | Not_a_task | Limit_reached -> failwith "reservation Task was not retained"
    in
    let issued = ref 0 in
    let rolled_back = ref [] in
    let operation : Chatml_host_runtime.op_def =
      { name = "Reservation.reserve"
      ; kind =
          Local_transactional_with_result
            { rollback =
                (function
                  | Lang.VString id :: _ -> rolled_back := id :: !rolled_back
                  | _ -> failwith "rollback lost reservation identity")
            }
      ; phase_check = (fun _ -> Ok ())
      ; perform =
          (fun _ args ->
            match args with
            | [ Lang.VString "same" ] ->
              incr issued;
              Ok (Lang.VString ("ticket-" ^ Int.to_string !issued))
            | _ -> Error "wrong reservation request")
      }
    in
    let drive ?prepare_result () =
      Repl.drive_retained
        repl
        handle
        ~operations:[ operation ]
        ?prepare_result
        ~action_policy:Execution.Unrestricted
        ()
      |> ok_repl
    in
    let disabled = drive () in
    (match Repl.Task_result.outcome disabled with
     | Failed Runtime -> ()
     | Succeeded | Failed Budget -> failwith "local operation lacked result admission");
    [%test_eq: int] !issued 0;
    let installed = ref 0 in
    let prepare_result ~value ~local_effects =
      (match value, local_effects with
       | ( Lang.VString "ticket-2"
         , [ { Lang.op = "Reservation.reserve"
             ; args = [ Lang.VString "ticket-2"; Lang.VString "same" ]
             }
           ] ) -> ()
       | _ -> failwith "surviving result did not match its reservation");
      Ok (fun () -> incr installed)
    in
    let admitted = drive ~prepare_result () in
    (match Repl.Task_result.outcome admitted, Repl.Task_result.value_preview admitted with
     | Succeeded, Some "ticket-2" -> ()
     | _ -> failwith "retained result transaction was not installed");
    [%test_eq: int] !issued 2;
    [%test_eq: string list] !rolled_back [ "ticket-1" ];
    [%test_eq: int] !installed 1;
    Repl.close repl |> ok_repl)
;;

let%test_unit "portable REPL finalizes cancellation and budget without replay" =
  Eio_main.run (fun env ->
    let calls = ref 0 in
    let interrupt = ref (fun () -> ()) in
    let surface =
      Chatml.Chatml_builtin_surface.merge
        { Chatml.Chatml_builtin_surface.empty with
          globals =
            [ { B.name = "mark"
              ; scheme = TFun ([], TUnit)
              ; impl =
                  (function
                    | [] ->
                      incr calls;
                      !interrupt ();
                      Lang.VUnit
                    | _ -> assert false)
              }
            ]
        }
        Chatml.Chatml_builtin_surface.core_surface
    in
    let contract = Support.native_contract ~surface () in
    let typing = owner ~catalog_body:"let value = 1" ~contract () in
    let repl =
      Repl.create
        ~typing
        ~surface
        ~env
        ~policy:(Bounded Execution.default_limits)
        ~limits:(repl_limits ())
        ~checkpoint
      |> ok_repl
    in
    let run path body =
      Repl.execute_next repl (candidate typing path body) ~checkpoint |> ok_repl
    in
    ignore (run "state.chatml" "let state = ref(0)" : Repl.Receipt.t);
    let marker = Failure "cancel after mark" in
    (match
       Eio.Cancel.sub (fun cancellation ->
         (interrupt
          := fun () ->
               Eio.Cancel.cancel cancellation marker;
               Eio.Fiber.yield ());
         run "cancelled.chatml" "let missed = (state := 1; mark())")
     with
     | exception Eio.Cancel.Cancelled actual -> assert (phys_equal actual marker)
     | (exception _) | _ -> failwith "cancellation was not preserved");
    [%test_eq: int] !calls 1;
    let retained = run "retained.chatml" "!state" in
    [%test_eq: string option] (Repl.Receipt.value_preview retained) (Some "1");
    Repl.close repl |> ok_repl;
    let fuel_limits = { Execution.default_limits with fuel = 1 } in
    let budget_typing = owner ~catalog_body:"let value = 1" ~contract () in
    let budget_repl =
      Repl.create
        ~typing:budget_typing
        ~surface
        ~env
        ~policy:(Bounded fuel_limits)
        ~limits:(repl_limits ())
        ~checkpoint
      |> ok_repl
    in
    let budget =
      Repl.execute_next
        budget_repl
        (candidate budget_typing "budget.chatml" "1 + 2")
        ~checkpoint
      |> ok_repl
    in
    (match Repl.Receipt.outcome budget with
     | Failed Budget -> ()
     | Succeeded | Failed (Runtime | Initializer) ->
       failwith "bounded execution did not report budget failure");
    Repl.close budget_repl |> ok_repl;
    (interrupt := fun () -> ());
    let fault_typing =
      owner ~catalog_body:"let started = mark() let value = 1 / 0" ~contract ()
    in
    let fault_repl =
      Repl.create
        ~typing:fault_typing
        ~surface
        ~env
        ~policy:(Bounded Execution.default_limits)
        ~limits:(repl_limits ())
        ~checkpoint
      |> ok_repl
    in
    let attempt path =
      Repl.execute_next
        fault_repl
        (candidate fault_typing path "Catalog.value")
        ~checkpoint
      |> ok_repl
    in
    let first = attempt "fault-repl-first.chatml" in
    let second = attempt "fault-repl-second.chatml" in
    (match Repl.Receipt.outcome first, Repl.Receipt.outcome second with
     | Failed Initializer, Failed Initializer -> ()
     | _ -> failwith "faulted initializer was not retained");
    [%test_eq: int] !calls 2;
    Repl.close fault_repl |> ok_repl;
    let utf8_typing =
      owner
        ~catalog_body:"let value = 1"
        ~contract:(Contract.for_target One_off_v1 ~checkpoint)
        ()
    in
    let utf8_repl =
      Repl.create
        ~typing:utf8_typing
        ~surface:Surface.one_off_v1
        ~env
        ~policy:(Bounded Execution.default_limits)
        ~limits:(repl_limits ~max_preview_bytes:3 ())
        ~checkpoint
      |> ok_repl
    in
    let utf8 =
      Repl.execute_next
        utf8_repl
        (candidate utf8_typing "utf8.chatml" "\"éé\"")
        ~checkpoint
      |> ok_repl
    in
    (match Repl.Receipt.outcome utf8 with
     | Succeeded -> ()
     | Failed _ -> failwith "bounded preview changed successful execution");
    let preview = Repl.Receipt.value_preview utf8 |> Option.value_exn in
    assert (Stdlib.String.is_valid_utf_8 preview);
    assert (String.length preview <= 3);
    Repl.close utf8_repl |> ok_repl)
;;
