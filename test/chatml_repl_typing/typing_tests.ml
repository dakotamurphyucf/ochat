open Core
module S = Chatml_source
module P = Chatml_prepared_program
module Lookup = Chatml_configured_lookup
module Session = Chatml_dependency_session
module Contract = Chatml.Chatml_compiler_contract
module Parse = Chatml.Chatml_parse
module Typing = Chatml_repl_typing
module Snapshot = Chatml_typechecker.Type_snapshot
module Copy = Chatml_typechecker.Repl_type_roots

let checkpoint () = ()

let ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Typing.Error.t)]
;;

let id path =
  S.Id.create ~package_namespace:"repl-typing-test" ~relative_path:path |> Or_error.ok_exn
;;

let capture path contents =
  S.create (id path) ~contents ~expected_digest:None ~max_bytes:None ~checkpoint
  |> Or_error.ok_exn
;;

let input () =
  let contract = Contract.for_target One_off_v1 ~checkpoint in
  let digest = Contract.fingerprint contract ~checkpoint in
  let configuration = capture "project.json" "selected scope" in
  let declared_at =
    S.locate configuration ~start_byte:0 ~end_byte:1 ~checkpoint |> Or_error.ok_exn
  in
  let entry = capture "entry.chatml" "let host = 1" in
  let catalog = capture "catalog.chatml" "let cells = []" in
  let definitions =
    List.map [ entry; catalog ] ~f:(fun source ->
      P.Definition.create ~implementation:(S.id source) ~interface:None ~declared_at
      |> Or_error.ok_exn)
  in
  let scopes =
    [ P.Scope.create
        (S.id entry)
        ~bindings:
          [ { P.Scope.name = "Catalog"
            ; module_id = P.Module_id.of_implementation (S.id catalog)
            ; declared_at
            }
          ]
        ~selected_at:(Some declared_at)
        ~checkpoint
      |> Or_error.ok_exn
    ; P.Scope.create
        (S.id catalog)
        ~bindings:[]
        ~selected_at:(Some declared_at)
        ~checkpoint
      |> Or_error.ok_exn
    ]
  in
  let prepared =
    P.create
      ~max_input_bytes:None
      ~compiler_contract:digest
      ~environment:(P.Environment.implicit ~required_contract:digest)
      ~provenance:(Project { declaration = S.id configuration; lock = None })
      ~entry:(S.id entry)
      ~sources:[ configuration; entry; catalog ]
      ~definitions
      ~scopes
      ~checkpoint
      ()
    |> function
    | Ok input -> input
    | Error error -> failwith error.P.Error.message
  in
  prepared, contract
;;

let owner () =
  let prepared, contract = input () in
  let scope =
    Lookup.Repl_scope.create
      prepared
      ~anchor_owner:(P.Module_id.of_implementation (id "entry.chatml"))
      ~anchor_implementation:(id "entry.chatml")
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
  let display =
    Snapshot.Limits.create ~max_nodes:100000 ~max_edges:100000 ~max_label_bytes:100000
    |> Or_error.ok_exn
  in
  let root_copy =
    Copy.Limits.create
      ~max_bindings:1000
      ~max_type_roots:2000
      ~max_type_nodes:100000
      ~max_variable_cells:10000
      ~max_type_edges:100000
      ~max_copy_work:300000
      ~max_copy_depth:256
      ~max_name_work:300000
      ~max_name_bytes:100000
    |> Or_error.ok_exn
  in
  let limits =
    Typing.Limits.create
      ~parse
      ~display
      ~max_display_name_bytes:100000
      ~max_statements:30
      ~root_copy
    |> Or_error.ok_exn
  in
  Typing.create
    ~prepared
    ~scope
    ~contract
    ~root_limits:
      (Session.Roots.Limits.create
         ~max_checked_modules:20
         ~max_edges:50
         ~max_dependency_depth:20
       |> Or_error.ok_exn)
    ~limits
    ~checkpoint
  |> ok
;;

let ready owner path body =
  match Typing.admit owner (capture path body) ~checkpoint |> ok with
  | Ready ready -> ready
  | Need_more _ | Rejected _ -> failwith "fixture was not a complete submission"
;;

let prepare owner ready = Typing.prepare_next owner ready ~checkpoint |> ok

let finish owner candidate outcome =
  let reserved = Typing.reserve_execution owner candidate ~checkpoint |> ok in
  Typing.finalize_execution reserved ~outcome |> ok
;;

let print_binding receipt name =
  let frozen = Typing.Published.frozen_types receipt in
  let id =
    List.Assoc.find (Typing.Frozen_types.bindings frozen) name ~equal:String.equal
    |> Option.value_exn
  in
  let node = Snapshot.find (Typing.Frozen_types.snapshot frozen) id |> Option.value_exn in
  print_s [%sexp (name : string), (node : Snapshot.Node.t)]
;;

let%expect_test "one actor commits actual checked statements in sequence" =
  let owner = owner () in
  let first = ready owner "first.chatml" "let x = 1 let y = x + 1" in
  let one = prepare owner first in
  print_binding (finish owner one Typing.Published.After_success) "x";
  let two = prepare owner first in
  print_binding (finish owner two Typing.Published.After_success) "y";
  let third = ready owner "third.chatml" "let z = y + 1" in
  print_binding (finish owner (prepare owner third) Typing.Published.After_success) "z";
  [%expect
    {|
    (x Int)
    (y Int)
    (z Int)
    |}]
;;

let%test_unit "failed typing discards candidate weak constraints and lexical names" =
  let owner = owner () in
  let initial = ready owner "initial.chatml" "open Catalog" in
  let opened = prepare owner initial in
  [%test_eq: int] (List.length (Typing.Candidate.new_checked_dependencies opened)) 1;
  assert (
    List.Assoc.mem
      (Typing.Candidate.frozen_types opened |> Typing.Frozen_types.bindings)
      "cells"
      ~equal:String.equal);
  ignore (finish owner opened Typing.Published.After_success);
  let invalid =
    ready owner "invalid.chatml" "let doomed = Array.set(cells, 0, true) + 1"
  in
  (match Typing.prepare_next owner invalid ~checkpoint with
   | Error (Typing.Error.Type_diagnostic _) -> ()
   | _ -> failwith "expected a checker rejection");
  let valid = ready owner "valid.chatml" "let good = Array.set(cells, 0, 1)" in
  let good = prepare owner valid in
  [%test_eq: int] (List.length (Typing.Candidate.new_checked_dependencies good)) 0;
  ignore (finish owner good Typing.Published.After_success)
;;

let%test_unit
    "runtime failure keeps shared public constraints but excludes failed new name"
  =
  let owner = owner () in
  let first = ready owner "open.chatml" "open Catalog" in
  ignore (finish owner (prepare owner first) Typing.Published.After_success);
  let failed = ready owner "failure.chatml" "let failed = Array.set(cells, 0, 1)" in
  let candidate = prepare owner failed in
  let preview_roots =
    Typing.Candidate.frozen_types candidate
    |> Typing.Frozen_types.snapshot
    |> Snapshot.roots
    |> List.length
  in
  let receipt =
    finish owner candidate Typing.Published.Constrained_before_runtime_failure
  in
  let published_roots =
    Typing.Published.frozen_types receipt
    |> Typing.Frozen_types.snapshot
    |> Snapshot.roots
    |> List.length
  in
  [%test_eq: int] published_roots (preview_roots - 1);
  let wrong =
    ready owner "wrong.chatml" "let wrong = Array.set(Catalog.cells, 0, true)"
  in
  (match Typing.prepare_next owner wrong ~checkpoint with
   | Error (Typing.Error.Type_diagnostic _) -> ()
   | _ -> failwith "runtime-failure type constraint was lost");
  let missing = ready owner "missing.chatml" "let use = failed" in
  match Typing.prepare_next owner missing ~checkpoint with
  | Error (Typing.Error.Type_diagnostic _) -> ()
  | _ -> failwith "failed lexical name was published"
;;

let%test_unit "a new complete source supersedes an unreserved typed candidate" =
  let owner = owner () in
  let earlier = ready owner "earlier.chatml" "let old = 1" in
  let stale = prepare owner earlier in
  (match Typing.admit owner (capture "incomplete.chatml" "let x =") ~checkpoint |> ok with
   | Need_more _ -> ()
   | _ -> failwith "unfinished input did not request continuation");
  (match Typing.admit owner (capture "invalid.chatml" "let =") ~checkpoint |> ok with
   | Rejected _ -> ()
   | _ -> failwith "invalid input was not rejected");
  (match Typing.prepare_next owner earlier ~checkpoint with
   | Error Typing.Error.Busy_executing -> ()
   | _ -> failwith "incomplete/rejected input displaced the candidate");
  let latest = ready owner "latest.chatml" "let current = 2" in
  (match Typing.reserve_execution owner stale ~checkpoint with
   | Error Typing.Error.Stale_candidate -> ()
   | _ -> failwith "superseded candidate was executable");
  ignore (finish owner (prepare owner latest) Typing.Published.After_success);
  let initial = ready owner "shadow-int.chatml" "let value = 1" in
  ignore (finish owner (prepare owner initial) Typing.Published.After_success);
  let shadow = ready owner "shadow-bool.chatml" "let value = true" in
  ignore (finish owner (prepare owner shadow) Typing.Published.After_success);
  let use = ready owner "shadow-use.chatml" "let chosen = if value then 1 else 2" in
  ignore (finish owner (prepare owner use) Typing.Published.After_success)
;;

let%test_unit "bounded outcome sequences match lexical publication model" =
  for outcomes = 0 to 7 do
    let owner = owner () in
    for step = 0 to 2 do
      let name = "value" ^ Int.to_string step in
      let definition =
        ready
          owner
          ("definition" ^ Int.to_string step ^ ".chatml")
          ("let " ^ name ^ " = " ^ Int.to_string step)
      in
      let candidate = prepare owner definition in
      let reservation = Typing.reserve_execution owner candidate ~checkpoint |> ok in
      (match Typing.close owner with
       | Error Typing.Error.Busy_executing -> ()
       | _ -> failwith "reservation permitted close");
      let succeeds = outcomes land (1 lsl step) <> 0 in
      let outcome =
        if succeeds
        then Typing.Published.After_success
        else Constrained_before_runtime_failure
      in
      let receipt = Typing.finalize_execution reservation ~outcome |> ok in
      let retry = Typing.finalize_execution reservation ~outcome |> ok in
      assert (phys_equal receipt retry);
      let other =
        if succeeds
        then Typing.Published.Constrained_before_runtime_failure
        else After_success
      in
      (match Typing.finalize_execution reservation ~outcome:other with
       | Error Typing.Error.Conflicting_finalization -> ()
       | _ -> failwith "conflicting finalization changed state");
      let probe =
        ready
          owner
          ("probe" ^ Int.to_string step ^ ".chatml")
          ("let probe" ^ Int.to_string step ^ " = " ^ name)
      in
      match Typing.prepare_next owner probe ~checkpoint with
      | Ok candidate when succeeds ->
        ignore (finish owner candidate Typing.Published.After_success)
      | Error (Typing.Error.Type_diagnostic _) when not succeeds -> ()
      | _ -> failwith "lexical publication diverged from reference model"
    done
  done
;;

exception Cancelled of int ref

let%test_unit "cancelled candidate preserves committed graph and aborts its ready source" =
  let owner = owner () in
  let first = ready owner "keep.chatml" "let keep = 1" in
  ignore (finish owner (prepare owner first) Typing.Published.After_success);
  let interrupted = ready owner "interrupted.chatml" "let poisoned = keep + 2" in
  let token = ref 0 in
  let polls = ref 0 in
  (try
     ignore
       (Typing.prepare_next owner interrupted ~checkpoint:(fun () ->
          Int.incr polls;
          if !polls = 4 then raise (Cancelled token)));
     failwith "checkpoint cancellation was swallowed"
   with
   | Cancelled observed when phys_equal observed token -> ());
  (match Typing.prepare_next owner interrupted ~checkpoint with
   | Error Typing.Error.Aborted_submission -> ()
   | _ -> failwith "cancelled ready source remained executable");
  let next = ready owner "next.chatml" "let next = keep + 1" in
  ignore (finish owner (prepare owner next) Typing.Published.After_success)
;;

let copy_limits ?(max_bindings = 8) ?(max_name_work = 1000) ?(max_name_bytes = 1000) () =
  Copy.Limits.create
    ~max_bindings
    ~max_type_roots:16
    ~max_type_nodes:128
    ~max_variable_cells:32
    ~max_type_edges:128
    ~max_copy_work:1000
    ~max_copy_depth:32
    ~max_name_work
    ~max_name_bytes
  |> Or_error.ok_exn
;;

let copied lexical ~limits =
  let prepared, _ = input () in
  Copy.copy_detached
    ~lexical
    ~modules:
      (Session.Roots.empty
         prepared
         ~limits:
           (Session.Roots.Limits.create
              ~max_checked_modules:3
              ~max_edges:3
              ~max_dependency_depth:3
            |> Or_error.ok_exn))
    ~limits
    ~checkpoint
;;

let copy_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Copy.Error.t)]
;;

let%test_unit "one detached memo shares candidate cells and leaves originals untouched" =
  let original =
    ref (Chatml_typechecker.Free { name = "x"; level = 0; equality = None })
  in
  let lexical =
    Chatml_typechecker.Value_scope.empty
    |> fun scope ->
    Chatml_typechecker.Value_scope.add_local
      scope
      ~name:"left"
      ~scheme:(Chatml_typechecker.Var original)
    |> fun scope ->
    Chatml_typechecker.Value_scope.add_local
      scope
      ~name:"right"
      ~scheme:(Chatml_typechecker.Var original)
  in
  let copy = copied lexical ~limits:(copy_limits ()) |> copy_ok in
  let cell name =
    match
      Chatml_typechecker.Value_scope.find (Copy.Copy.lexical copy) ~name
      |> Option.value_exn
      |> Chatml_typechecker.Value_scope.Binding.scheme
    with
    | Chatml_typechecker.Var cell -> cell
    | _ -> failwith "copied root lost its cell"
  in
  let left = cell "left" in
  let right = cell "right" in
  assert (phys_equal left right);
  assert (not (phys_equal left original));
  left := Chatml_typechecker.Bound Chatml_typechecker.TInt;
  match !original with
  | Chatml_typechecker.Free _ -> ()
  | _ -> failwith "copy mutated committed cell"
;;

let%test_unit "name, binding and fresh-counter boundaries fail closed" =
  let lexical =
    Chatml_typechecker.Value_scope.add_local
      Chatml_typechecker.Value_scope.empty
      ~name:"x"
      ~scheme:(Chatml_typechecker.Generic { name = "a"; equality = None })
  in
  (match copied lexical ~limits:(copy_limits ~max_bindings:0 ()) with
   | Error Copy.Error.Binding_limit -> ()
   | _ -> failwith "binding limit was bypassed");
  (match copied lexical ~limits:(copy_limits ~max_name_work:0 ()) with
   | Error Copy.Error.Name_limit -> ()
   | _ -> failwith "name work limit was bypassed");
  (match copied lexical ~limits:(copy_limits ~max_name_bytes:0 ()) with
   | Error Copy.Error.Name_limit -> ()
   | _ -> failwith "name byte limit was bypassed");
  let generated_name id =
    let letter = Char.of_int_exn ((id mod 26) + Char.to_int 'a') |> Char.to_string in
    letter ^ Int.to_string (id / 26)
  in
  let near_max =
    Chatml_typechecker.Value_scope.add_local
      Chatml_typechecker.Value_scope.empty
      ~name:"x"
      ~scheme:
        (Chatml_typechecker.Generic
           { name = generated_name (Int.max_value - 1); equality = None })
  in
  let seed = copied near_max ~limits:(copy_limits ()) |> copy_ok |> Copy.Copy.name_seed in
  [%test_eq: int] (Copy.Name_seed.next_current_id seed) Int.max_value;
  let state = Chatml_typechecker.create_state ~checkpoint () in
  state.shared.current_id <- Copy.Name_seed.next_current_id seed;
  (match Chatml_typechecker.gensym state with
   | _ -> failwith "fresh name wrapped past maximum"
   | exception Chatml_typechecker.Type_error _ -> ());
  let exhausted =
    Chatml_typechecker.Value_scope.add_local
      Chatml_typechecker.Value_scope.empty
      ~name:"x"
      ~scheme:
        (Chatml_typechecker.Generic
           { name = generated_name Int.max_value; equality = None })
  in
  match copied exhausted ~limits:(copy_limits ()) with
  | Error Copy.Error.Name_counter_overflow -> ()
  | _ -> failwith "exhausted imported name was accepted"
;;

let%test_unit "a declared closed alias remains usable across typed submissions" =
  let owner = owner () in
  let declaration = ready owner "alias.chatml" "type token = { value : int }" in
  ignore (finish owner (prepare owner declaration) Typing.Published.After_success);
  let use = ready owner "alias-use.chatml" "let typed : token = { value = 1 }" in
  ignore (finish owner (prepare owner use) Typing.Published.After_success)
;;
