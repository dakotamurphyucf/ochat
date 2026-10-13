open Core
module A = Chatml_authoring_bundle_project
module B = Chatml_project_build
module P = Chatml_project
module R = Chatml_prepared_program
module S = Chatml_source
module W = Agent_protocol.Authoring_bundle.Bundle
module Metadata = Agent_protocol.Authoring_common.Source
module Selection = Agent_protocol.Authoring_context.Context.Selection

let wire_ok = function
  | Ok value -> value
  | Error _ -> failwith "valid bundle metadata rejected"
;;

let adapter_ok = function
  | Ok value -> value
  | Error _ -> failwith "valid captured project rejected"
;;

let metadata source =
  Metadata.create
    ~id:(S.id source)
    ~digest:(S.digest source)
    ~byte_length:(S.byte_length source)
    ~editor_revision:None
  |> wire_ok
;;

let member source = W.Member.create ~source:(metadata source) |> wire_ok

let routes build =
  List.map
    (P.Lock.catalogs (B.lock build))
    ~f:(fun catalog ->
      W.Manifest.Route.create
        ~declaration:(P.Lock.Catalog.declaration catalog)
        ~origin:(P.Lock.Catalog.origin catalog)
        ~dependencies:(P.Lock.Catalog.dependencies catalog)
      |> wire_ok)
;;

let selection target = Selection.create ~target ~environment:None |> wire_ok

let project ?chosen_routes build ~source_members ~selection ~entry ~mode =
  W.Manifest.Project.create
    ~root_source:(P.Lock.root (B.lock build))
    ~selection
    ~entry
    ~source_members:(List.map source_members ~f:S.id)
    ~routes:(Option.value chosen_routes ~default:(routes build))
    ~mode
  |> wire_ok
;;

let manifest project sources =
  W.Manifest.create ~basis:(Project project) ~members:(List.map sources ~f:member)
  |> wire_ok
;;

let limits ~input_bytes =
  A.Limits.create
    ~max_work_steps:100_000
    ~max_retained_input_bytes:input_bytes
    ~document:Document_schema.Limits.default
  |> Or_error.ok_exn
;;

let admit ?(input_bytes = 4_000_000) manifest sources =
  A.Attempt.admit manifest ~sources ~limits:(limits ~input_bytes) ~checkpoint:ignore
;;

let reject expected = function
  | Ok _ -> failwith "invalid captured project admitted"
  | Error error ->
    (match A.Error.problem error, expected with
     | A.Error.Snapshot_mismatch, `Snapshot
     | Missing_or_surplus_member, `Surplus
     | Previous_lock_mismatch, `Previous
     | Input_limit, `Input -> ()
     | Builder problem, `Input
       when match B.Error.problem problem with
            | Limit_exceeded -> true
            | _ -> false -> ()
     | Builder problem, `Input
       when match B.Error.problem problem with
            | Prepared prepared ->
              (match prepared.R.Error.problem with
               | Input_limit _ -> true
               | _ -> false)
            | _ -> false -> ()
     | Result_mismatch, `Result -> ()
     | Attempt_closed, `Closed -> ()
     | _ -> failwith "wrong captured project failure")
;;

let document_manifest ~document ~scripts ~project_sources =
  let member_path = Source.Portable_path.create "entry.chatmd" |> Or_error.ok_exn in
  let document_member =
    W.Manifest.Document_member.create ~source:(S.id document) ~member:member_path
    |> wire_ok
  in
  let basis =
    W.Manifest.Chatmd_document.create
      ~root_source:(S.id document)
      ~document_members:[ document_member ]
      ~scripts
    |> wire_ok
  in
  W.Manifest.create
    ~basis:(Chatmd_document basis)
    ~members:(List.map (document :: project_sources) ~f:member)
  |> wire_ok
;;

let selected_script ~document ~entry ~start_byte ~end_byte ~script_id project =
  let selector =
    W.Manifest.Script.Selector.create
      ~registration_source:(S.id document)
      ~declaration_source:(S.id document)
      ~start_byte
      ~end_byte
      ~script_id
      ~body_digest:(S.digest entry)
      ~body_byte_length:(S.byte_length entry)
    |> wire_ok
  in
  let script =
    W.Manifest.Script.create ~selector ~choice:(Selected_project project) |> wire_ok
  in
  selector, script
;;

let captured_entry source =
  W.Manifest.Project.Captured_entry.create
    ~source:(metadata source)
    ~delivery:Uploaded_member
  |> wire_ok
;;

let find_entry build =
  R.find_source (B.prepared build) (R.entry (B.prepared build)) |> Option.value_exn
;;

let%test_unit "uploaded Library entry is distinct from catalog membership" =
  let moderator =
    Chatml.Chatml_compiler_contract.for_target Moderator_v1 ~checkpoint:ignore
  in
  let standalone = Recomposition_tests.library moderator in
  let fixture = Fixtures.make ~library:true () in
  let overlap =
    Fixtures.compose ~entry:(Captured (Fixtures.main fixture)) fixture |> Support.ok
  in
  let same_namespace =
    Fixtures.compose
      ~entry:
        (Captured (Support.capture ~namespace:"root" "standalone.chatml" "let value = 7"))
      fixture
    |> Support.ok
  in
  List.iter
    [ standalone, moderator
    ; overlap, Fixtures.contract fixture
    ; same_namespace, Fixtures.contract fixture
    ]
    ~f:(fun (original, contract) ->
      let sources = Support.without_lock original in
      let entry = find_entry original in
      let project =
        project
          original
          ~source_members:sources
          ~selection:(selection Library)
          ~entry:(Captured (captured_entry entry))
          ~mode:
            (Compose
               { selected_previous_lock = None
               ; generated_lock =
                   Support.id
                     ~namespace:(S.Id.package_namespace (P.Lock.root (B.lock original)))
                     "uploaded.lock.json"
               })
      in
      let manifest = manifest project sources in
      let validated =
        admit manifest (List.rev sources)
        |> adapter_ok
        |> fun attempt ->
        A.Attempt.validate_top_project attempt ~previous:None ~contract ~checkpoint:ignore
        |> adapter_ok
      in
      let actual = A.Validated.build validated in
      Support.assert_same_source
        entry
        (R.find_source (B.prepared actual) (S.id entry) |> Option.value_exn);
      assert (S.Id.equal (R.entry (B.prepared actual)) (S.id entry)))
;;

let%test_unit "nested Project subsets share one snapshot and one aggregate allowance" =
  let contract =
    Chatml.Chatml_compiler_contract.for_target Moderator_v1 ~checkpoint:ignore
  in
  let original = Recomposition_tests.library contract in
  let sources = Support.without_lock original in
  let entry = find_entry original in
  let document = Support.capture ~namespace:"docs" "entry.chatmd" "ab" in
  let make_project lock_name =
    project
      original
      ~source_members:sources
      ~selection:(selection Library)
      ~entry:(Captured (captured_entry entry))
      ~mode:
        (Compose
           { selected_previous_lock = None
           ; generated_lock = Support.id ~namespace:"role" lock_name
           })
  in
  let first_selector, first =
    selected_script
      ~document
      ~entry
      ~start_byte:0
      ~end_byte:1
      ~script_id:"first"
      (make_project "first.uploaded.lock.json")
  in
  let second_selector, second =
    selected_script
      ~document
      ~entry
      ~start_byte:1
      ~end_byte:2
      ~script_id:"second"
      (make_project "second.uploaded.lock.json")
  in
  let snapshot = document :: sources in
  let upload =
    document_manifest ~document ~scripts:[ first; second ] ~project_sources:sources
  in
  let attempt = admit upload snapshot |> adapter_ok in
  let validate attempt selector =
    A.Attempt.validate_selected_script
      attempt
      ~selector
      ~derived_entry:None
      ~previous:None
      ~contract
      ~checkpoint:ignore
  in
  let first = validate attempt first_selector |> adapter_ok in
  let second = validate attempt second_selector |> adapter_ok in
  assert (
    not
      (S.Digest.equal
         (A.Validated.input_fingerprint first)
         (A.Validated.input_fingerprint second)));
  let snapshot_bytes =
    List.fold snapshot ~init:0 ~f:(fun bytes source -> bytes + S.byte_length source)
  in
  let build_bytes =
    R.Accounting.input_bytes (R.accounting (B.prepared (A.Validated.build first)))
  in
  let cap = snapshot_bytes + (2 * build_bytes) - 1 in
  let attempt = admit ~input_bytes:cap upload snapshot |> adapter_ok in
  ignore (validate attempt first_selector |> adapter_ok : A.Validated.t);
  reject `Input (validate attempt second_selector);
  reject `Closed (validate attempt second_selector);
  let catalog_sources =
    List.filter sources ~f:(fun source -> not (S.Id.equal (S.id source) (S.id entry)))
  in
  let derived_entry =
    W.Manifest.Project.Captured_entry.create
      ~source:(metadata entry)
      ~delivery:Derived_script
    |> wire_ok
  in
  let derived_project =
    project
      original
      ~source_members:catalog_sources
      ~selection:(selection Library)
      ~entry:(Captured derived_entry)
      ~mode:
        (Compose
           { selected_previous_lock = None
           ; generated_lock = Support.id ~namespace:"role" "derived.lock.json"
           })
  in
  let derived_selector, derived_script =
    selected_script
      ~document
      ~entry
      ~start_byte:0
      ~end_byte:1
      ~script_id:"derived"
      derived_project
  in
  let upload =
    document_manifest
      ~document
      ~scripts:[ derived_script ]
      ~project_sources:catalog_sources
  in
  let attempt = admit upload (document :: catalog_sources) |> adapter_ok in
  let derived =
    A.Attempt.validate_selected_script
      attempt
      ~selector:derived_selector
      ~derived_entry:(Some entry)
      ~previous:None
      ~contract
      ~checkpoint:ignore
    |> adapter_ok
  in
  Support.assert_same_source
    entry
    (R.find_source (B.prepared (A.Validated.build derived)) (S.id entry)
     |> Option.value_exn)
;;

let%test_unit "borrowed parser stage is one fail-closed attempt" =
  let document = Support.capture ~namespace:"docs" "entry.chatmd" "body" in
  let upload = document_manifest ~document ~scripts:[] ~project_sources:[] in
  let fresh () = admit upload [ document ] |> adapter_ok in
  let nested = fresh () in
  (match
     A.Attempt.run_stage nested ~checkpoint:ignore ~f:(fun stage ->
       A.Attempt.Stage.checkpoint stage;
       (match A.Attempt.run_stage nested ~checkpoint:ignore ~f:(fun _ -> Ok ()) with
        | Error (Budget_error problem) ->
          (match A.Error.problem problem with
           | Attempt_busy -> ()
           | _ -> failwith "wrong nested stage failure")
        | Ok _ | Error _ -> failwith "nested stage was admitted");
       Ok ())
   with
   | Error (Budget_error problem) ->
     (match A.Error.problem problem with
      | Attempt_closed -> ()
      | _ -> failwith "caught nested rejection did not close attempt")
   | Ok _ | Error _ -> failwith "caught nested rejection published stage");
  let exhausted = fresh () in
  (match
     A.Attempt.run_stage exhausted ~checkpoint:ignore ~f:(fun stage ->
       (match A.Attempt.Stage.debit_input stage ~bytes:Int.max_value with
        | exception _ -> ()
        | () -> failwith "oversized parser debit was admitted");
       Ok ())
   with
   | Error (Budget_error problem) ->
     (match A.Error.problem problem with
      | Attempt_closed -> ()
      | _ -> failwith "caught budget failure did not close attempt")
   | Ok _ | Error _ -> failwith "caught budget failure published stage");
  let escaped = fresh () in
  let borrowed =
    A.Attempt.run_stage escaped ~checkpoint:ignore ~f:(fun stage -> Ok stage)
    |> function
    | Ok stage -> stage
    | Error _ -> failwith "borrowed stage failed"
  in
  (match A.Attempt.Stage.checkpoint borrowed with
   | exception _ -> ()
   | () -> failwith "escaped stage remained live");
  (match A.Attempt.run_stage escaped ~checkpoint:ignore ~f:(fun _ -> Ok ()) with
   | Error (Budget_error problem) ->
     (match A.Error.problem problem with
      | Attempt_closed -> ()
      | _ -> failwith "escaped stage did not close owner")
   | Ok _ | Error _ -> failwith "escaped stage allowed budget reset");
  let exception Interrupted of unit ref in
  let identity = ref () in
  let cancelled = fresh () in
  match
    A.Attempt.run_stage
      cancelled
      ~checkpoint:(fun () -> raise (Interrupted identity))
      ~f:(fun stage ->
        A.Attempt.Stage.checkpoint stage;
        Ok ())
  with
  | exception Interrupted actual -> assert (phys_equal identity actual)
  | Ok _ | Error _ -> failwith "stage cancellation identity lost"
;;

let%test_unit "exact-lock default selection and pinned previous revision are checked" =
  let original = Gate_tests.compose (Gate_tests.git 'a') Initial_prepare |> Support.ok in
  let sources = Support.without_lock original in
  let entry = find_entry original in
  let selected =
    project
      original
      ~source_members:sources
      ~selection:(selection Library)
      ~entry:(Captured (captured_entry entry))
      ~mode:
        (Compose
           { selected_previous_lock = Some (metadata (B.lock_source original))
           ; generated_lock = Support.id ~namespace:"owner" "uploaded.lock.json"
           })
  in
  let upload = manifest selected sources in
  let attempt = admit upload sources |> adapter_ok in
  let validated =
    A.Attempt.validate_top_project
      attempt
      ~previous:(Some original)
      ~contract:Gate_tests.contract
      ~checkpoint:ignore
    |> adapter_ok
  in
  assert (
    S.Id.equal
      (S.id (B.lock_source (A.Validated.build validated)))
      (Support.id ~namespace:"owner" "uploaded.lock.json"));
  let unproven_project =
    project
      original
      ~source_members:sources
      ~selection:(selection Library)
      ~entry:(Captured (captured_entry entry))
      ~mode:
        (Compose
           { selected_previous_lock = None
           ; generated_lock = Support.id ~namespace:"owner" "unproven.lock.json"
           })
  in
  let unproven_manifest = manifest unproven_project sources in
  let unproven_attempt = admit unproven_manifest sources |> adapter_ok in
  (match
     A.Attempt.validate_top_project
       unproven_attempt
       ~previous:None
       ~contract:Gate_tests.contract
       ~checkpoint:ignore
   with
   | Error problem ->
     (match A.Error.problem problem with
      | Builder problem ->
        (match B.Error.problem problem with
         | Needs_update -> ()
         | _ -> failwith "Git provenance was inferred")
      | _ -> failwith "Git provenance rejection lost builder ownership")
   | Ok _ -> failwith "Git provenance was invented");
  let alternate =
    Gate_tests.compose ~environment:"alternate" (Gate_tests.git 'a') Initial_prepare
    |> Support.ok
  in
  let exact_sources = R.sources (B.prepared alternate) in
  let wrong_default =
    project
      alternate
      ~source_members:exact_sources
      ~selection:(selection Library)
      ~entry:(Captured (captured_entry (find_entry alternate)))
      ~mode:(Exact_lock (S.id (B.lock_source alternate)))
  in
  let exact_upload = manifest wrong_default exact_sources in
  let attempt = admit exact_upload exact_sources |> adapter_ok in
  reject
    `Result
    (A.Attempt.validate_top_project
       attempt
       ~previous:None
       ~contract:Gate_tests.contract
       ~checkpoint:ignore);
  let wrong_routes =
    List.map (routes original) ~f:(fun route ->
      if S.Id.equal (W.Manifest.Route.declaration route) (P.Lock.root (B.lock original))
      then
        W.Manifest.Route.create
          ~declaration:(W.Manifest.Route.declaration route)
          ~origin:Local
          ~dependencies:(W.Manifest.Route.dependencies route)
        |> wire_ok
      else route)
  in
  let route_sources = R.sources (B.prepared original) in
  let route_project =
    project
      ~chosen_routes:wrong_routes
      original
      ~source_members:route_sources
      ~selection:(selection Library)
      ~entry:(Captured (captured_entry entry))
      ~mode:(Exact_lock (S.id (B.lock_source original)))
  in
  let route_manifest = manifest route_project route_sources in
  let attempt = admit route_manifest route_sources |> adapter_ok in
  reject
    `Result
    (A.Attempt.validate_top_project
       attempt
       ~previous:None
       ~contract:Gate_tests.contract
       ~checkpoint:ignore)
;;

let%test_unit
    "uploaded exact lock and Local composition retain the captured project model"
  =
  let fixture = Fixtures.make () in
  let original = Fixtures.compose fixture |> Support.ok in
  let complete = R.sources (B.prepared original) in
  let exact_project =
    project
      original
      ~source_members:complete
      ~selection:(selection (Entry "main"))
      ~entry:Declared
      ~mode:(Exact_lock (S.id (B.lock_source original)))
  in
  let exact_manifest = manifest exact_project complete in
  let success_polls = ref 0 in
  let exact =
    admit exact_manifest (List.rev complete)
    |> adapter_ok
    |> fun attempt ->
    A.Attempt.validate_top_project
      attempt
      ~previous:None
      ~contract:(Fixtures.contract fixture)
      ~checkpoint:(fun () -> Int.incr success_polls)
    |> adapter_ok
  in
  Fixtures.assert_model fixture (A.Validated.build exact);
  Support.assert_same_source
    (B.lock_source original)
    (B.lock_source (A.Validated.build exact));
  assert (
    S.Digest.equal
      (A.Validated.manifest_fingerprint exact)
      (W.Manifest.fingerprint exact_manifest));
  assert (!success_polls > 2);
  let interrupted_attempt = admit exact_manifest complete |> adapter_ok in
  let interrupted_polls = ref 0 in
  let exception Interrupted in
  let interrupted_checkpoint () =
    Int.incr interrupted_polls;
    if Int.equal !interrupted_polls (!success_polls - 1) then raise Interrupted
  in
  (match
     A.Attempt.validate_top_project
       interrupted_attempt
       ~previous:None
       ~contract:(Fixtures.contract fixture)
       ~checkpoint:interrupted_checkpoint
   with
   | Ok _ -> failwith "interrupted validation published a result"
   | Error _ -> failwith "checkpoint exception lost its identity"
   | exception Interrupted -> ());
  reject
    `Closed
    (A.Attempt.validate_top_project
       interrupted_attempt
       ~previous:None
       ~contract:(Fixtures.contract fixture)
       ~checkpoint:ignore);
  let selected = Support.without_lock original in
  let generated_lock = Support.id ~namespace:"root" "uploaded.lock.json" in
  let compose_project =
    project
      original
      ~source_members:selected
      ~selection:(selection (Entry "main"))
      ~entry:Declared
      ~mode:(Compose { selected_previous_lock = None; generated_lock })
  in
  let compose_manifest = manifest compose_project selected in
  let composed =
    admit compose_manifest selected
    |> adapter_ok
    |> fun attempt ->
    A.Attempt.validate_top_project
      attempt
      ~previous:None
      ~contract:(Fixtures.contract fixture)
      ~checkpoint:ignore
    |> adapter_ok
  in
  Fixtures.assert_model fixture (A.Validated.build composed);
  assert (S.Id.equal (S.id (B.lock_source (A.Validated.build composed))) generated_lock)
;;

let%test_unit "upload snapshot and selected prior lock revision must match exactly" =
  let fixture = Fixtures.make () in
  let original = Fixtures.compose fixture |> Support.ok in
  let sources = Support.without_lock original in
  let generated_lock = Support.id ~namespace:"root" "uploaded.lock.json" in
  let make_project selected_previous_lock =
    project
      original
      ~source_members:sources
      ~selection:(selection (Entry "main"))
      ~entry:Declared
      ~mode:(Compose { selected_previous_lock; generated_lock })
  in
  let base = manifest (make_project None) sources in
  let changed = Support.capture ~namespace:"root" "src/main.chatml" "let main = 42" in
  let changed_snapshot =
    List.map sources ~f:(fun source ->
      if S.Id.equal (S.id source) (S.id changed) then changed else source)
  in
  reject `Snapshot (admit base changed_snapshot);
  let extra = Support.capture ~namespace:"extra" "unused.chatml" "let unused = 0" in
  let surplus = manifest (make_project None) (extra :: sources) in
  reject `Surplus (admit surplus (extra :: sources));
  let wrong_prior =
    metadata
      (Support.capture
         ~namespace:"root"
         "selected.lock.json"
         (S.contents (B.lock_source original) ^ " "))
  in
  let prior_manifest = manifest (make_project (Some wrong_prior)) sources in
  let attempt = admit prior_manifest sources |> adapter_ok in
  reject
    `Previous
    (A.Attempt.validate_top_project
       attempt
       ~previous:(Some original)
       ~contract:(Fixtures.contract fixture)
       ~checkpoint:ignore)
;;
