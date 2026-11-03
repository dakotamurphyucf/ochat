open! Core
open Fixtures
module A = Agent_session
module P = Agent_protocol
module I = Instruction_content
module C = A.Instruction_capture
module Command = A.Instruction_command
module Selection = A.Instruction_selection
module D = Document_schema
module Storage = Agent_store.Prompt_program_bundle

(* Deliberately pure consistency fixtures. They do not prove an authorized read,
   native activation, current-use permission or provider-context integration. *)
let content_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : I.Error.t)]
;;

let root = I.Root_id.of_string "selection-fixture" |> Or_error.ok_exn
let path value = Source.Portable_path.create value |> Or_error.ok_exn

let loaded ?(root = root) ?(kind = I.Kind.Skill) member contents =
  let id =
    Chatml_source.Id.create ~package_namespace:"inert-fixture" ~relative_path:member
    |> Or_error.ok_exn
  in
  let source =
    Chatml_source.create
      id
      ~contents
      ~expected_digest:None
      ~max_bytes:(Some 1048576)
      ~checkpoint:ignore
    |> Or_error.ok_exn
  in
  let observed =
    I.observe source ~root ~kind ~limits:I.Limits.standard ~checkpoint:ignore
    |> content_ok
  in
  I.load observed ~source ~limits:I.Limits.standard ~checkpoint:ignore |> content_ok
;;

let snapshot values =
  I.Snapshot.capture values ~limits:I.Limits.standard ~checkpoint:ignore |> content_ok
;;

let a = loaded "alpha/SKILL.md" "# Alpha exact bytes\r\n"
let b = loaded "beta/SKILL.md" "# Beta exact bytes\n"
let key value = P.Idempotency_key.of_string value |> protocol_ok

let request selection key_value change =
  Command.create
    ~session_id
    ~generation:0
    ~expected_revision:(Selection.revision selection)
    ~key:(key key_value)
    ~change
  |> protocol_ok
;;

let prepare selection request =
  Selection.prepare
    selection
    ~request
    ~principal_id
    ~session_id
    ~generation:0
    ~now:timestamp
;;

let committed = function
  | Ok (Selection.Commit { selection; receipt }) -> selection, receipt
  | Ok (Retained _) -> failwith "expected fresh instruction receipt"
  | Error error -> raise_s [%sexp (error : P.Error.t)]
;;

let apply selection key change =
  prepare selection (request selection key change) |> committed |> fst
;;

let bytes values = List.map values ~f:I.Loaded.contents

let set raw name value =
  match raw with
  | `Object fields -> `Object (List.Assoc.add fields ~equal:String.equal name value)
  | _ -> failwith "expected fixture object"
;;

let member raw name =
  match D.Json.field raw ~name with
  | Value value -> value
  | Absent | Null -> failwith ("missing fixture " ^ name)
;;

let edit_payload raw f = set raw "payload" (f (member raw "payload"))
let raw selection = Selection.to_jsonaf selection |> protocol_ok

let%expect_test
    "independent bounded reducer oracle: order, retained retries and no-op receipts"
  =
  let ca = C.skills (snapshot [ a ]) |> protocol_ok in
  let cb = C.skills (snapshot [ b ]) |> protocol_ok in
  let alphabet = [ 0; 1; 2; 3 ] in
  List.iter alphabet ~f:(fun x ->
    List.iter alphabet ~f:(fun y ->
      List.iter alphabet ~f:(fun z ->
        let model = ref []
        and selection = ref Selection.empty in
        List.iteri [ x; y; z ] ~f:(fun index code ->
          let change =
            match code with
            | 0 ->
              model := [ I.Loaded.contents a ];
              Command.Change.Replace_skills ca
            | 1 ->
              model := [ I.Loaded.contents b ];
              Replace_skills cb
            | 2 ->
              model
              := List.filter !model ~f:(fun value ->
                   not (String.equal value (I.Loaded.contents a)));
              Remove_skills [ I.Observed.id (I.Loaded.observed a) ]
            | 3 ->
              model := [];
              Clear
            | _ -> assert false
          in
          let command = request !selection ("oracle-" ^ Int.to_string index) change in
          let next, receipt = prepare !selection command |> committed in
          assert (List.equal String.equal !model (bytes (Selection.skills next)));
          assert (Int64.equal (Selection.revision next) (Int64.of_int (index + 1)));
          assert (List.length (Selection.receipts next) = index + 1);
          let reopened = raw next |> Selection.of_jsonaf |> protocol_ok in
          assert (D.Json.equal (raw next) (raw reopened));
          (match prepare reopened command |> protocol_ok with
           | Retained actual -> assert (Command.Receipt.equal receipt actual)
           | Commit _ -> assert false);
          selection := reopened))));
  let noop = request Selection.empty "initial-noop" Clear in
  let accepted, receipt = prepare Selection.empty noop |> committed in
  let changed = apply accepted "after-noop" (Replace_skills ca) in
  (match prepare changed noop |> protocol_ok with
   | Retained actual -> assert (Command.Receipt.equal receipt actual)
   | Commit _ -> assert false);
  let conflicting =
    Command.create
      ~session_id
      ~generation:0
      ~expected_revision:0L
      ~key:(key "initial-noop")
      ~change:(Replace_skills ca)
    |> protocol_ok
  in
  assert (Result.is_error (prepare changed conflicting));
  assert (Result.is_error (prepare changed (request accepted "stale" Clear)));
  assert (
    Result.is_error
      (Command.create
         ~session_id
         ~generation:0
         ~expected_revision:0L
         ~key:(key "kind")
         ~change:(Replace_project ca)));
  print_endline
    "64 independent paths: exact order, one receipt per fresh command, stable retry, \
     immutable no-op key, stale CAS";
  [%expect
    {| 64 independent paths: exact order, one receipt per fresh command, stable retry, immutable no-op key, stale CAS |}]
;;

let%expect_test "project order, qualified skills and empty selected chain remain distinct"
  =
  let guidance member text = loaded ~kind:Project_guidance member text in
  let top = guidance "AGENTS.md" "# Root"
  and leaf = guidance "src/AGENTS.md" "# Src" in
  let scope = I.Scope.of_path (path "src/sub") in
  let project = C.project ~root ~scope (snapshot [ top; leaf ]) |> protocol_ok in
  assert (Result.is_error (C.project ~root ~scope (snapshot [ leaf; top ])));
  assert (Result.is_error (C.project ~root ~scope:I.Scope.root (snapshot [ leaf ])));
  let selected = apply Selection.empty "project" (Replace_project project) in
  assert (
    List.equal String.equal (bytes (Selection.project selected)) [ "# Root"; "# Src" ]);
  let other_root = I.Root_id.of_string "other-root" |> Or_error.ok_exn in
  let other_a = loaded ~root:other_root "alpha/SKILL.md" "# Other root" in
  let selected =
    apply
      selected
      "skills"
      (Replace_skills (C.skills (snapshot [ other_a; a; b ]) |> protocol_ok))
  in
  let removed =
    apply selected "partial" (Remove_skills [ I.Observed.id (I.Loaded.observed a) ])
  in
  assert (
    List.equal
      String.equal
      (bytes (Selection.skills removed))
      [ "# Other root"; I.Loaded.contents b ]);
  let empty_project = C.project ~root ~scope (snapshot []) |> protocol_ok in
  let selected = apply removed "empty-project" (Replace_project empty_project) in
  let status = Selection.summary selected in
  assert (status.project_selected && status.project_files = 0 && status.skills = 2);
  let cleared = apply selected "clear" Clear in
  assert (not (Selection.summary cleared).project_selected);
  assert (List.length (Selection.captures cleared) = 3);
  let summary = Selection.summary selected in
  let encoded_summary = P.Instruction_status.to_json summary in
  assert (
    P.Instruction_status.equal
      summary
      (P.Instruction_status.of_json encoded_summary |> protocol_ok));
  let status = Jsonaf.to_string encoded_summary in
  assert (not (String.is_substring status ~substring:"AGENTS"));
  assert (not (String.is_substring status ~substring:"selection-fixture"));
  print_endline
    "root-to-target project order; qualified cross-root skills; selected empty project; \
     body/path/root-free status";
  [%expect
    {| root-to-target project order; qualified cross-root skills; selected empty project; body/path/root-free status |}]
;;

let%expect_test
    "exact nested snapshot custody, deduplication, compatible unknowns and corrupt pool"
  =
  let stored = snapshot [ a; b ] |> I.Snapshot.to_storage in
  let original = Storage.manifest stored in
  let metadata = Storage.Manifest.metadata original in
  let metadata_json =
    edit_payload (D.Document.json metadata) (fun payload ->
      set payload "future_snapshot" (`String "retain exact"))
  in
  let metadata =
    D.Document.inspect metadata_json ~limits:D.Limits.default |> document_ok
  in
  let stored = Storage.create ~metadata ~payloads:(Storage.payloads stored) |> store_ok in
  let captured =
    I.Snapshot.of_storage stored ~limits:I.Limits.standard ~checkpoint:ignore
    |> content_ok
    |> C.skills
    |> protocol_ok
  in
  let selection = apply Selection.empty "capture" (Replace_skills captured) in
  let encoded =
    let encoded = set (raw selection) "future_envelope" (`String "keep") in
    edit_payload encoded (fun payload ->
      set payload "future_selection" (`Object [ "retained", `True ]))
  in
  let selection = Selection.of_jsonaf encoded |> protocol_ok in
  let replacement = C.skills (snapshot [ b ]) |> protocol_ok in
  let selection =
    apply selection "replace" (Replace_skills replacement)
    |> fun s -> apply s "clear" Clear
  in
  let encoded = raw selection in
  assert (D.Json.equal (member encoded "future_envelope") (`String "keep"));
  assert (
    D.Json.equal
      (member (member encoded "payload") "future_selection")
      (`Object [ "retained", `True ]));
  let captures = member (member encoded "payload") "captures" in
  let first =
    match captures with
    | `Array (first :: _) -> first
    | _ -> assert false
  in
  assert (
    D.Json.equal
      (member (member (member first "manifest") "metadata") "payload"
       |> fun payload -> member payload "future_snapshot")
      (`String "retain exact"));
  let pool = member (member encoded "payload") "payloads" in
  assert (
    match pool with
    | `Array rows -> List.length rows = 2
    | _ -> false);
  let missing =
    edit_payload encoded (fun payload -> set payload "payloads" (`Array []))
  in
  assert (Result.is_error (Selection.of_jsonaf missing));
  let unsupported =
    set
      encoded
      "required_semantics"
      (`Array [ `String "future.instructions.must-understand" ])
  in
  assert (Result.is_error (Selection.of_jsonaf unsupported));
  let corrupt =
    edit_payload encoded (fun payload ->
      set
        payload
        "payloads"
        (match pool with
         | `Array (first :: rest) ->
           `Array (set first "contents" (`String "corrupt") :: rest)
         | _ -> assert false))
  in
  assert (Result.is_error (Selection.of_jsonaf corrupt));
  let roundtrip = Selection.t_of_sexp (Selection.sexp_of_t selection) in
  assert (D.Json.equal (raw selection) (raw roundtrip));
  assert (
    Result.is_error
      (Or_error.try_with (fun () ->
         Selection.t_of_sexp (Sexp.Atom (Jsonaf.to_string corrupt)))));
  print_endline
    "exact original snapshot unknowns and inactive bytes retained; shared payloads \
     deduplicate; corrupt/missing custody and sexp bypass reject";
  [%expect
    {| exact original snapshot unknowns and inactive bytes retained; shared payloads deduplicate; corrupt/missing custody and sexp bypass reject |}]
;;

let%expect_test
    "finite lifetime profile reserves a final Clear and exact retry at saturation"
  =
  let project = C.project ~root ~scope:I.Scope.root (snapshot []) |> protocol_ok in
  let selected = ref (apply Selection.empty "selected-empty" (Replace_project project)) in
  for index = 2 to 127 do
    selected := apply !selected ("noop-" ^ Int.to_string index) (Replace_project project)
  done;
  let old = !selected in
  assert (
    Result.is_error (prepare old (request old "unrelated-noop" (Replace_project project))));
  let command = request old "reserved-clear" Clear in
  let cleared, receipt = prepare old command |> committed in
  assert (List.length (Selection.receipts cleared) = 128);
  assert (Result.is_error (prepare cleared (request cleared "full" Clear)));
  (match prepare cleared command |> protocol_ok with
   | Retained actual -> assert (Command.Receipt.equal receipt actual)
   | Commit _ -> assert false);
  assert (List.length (Selection.receipts old) = 127);
  print_endline
    "selected empty group reserves Clear; fresh no-op saturation rejects; final Clear \
     succeeds; exact retry remains unchanged";
  [%expect
    {| selected empty group reserves Clear; fresh no-op saturation rejects; final Clear succeeds; exact retry remains unchanged |}]
;;
