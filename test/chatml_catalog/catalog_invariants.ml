open Core
module P = Chatml_project
module S = Chatml_source
module C = Chatml_catalog

let capture ?(namespace = "test") path contents =
  let id =
    S.Id.create ~package_namespace:namespace ~relative_path:path |> Or_error.ok_exn
  in
  S.create id ~contents ~expected_digest:None ~max_bytes:None ~checkpoint:ignore
  |> Or_error.ok_exn
;;

let project bytes =
  match
    P.decode
      (capture "chatml.project.json" bytes)
      ~limits:Document_schema.Limits.default
      ~checkpoint:ignore
  with
  | Ok value -> value
  | Error error -> raise_s [%sexp (P.Error.details error : Document_schema.Error.t)]
;;

let request = P.Selection.Library { environment = None }

let ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (C.Error.problem error : C.Error.problem)]
;;

let plan project names =
  let discovery = C.Plan.Discovery.create project ~request ~checkpoint:ignore |> ok in
  let directory = List.hd_exn (P.source_dirs project) in
  let observations =
    List.filter_map names ~f:(fun name ->
      C.Plan.Discovery.observe discovery ~directory ~name |> ok)
  in
  C.Plan.create discovery ~observations ~checkpoint:ignore |> ok
;;

let%test_unit "scope lookup polls complete searches and preserves immutable selections" =
  let p =
    project
      {|{"schema_version":1,"namespace":"test","source_dirs":["."],"environments":{"default":{},"isolated":{"modules":{}}},"file_environments":{"z.chatml":"isolated"}}|}
  in
  let first = capture "a.chatml" "let a = 1" in
  let last = capture "z.chatml" "let z = 2" in
  let catalog =
    C.create
      (plan p [ "a.chatml"; "z.chatml" ])
      ~sources:[ P.source p; first; last ]
      ~checkpoint:ignore
    |> ok
  in
  let lookup ?checkpoint source = C.scope_for ?checkpoint catalog ~source in
  let selected source = lookup source |> Option.value_exn |> P.Scope_spec.name in
  [%test_eq: string] (selected (S.id first)) "default";
  [%test_eq: string] (selected (S.id last)) "isolated";
  let polls = ref 0 in
  ignore (lookup ~checkpoint:(fun () -> incr polls) (S.id last) : P.Scope_spec.t option);
  (* Namespace admission, every preceding member and the selected member must
     be interruptible. Do not lock the assertion to one implementation count. *)
  assert (!polls >= 4);
  let exception Cancelled of unit ref in
  for stop = 1 to !polls do
    let token = ref () in
    let observed = ref 0 in
    let checkpoint () =
      incr observed;
      if !observed = stop then raise (Cancelled token)
    in
    (match lookup ~checkpoint (S.id last) with
     | _ -> failwith "cancelled lookup returned a selection"
     | exception Cancelled actual -> assert (phys_equal actual token));
    [%test_eq: string] (selected (S.id last)) "isolated"
  done;
  assert (Option.is_none (lookup (S.id (P.source p))));
  assert (Option.is_none (lookup (S.id (capture "missing.chatml" ""))));
  assert (Option.is_none (lookup (S.id (capture ~namespace:"other" "z.chatml" ""))))
;;

let paths plan =
  List.map (C.Plan.members plan) ~f:(fun value ->
    ( Source.Portable_path.to_string (C.Plan.Member.path value)
    , C.Plan.Member.requirement value ))
;;

(* Exhaust the bounded enumeration orders independently of catalog sorting. *)
let rec insert_everywhere item = function
  | [] -> [ [ item ] ]
  | head :: tail as items ->
    (item :: items)
    :: List.map (insert_everywhere item tail) ~f:(fun rest -> head :: rest)
;;

let rec permutations = function
  | [] -> [ [] ]
  | head :: tail -> List.concat_map (permutations tail) ~f:(insert_everywhere head)
;;

let%test_unit "handwritten discovery oracle, ordering and override precedence" =
  let fixtures =
    [ ( {|{"schema_version":1,"namespace":"test","source_dirs":["."]}|}
      , [ "a.chatml"; "a.chatmli"; "b.chatml"; "notes.txt"; "subdir" ]
      , [ "a.chatml", C.Plan.Member.Required
        ; "a.chatmli", Optional
        ; "b.chatml", Required
        ; "b.chatmli", Optional
        ] )
    ; ( {|{"schema_version":1,"namespace":"test","source_dirs":["."],"exclude":["a.chatmli","b.chatml"]}|}
      , [ "a.chatml"; "a.chatmli"; "b.chatml"; "notes.txt" ]
      , [ "a.chatml", Required ] )
    ; ( {|{"schema_version":1,"namespace":"test","source_dirs":["."],"modules":{"A":{"implementation":"chosen.raw","interface":"chosen.signature"}}}|}
      , [ "a.chatml"; "A.chatml"; "a.chatmli"; "A.chatmli" ]
      , [ "chosen.raw", Required; "chosen.signature", Required ] )
    ]
  in
  List.iter fixtures ~f:(fun (wire, names, expected) ->
    let p = project wire in
    List.iter (permutations names) ~f:(fun names ->
      assert (
        List.equal
          (fun (left_path, left_requirement) (right_path, right_requirement) ->
             String.equal left_path right_path
             && C.Plan.Member.equal_requirement left_requirement right_requirement)
          (paths (plan p names))
          expected)))
;;

let%test_unit "discovery affinity and bounded candidate error ownership" =
  let p = project {|{"schema_version":1,"namespace":"test","source_dirs":["."]}|} in
  let d1 = C.Plan.Discovery.create p ~request ~checkpoint:ignore |> ok in
  let d2 = C.Plan.Discovery.create p ~request ~checkpoint:ignore |> ok in
  let directory = List.hd_exn (P.source_dirs p) in
  let observation =
    C.Plan.Discovery.observe d1 ~directory ~name:"a.chatml" |> ok |> Option.value_exn
  in
  (match C.Plan.create d2 ~observations:[ observation ] ~checkpoint:ignore with
   | Error e ->
     (match C.Error.problem e with
      | Foreign_observation -> ()
      | _ -> failwith "affinity")
   | Ok _ -> failwith "foreign observation admitted");
  match C.Plan.Discovery.observe d1 ~directory ~name:(String.make 1_000_000 'a') with
  | Error e ->
    assert (
      S.Id.equal (S.id (P.source p)) (S.id (P.Declared_at.source (C.Error.declared_at e))));
    (match C.Error.problem e with
     | Invalid_candidate_path { path_prefix } -> assert (String.length path_prefix = 256)
     | _ -> failwith "prefix")
  | Ok _ -> failwith "unbounded basename admitted"
;;

let%test_unit "validated capture factory preserves aliases and rejects forged inventories"
  =
  let p =
    project
      {|{"schema_version":1,"namespace":"test","modules":{"A":{"implementation":"code.chatml","interface":"public.chatmli"},"Alias":{"implementation":"code.chatml","interface":"public.chatmli"}},"entries":{"main":{"source":"code.chatml","target":"one_off_v1"}}}|}
  in
  let discovery =
    C.Plan.Discovery.create
      p
      ~request:(Entry { name = "main"; environment = None })
      ~checkpoint:ignore
    |> ok
  in
  let plan = C.Plan.create discovery ~observations:[] ~checkpoint:ignore |> ok in
  let code = capture "code.chatml" "let x = 1" in
  let signature = capture "public.chatmli" "val x : int" in
  let sources = [ P.source p; code; signature ] in
  let captured = C.create plan ~sources ~checkpoint:ignore |> ok in
  [%test_eq: int] (List.length (C.definitions captured)) 1;
  [%test_eq: int] (List.length (C.bindings captured)) 2;
  assert (S.Id.equal (Option.value_exn (C.entry captured)) (S.id code));
  assert (
    Chatml_prepared_program.Module_id.equal
      (C.Binding.module_id (List.hd_exn (C.bindings captured)))
      (C.Binding.module_id (List.last_exn (C.bindings captured))));
  [%test_eq: int] (C.Accounting.source_count (C.accounting captured)) 3;
  let reject supplied expected =
    match C.create plan ~sources:supplied ~checkpoint:ignore with
    | Error error ->
      assert (expected (C.Error.problem error));
      assert (
        S.Id.equal
          (S.id (P.source p))
          (S.id (P.Declared_at.source (C.Error.declared_at error))))
    | Ok _ -> failwith "invalid capture inventory admitted"
  in
  reject (code :: sources) (function
    | Duplicate_capture _ -> true
    | _ -> false);
  reject
    [ P.source p; code ]
    (function
      | Missing_capture _ -> true
      | _ -> false);
  reject
    [ capture "chatml.project.json" "different"; code; signature ]
    (function
      | Conflicting_capture _ -> true
      | _ -> false);
  reject
    [ P.source p; capture ~namespace:"foreign" "code.chatml" "let x=1"; signature ]
    (function
      | Wrong_namespace _ -> true
      | _ -> false);
  reject (capture "extra.chatml" "extra" :: sources) (function
    | Unexpected_capture _ -> true
    | _ -> false)
;;

let%test_unit "orphan, portable collision, conflicting aliases and optional omission" =
  let p = project {|{"schema_version":1,"namespace":"test","source_dirs":["."]}|} in
  let d = C.Plan.Discovery.create p ~request ~checkpoint:ignore |> ok in
  let observation =
    C.Plan.Discovery.observe
      d
      ~directory:(List.hd_exn (P.source_dirs p))
      ~name:"alone.chatmli"
    |> ok
    |> Option.value_exn
  in
  (match C.Plan.create d ~observations:[ observation ] ~checkpoint:ignore with
   | Error e ->
     (match C.Error.problem e with
      | Orphan_interface _ -> ()
      | _ -> failwith "orphan")
   | Ok _ -> failwith "orphan admitted");
  let check wire supplied expected =
    let p = project wire in
    let d = C.Plan.Discovery.create p ~request ~checkpoint:ignore |> ok in
    let plan = C.Plan.create d ~observations:[] ~checkpoint:ignore |> ok in
    match C.create plan ~sources:(P.source p :: supplied) ~checkpoint:ignore with
    | Error e -> assert (expected (C.Error.problem e))
    | Ok _ -> failwith "invalid association admitted"
  in
  check
    {|{"schema_version":1,"namespace":"test","modules":{"A":{"implementation":"code.raw"},"B":{"implementation":"Code.raw"}}}|}
    [ capture "code.raw" "a"; capture "Code.raw" "b" ]
    (function
      | Portable_collision _ -> true
      | _ -> false);
  check
    {|{"schema_version":1,"namespace":"test","modules":{"A":{"implementation":"code.raw","interface":"a.signature"},"B":{"implementation":"code.raw","interface":"b.signature"}}}|}
    [ capture "code.raw" "a"; capture "a.signature" "a"; capture "b.signature" "b" ]
    (function
      | Conflicting_interfaces _ -> true
      | _ -> false);
  let p =
    project
      {|{"schema_version":1,"namespace":"test","modules":{"A":{"implementation":"code.chatml"}}}|}
  in
  let d = C.Plan.Discovery.create p ~request ~checkpoint:ignore |> ok in
  let plan = C.Plan.create d ~observations:[] ~checkpoint:ignore |> ok in
  let value =
    C.create plan ~sources:[ P.source p; capture "code.chatml" "code" ] ~checkpoint:ignore
    |> ok
  in
  assert (
    Option.is_none
      (Chatml_prepared_program.Definition.interface (List.hd_exn (C.definitions value))))
;;

let%test_unit "bounded generated companion path; explicit interface bypasses guessing" =
  let implementation = String.make 1017 'a' ^ ".chatml" in
  let wire suffix =
    sprintf
      {|{"schema_version":1,"namespace":"test","modules":{"A":{"implementation":"%s"%s}}}|}
      implementation
      suffix
  in
  let p = project (wire "") in
  let d = C.Plan.Discovery.create p ~request ~checkpoint:ignore |> ok in
  (match C.Plan.create d ~observations:[] ~checkpoint:ignore with
   | Error e ->
     (match C.Error.problem e with
      | Invalid_candidate_path { path_prefix } -> assert (String.length path_prefix = 256)
      | _ -> failwith "companion bounds")
   | Ok _ -> failwith "oversized implicit companion admitted");
  let p = project (wire {|,"interface":"public.signature"|}) in
  let d = C.Plan.Discovery.create p ~request ~checkpoint:ignore |> ok in
  ignore (C.Plan.create d ~observations:[] ~checkpoint:ignore |> ok : C.Plan.t)
;;
