open Core
module B = Chatml_project_build
module P = Chatml_project
module S = Chatml_source

let field object_ key =
  match object_ with
  | `Object fields -> List.Assoc.find_exn fields key ~equal:String.equal
  | _ -> failwith "expected fixture object"
;;

let map_field object_ key ~f =
  match object_ with
  | `Object fields ->
    [%test_eq: int] (List.count fields ~f:(fun (name, _) -> String.equal name key)) 1;
    `Object
      (List.map fields ~f:(fun (name, value) ->
         name, if String.equal name key then f value else value))
  | _ -> failwith "expected fixture object"
;;

let map_catalog wire namespace ~f =
  map_field wire "catalogs" ~f:(function
    | `Array catalogs ->
      let matches catalog =
        match field catalog "namespace" with
        | `String name -> String.equal name namespace
        | _ -> failwith "expected namespace"
      in
      [%test_eq: int] (List.count catalogs ~f:matches) 1;
      `Array
        (List.map catalogs ~f:(fun catalog ->
           if matches catalog then f catalog else catalog))
    | _ -> failwith "expected catalog array")
;;

let omit_companion wire =
  map_catalog wire "lib" ~f:(fun catalog ->
    map_field catalog "modules" ~f:(function
      | `Array [ `Object fields ] ->
        [%test_eq: int]
          (List.count fields ~f:(fun (name, _) -> String.equal name "interface"))
          1;
        `Array
          [ `Object
              (List.filter fields ~f:(fun (name, _) ->
                 not (String.equal name "interface")))
          ]
      | _ -> failwith "expected one actual dependency module"))
;;

let change_existing_binding_target wire =
  map_catalog wire "root" ~f:(fun catalog ->
    map_field catalog "scopes" ~f:(function
      | `Array scopes ->
        let changed = ref 0 in
        let result =
          List.map scopes ~f:(fun scope ->
            match field (field scope "source") "path" with
            | `String "src/main.chatml" ->
              map_field scope "bindings" ~f:(function
                | `Array bindings ->
                  `Array
                    (List.map bindings ~f:(fun binding ->
                       match field binding "name" with
                       | `String "Imported" ->
                         incr changed;
                         map_field binding "module" ~f:(fun _ ->
                           `Object
                             [ "namespace", `String "root"
                             ; "path", `String "src/main.chatml"
                             ])
                       | `String _ -> binding
                       | _ -> failwith "expected configured name"))
                | _ -> failwith "expected binding array")
            | `String _ -> scope
            | _ -> failwith "expected source path")
        in
        [%test_eq: int] !changed 1;
        `Array result
      | _ -> failwith "expected scope array"))
;;

let%test_unit
    "primitive-valid saved module and scope relationships must match actual declarations"
  =
  let fixture = Fixtures.make () in
  let original = Fixtures.compose fixture |> Support.ok in
  let sources = Support.without_lock original in
  let wire = Jsonaf.of_string (S.contents (B.lock_source original)) in
  List.iter
    [ omit_companion wire; change_existing_binding_target wire ]
    ~f:(fun changed ->
      let lock =
        Support.capture ~namespace:"root" "selected.lock.json" (Jsonaf.to_string changed)
      in
      (* These edits keep source digests/counts and primitive identity membership
       valid. Decoder success cannot establish the actual declaration relations. *)
      (match
         P.Lock.decode lock ~limits:Document_schema.Limits.default ~checkpoint:ignore
       with
       | Ok _ -> ()
       | Error _ -> failwith "relationship mutation was only a codec rejection");
      let error =
        B.restore
          lock
          ~sources
          ~contract:(Fixtures.contract fixture)
          ~limits:(Support.limits ())
          ~checkpoint:ignore
        |> Support.reject
      in
      (match B.Error.problem error with
       | Lock_mismatch -> ()
       | _ -> failwith "relationship mismatch classification");
      let owner = B.Error.declared_at error |> Option.value_exn |> P.Declared_at.source in
      assert (
        List.exists (lock :: sources) ~f:(fun actual ->
          S.Id.equal (S.id actual) (S.id owner)
          && S.Digest.equal (S.digest actual) (S.digest owner)
          && String.equal (S.contents actual) (S.contents owner))));
  let retry =
    B.restore
      (B.lock_source original)
      ~sources
      ~contract:(Fixtures.contract fixture)
      ~limits:(Support.limits ())
      ~checkpoint:ignore
    |> Support.ok
  in
  Fixtures.assert_model fixture retry;
  Support.assert_same_source (B.lock_source original) (B.lock_source retry)
;;
