open Core
module P = Chatml_project
module S = Chatml_source

let source contents =
  let id =
    S.Id.create ~package_namespace:"bootstrap-test" ~relative_path:"project.chatml.json"
    |> Or_error.ok_exn
  in
  S.create id ~contents ~expected_digest:None ~max_bytes:None ~checkpoint:ignore
  |> Or_error.ok_exn
;;

let%test_unit "source-free bootstrap and full decode share actual root validation" =
  let limits = Document_schema.Limits.default in
  List.iter
    [ {|{"schema_version":1,"namespace":"actual"}|}
    ; {|{"namespace":"actual","schema_version":1}|}
    ; {|{"schema_version":2,"namespace":"actual"}|}
    ; {|{"schema_version":1.0,"namespace":"actual"}|}
    ; {|{"schema_version":1,"namespace":""}|}
    ; {|{"schema_version":1,"namespace":null}|}
    ; {|{"schema_version":1}|}
    ; {|{"schema_version":null,"namespace":"actual"}|}
    ; {|{"schema_version":1,"namespace":"a","namespace":"b"}|}
    ; {|[]|}
    ]
    ~f:(fun contents ->
      match
        ( P.Namespace.of_contents contents ~limits ~checkpoint:ignore
        , P.decode (source contents) ~limits ~checkpoint:ignore )
      with
      | Ok namespace, Ok declaration ->
        assert (String.equal namespace (P.namespace declaration))
      | Error raw, Error owned ->
        assert (Document_schema.Error.equal raw (P.Error.details owned))
      | Ok _, Error _ | Error _, Ok _ -> failwith "bootstrap/full validation diverged");
  let unknown = {|{"schema_version":1,"namespace":"actual","unknown":1}|} in
  assert (Result.is_ok (P.Namespace.of_contents unknown ~limits ~checkpoint:ignore));
  assert (Result.is_error (P.decode (source unknown) ~limits ~checkpoint:ignore));
  Raw_invariants.sweep (fun checkpoint ->
    ignore
      (P.Namespace.of_contents Fixture.minimal_project ~limits ~checkpoint
       |> function
       | Ok _ -> ()
       | Error _ -> failwith "valid namespace rejected"))
;;
