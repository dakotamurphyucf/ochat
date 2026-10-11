open Core
module S = Chatml_source
module P = Chatml_project
module C = Chatml_catalog
module B = Chatml_project_build

let id ~namespace path =
  S.Id.create ~package_namespace:namespace ~relative_path:path |> Or_error.ok_exn
;;

let capture ~namespace path contents =
  S.create
    (id ~namespace path)
    ~contents
    ~expected_digest:None
    ~max_bytes:None
    ~checkpoint:ignore
  |> Or_error.ok_exn
;;

let limits ?(steps = 1_000_000) ?bytes ?(document = Document_schema.Limits.default) () =
  B.Limits.create ~max_work_steps:steps ~max_input_bytes:bytes ~document
  |> Or_error.ok_exn
;;

let reject = function
  | Error error -> error
  | Ok _ -> failwith "invalid public build admitted"
;;

let ok = function
  | Ok value -> value
  | Error _ -> failwith "valid public build rejected"
;;

let assert_same_source expected actual =
  assert (S.Id.equal (S.id expected) (S.id actual));
  assert (S.Digest.equal (S.digest expected) (S.digest actual));
  [%test_eq: string] (S.contents expected) (S.contents actual)
;;

let without_lock build =
  Chatml_prepared_program.sources (B.prepared build)
  |> List.filter ~f:(fun source ->
    not (S.Id.equal (S.id source) (S.id (B.lock_source build))))
;;

let assert_checkpoint_identity ~f =
  let polls = ref 0 in
  f (fun () -> incr polls);
  assert (!polls > 0);
  let exception Stop of unit ref in
  for stop = 1 to !polls do
    let token = ref () in
    let count = ref 0 in
    let checkpoint () =
      incr count;
      if Int.equal !count stop then raise (Stop token)
    in
    (match f checkpoint with
     | () -> failwith "checkpoint exception swallowed"
     | exception Stop actual -> assert (phys_equal token actual));
    [%test_eq: int] !count stop
  done;
  f ignore
;;

let catalog declaration ~request ~members =
  let p =
    match
      P.decode declaration ~limits:Document_schema.Limits.default ~checkpoint:ignore
    with
    | Ok p -> p
    | Error error -> raise_s [%sexp (P.Error.details error : Document_schema.Error.t)]
  in
  let catalog_ok = function
    | Ok value -> value
    | Error error -> raise_s [%sexp (C.Error.problem error : C.Error.problem)]
  in
  let discovery = C.Plan.Discovery.create p ~request ~checkpoint:ignore |> catalog_ok in
  let observations =
    List.concat_map (P.source_dirs p) ~f:(fun directory ->
      let prefix =
        match P.Source_dir.directory directory with
        | Root -> ""
        | Relative path -> Source.Portable_path.to_string path ^ "/"
      in
      List.filter_map members ~f:(fun source ->
        let path = S.Id.relative_path (S.id source) in
        match String.chop_prefix path ~prefix with
        | Some name when not (String.contains name '/') ->
          C.Plan.Discovery.observe discovery ~directory ~name |> catalog_ok
        | None | Some _ -> None))
  in
  let plan = C.Plan.create discovery ~observations ~checkpoint:ignore |> catalog_ok in
  C.create plan ~sources:(declaration :: members) ~checkpoint:ignore |> catalog_ok
;;
