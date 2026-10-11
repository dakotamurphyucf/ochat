open Core
module P = Chatml_project
module S = Chatml_source

let checkpoint () = ()

let capture ?(path = "chatml.project.json") ?(namespace = "test") contents =
  let id =
    S.Id.create ~package_namespace:namespace ~relative_path:path |> Or_error.ok_exn
  in
  S.create id ~contents ~expected_digest:None ~max_bytes:None ~checkpoint
  |> Or_error.ok_exn
;;

let fixture_exn = function
  | Ok value -> value
  | Error error ->
    raise_s [%sexp "Fixture rejected", (P.Error.details error : Document_schema.Error.t)]
;;

let decode_exn source =
  P.decode source ~limits:Document_schema.Limits.default ~checkpoint |> fixture_exn
;;

let select_exn declaration ~request =
  P.select declaration ~request ~checkpoint |> fixture_exn
;;

let assert_source expected ~actual =
  [%test_eq: bool] (S.Id.equal (S.id expected) (S.id actual)) true;
  [%test_eq: bool] (S.Digest.equal (S.digest expected) (S.digest actual)) true;
  [%test_eq: string] (S.contents actual) (S.contents expected)
;;

let assert_declared_at source ~actual ~field_path =
  assert_source source ~actual:(P.Declared_at.source actual);
  [%test_eq: string list] (P.Declared_at.field_path actual) field_path;
  let location = P.Declared_at.location actual in
  [%test_eq: bool] (S.Id.equal (S.Location.source location) (S.id source)) true;
  [%test_eq: bool]
    (S.Digest.equal (S.Location.source_digest location) (S.digest source))
    true;
  let span = S.Location.span location in
  [%test_eq: int] span.left.offset 0;
  [%test_eq: int] span.right.offset (String.length (S.contents source));
  [%test_eq: string] (S.read source location |> Or_error.ok_exn) (S.contents source)
;;

let assert_checkpoint_propagation ~f =
  let observed = ref 0 in
  f (fun () -> Int.incr observed);
  assert (!observed > 0);
  let exception Stop of unit ref in
  for stop_at = 1 to !observed do
    let token = ref () in
    let polls = ref 0 in
    let checkpoint () =
      Int.incr polls;
      if Int.equal !polls stop_at then raise (Stop token)
    in
    (match f checkpoint with
     | () -> failwith "Checkpoint exception was swallowed"
     | exception Stop actual ->
       assert (phys_equal token actual);
       [%test_eq: int] !polls stop_at);
    (* A caller can reuse the same immutable input after any interrupted run. *)
    f (fun () -> ())
  done
;;
