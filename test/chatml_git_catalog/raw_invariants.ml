open Core
module H = Chatml_git_host
module O = Git_object
module T = Git_tree
module C = Git_commit

let ok = function
  | Ok value -> value
  | Error _ -> failwith "raw fixture rejected"
;;

let object_fixture format kind body =
  let bytes = kind ^ " " ^ Int.to_string (String.length body) ^ "\000" ^ body in
  let hex =
    match format with
    | H.Pin.Sha1 -> Digestif.SHA1.(to_hex (digest_string bytes))
    | Sha256 -> Digestif.SHA256.(to_hex (digest_string bytes))
  in
  let id = H.Object_id.create ~object_format:format hex |> Or_error.ok_exn in
  id, hex ^ " " ^ kind ^ " " ^ Int.to_string (String.length body) ^ "\n" ^ body ^ "\n"
;;

let verify
      ?(checkpoint = ignore)
      ?(max_bytes = 100000)
      ?(admit_bytes = ignore)
      id
      wire
      expected
  =
  O.verify id ~wire ~expected ~max_bytes ~admit_bytes ~checkpoint
;;

let verified format kind body expected =
  let id, wire = object_fixture format kind body in
  verify id wire expected |> ok
;;

let sweep f =
  let count = ref 0 in
  f (fun () -> incr count);
  assert (!count > 0);
  for target = 1 to !count do
    let observed = ref 0 in
    let marker = Failure ("checkpoint " ^ Int.to_string target) in
    match
      f (fun () ->
        incr observed;
        if !observed = target then raise marker)
    with
    | () -> failwith "checkpoint did not interrupt"
    | exception exn -> assert (phys_equal exn marker)
  done;
  f ignore
;;

let%test_unit "raw object framing and hash checks precede captured bodies" =
  List.iter [ H.Pin.Sha1; Sha256 ] ~f:(fun format ->
    let body = "a\000b\r\n\255" in
    let id, wire = object_fixture format "blob" body in
    let hex = H.Object_id.to_string id in
    let admission = ref 0 in
    let result =
      verify ~admit_bytes:(fun bytes -> admission := bytes) id wire O.Kind.Blob
    in
    assert (String.equal (O.body (ok result)) body);
    assert (!admission = String.length body);
    let rejects wire error =
      admission := 0;
      assert (
        Result.equal
          (fun _ _ -> false)
          O.Error.equal
          (verify ~admit_bytes:(fun _ -> incr admission) id wire O.Kind.Blob)
          (Error error));
      assert (!admission = 0)
    in
    rejects (hex ^ " missing\n") Missing;
    rejects (hex ^ " blob 999999999999999999999999999999999\n") Malformed_protocol;
    rejects (wire ^ "\n") Malformed_protocol;
    rejects (String.drop_suffix wire 1) Malformed_protocol;
    rejects ("x" ^ String.drop_prefix wire 1) Malformed_protocol;
    rejects (hex ^ " tree 6\n" ^ body ^ "\n") Wrong_kind;
    assert (
      Result.is_error (verify ~max_bytes:(String.length body - 1) id wire O.Kind.Blob));
    let damaged = hex ^ " blob 6\n" ^ "x\000b\r\n\255" ^ "\n" in
    assert (
      Result.equal
        (fun _ _ -> false)
        O.Error.equal
        (verify id damaged O.Kind.Blob)
        (Error Hash_mismatch)))
;;

let tree_entry format mode name =
  let width =
    match format with
    | H.Pin.Sha1 -> 20
    | Sha256 -> 32
  in
  mode ^ " " ^ name ^ "\000" ^ String.make width '\000'
;;

let%test_unit "raw tree ordering, names and modes use exact Git bytes" =
  List.iter [ H.Pin.Sha1; Sha256 ] ~f:(fun format ->
    let decode body =
      T.decode (verified format "tree" body O.Kind.Tree) ~max_entries:8 ~checkpoint:ignore
    in
    let entries specs =
      List.map specs ~f:(fun (mode, name) -> tree_entry format mode name)
      |> String.concat ~sep:""
    in
    let valid =
      decode (entries [ "100644", "a.ext"; "40755", "a"; "100644", "\255" ]) |> ok
    in
    assert (List.length (T.entries valid) = 3);
    assert (T.Mode.equal (T.Entry.mode (List.nth_exn (T.entries valid) 1)) Unsupported);
    assert (String.equal (T.Entry.name (List.last_exn (T.entries valid))) "\255");
    List.iter
      [ entries [ "40000", "a"; "100644", "a.ext" ]
      ; entries [ "100644", "a"; "100644", "a.ext"; "40000", "a" ]
      ; tree_entry format "100644" "a/b"
      ; tree_entry format "100644" ".."
      ; tree_entry format "100644" ""
      ; tree_entry format "777777777777777777777" "a"
      ; String.drop_suffix (tree_entry format "100644" "a") 1
      ; tree_entry format "100644" (String.make 1025 'a')
      ]
      ~f:(fun body -> assert (Result.is_error (decode body)));
    ignore (decode (tree_entry format "100644" (String.make 1024 'a')) |> ok);
    let all_modes =
      decode (entries [ "100644", "a"; "100755", "b"; "120000", "c"; "160000", "d" ])
      |> ok
    in
    assert (
      List.equal
        T.Mode.equal
        (List.map (T.entries all_modes) ~f:T.Entry.mode)
        [ Regular; Executable; Symlink; Gitlink ]);
    assert (
      Result.is_error
        (T.decode
           (verified format "tree" (entries [ "100644", "a"; "100644", "b" ]) O.Kind.Tree)
           ~max_entries:1
           ~checkpoint:ignore)))
;;

let%test_unit "commit root is exact and extended headers remain opaque" =
  List.iter [ H.Pin.Sha1; Sha256 ] ~f:(fun format ->
    let width =
      match format with
      | H.Pin.Sha1 -> 40
      | Sha256 -> 64
    in
    let hex = String.make width 'a' in
    let decode body =
      C.root_tree (verified format "commit" body O.Kind.Commit) ~checkpoint:ignore
    in
    let root =
      decode ("tree " ^ hex ^ "\ngpgsig opaque\n continued\n\nmessage\ntree ignored\n")
      |> ok
    in
    assert (String.equal (H.Object_id.to_string root) hex);
    List.iter
      [ "tree " ^ hex ^ "\nmessage"
      ; "tree " ^ hex ^ "\ntree " ^ hex ^ "\n\n"
      ; "parent " ^ hex ^ "\ntree " ^ hex ^ "\n\n"
      ; "tree " ^ String.drop_suffix hex 1 ^ "\n\n"
      ]
      ~f:(fun body -> assert (Result.is_error (decode body))))
;;

let%test_unit
    "every observed raw traversal checkpoint preserves caller exception identity"
  =
  let body = String.init 65536 ~f:(fun offset -> Char.of_int_exn (offset mod 256)) in
  List.iter [ H.Pin.Sha1; Sha256 ] ~f:(fun format ->
    let id, wire = object_fixture format "blob" body in
    sweep (fun checkpoint -> ignore (verify ~checkpoint id wire O.Kind.Blob |> ok));
    let tree =
      List.init 128 ~f:(fun offset -> tree_entry format "100644" (sprintf "m%03d" offset))
      |> String.concat ~sep:""
      |> fun body -> verified format "tree" body O.Kind.Tree
    in
    sweep (fun checkpoint -> ignore (T.decode tree ~max_entries:128 ~checkpoint |> ok));
    let commit =
      "tree "
      ^ String.make (if H.Pin.equal_object_format format Sha1 then 40 else 64) 'a'
      ^ "\ngpgsig "
      ^ String.make 16384 'x'
      ^ "\n\n"
      |> fun body -> verified format "commit" body O.Kind.Commit
    in
    sweep (fun checkpoint -> ignore (C.root_tree commit ~checkpoint |> ok)))
;;
