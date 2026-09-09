open Core
open Agent_store_test_fixtures
module Scan = Agent_store.Blob_reference_scan
module Id = Agent_protocol.Id.Blob

let ids =
  List.map [ "blb_a"; "blb_ab"; "blb_complete-reference_1"; "blb_absent" ] ~f:(fun text ->
    Id.of_string text |> protocol_ok)
;;

let%expect_test
    "reference scans preserve embedded and overlapping IDs across arbitrary byte chunks"
  =
  let contents =
    "\000résumé:blb_complete-reference_1-tail\\\"blb_ab\\\";blb_incomplete\255"
  in
  let expected =
    List.filter ids ~f:(fun id ->
      String.is_substring contents ~substring:(Id.to_string id))
  in
  for chunk_size = 1 to String.length contents + 1 do
    let scan = Scan.create ids |> protocol_ok in
    let rec feed offset =
      match offset < String.length contents with
      | false -> ()
      | true ->
        let len = Int.min chunk_size (String.length contents - offset) in
        Scan.feed scan (String.sub contents ~pos:offset ~len);
        feed (offset + len)
    in
    feed 0;
    assert (List.equal Id.equal expected (Scan.references scan))
  done;
  print_s [%sexp (List.map expected ~f:Id.to_string : string list)];
  print_endline
    "every chunk size matched the whole-root oracle, including one-byte Unicode/binary \
     splits";
  [%expect
    {|
    (blb_a blb_ab blb_complete-reference_1)
    every chunk size matched the whole-root oracle, including one-byte Unicode/binary splits
    |}]
;;

let%expect_test
    "root boundaries and own-metadata suppression do not lose external references"
  =
  let own = Id.of_string "blb_owner" |> protocol_ok in
  let other = Id.of_string "blb_other" |> protocol_ok in
  let scan = Scan.create [ own; other ] |> protocol_ok in
  Scan.feed scan "blb_";
  Scan.begin_root scan;
  Scan.feed scan "owner";
  assert (List.is_empty (Scan.references scan));
  Scan.begin_root scan;
  Scan.feed ~ignore:own scan "metadata: blb_owner and blb_";
  Scan.feed ~ignore:own scan "other";
  assert (not (Scan.referenced scan own));
  assert (Scan.referenced scan other);
  Scan.begin_root scan;
  Scan.feed scan "historical reference: blb_owner";
  Scan.begin_root scan;
  Scan.feed ~ignore:own scan "blb_owner";
  print_s [%sexp (List.map (Scan.references scan) ~f:Id.to_string : string list)];
  print_endline
    "distinct roots cannot concatenate IDs; ignoring self does not erase another root's \
     reference";
  [%expect
    {|
    (blb_other blb_owner)
    distinct roots cannot concatenate IDs; ignoring self does not erase another root's reference
    |}]
;;
