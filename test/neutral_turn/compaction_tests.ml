open! Core
module P = History_entry.Payload

let ok = Result.ok_or_failwith

let allocator () =
  History_entry.Allocator.create ~namespace:"captured-compaction" ~next_sequence:0 |> ok
;;

let create allocator view metadata =
  let semantic = P.Semantic.create view ~metadata |> ok in
  let payload =
    P.captured
      semantic
      ~origin:P.Origin.unavailable
      ~raw:(`Object [ "future", `Number "1.00"; "opaque", `String "preserved" ])
    |> ok
  in
  History_entry.create ~allocator payload |> ok
;;

let call allocator path =
  create
    allocator
    (Call
       { kind = Function
       ; name = "read_file"
       ; namespace = Absent
       ; input_bytes = sprintf {|{"path":%S}|} path
       ; async = Absent
       })
    { P.Metadata.empty with call_id = Value "reused" }
;;

let output allocator call text =
  create
    allocator
    (Result
       { kind = Function; relation = Bound (History_entry.id call); output = Text text })
    { P.Metadata.empty with call_id = Value "reused" }
;;

let equal_payload left right =
  Document_schema.Json.equal
    (P.to_json (History_entry.payload left))
    (P.to_json (History_entry.payload right))
;;

let%expect_test
    "read-file collapse follows host occurrences and preserves untouched captures"
  =
  let allocator = allocator () in
  let first = call allocator "a" in
  let other = call allocator "b" in
  let first_result = output allocator first "old a" in
  let other_result = output allocator other "keep b" in
  let newest = call allocator "a" in
  let newest_result = output allocator newest "new a" in
  let entries = [ first; other; first_result; other_result; newest; newest_result ] in
  let compacted =
    Chat_response.Compact_history.collapse_read_file_entries ~placeholder:"stale" entries
  in
  let retained = List.map2_exn entries compacted ~f:equal_payload in
  let ids =
    List.for_all2_exn entries compacted ~f:(fun a b ->
      History_entry.Id.equal (History_entry.id a) (History_entry.id b))
  in
  let edited = List.nth_exn compacted 2 |> History_entry.payload in
  let authored =
    match P.representation edited with
    | Authored -> true
    | Captured _ | Reconstructed _ -> false
  in
  let text =
    match P.Semantic.view (P.semantic edited) with
    | Result { output = Text text; _ } -> text
    | _ -> "unexpected"
  in
  print_s [%sexp (retained : bool list), (ids : bool), (authored : bool), (text : string)];
  [%expect {| ((true true false true true true) true true stale) |}]
;;
