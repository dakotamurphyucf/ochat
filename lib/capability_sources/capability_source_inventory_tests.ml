open! Core
module I = Capability_source_inventory

let get = function
  | Ok value -> value
  | Error error -> failwith (I.Error.message error)
;;

let raw members = I.{ id = "sample"; title = "Sample"; entry = "main.chatml"; members }
let admit raw = I.admit [ raw ] ~limits:I.Limits.default ~checkpoint:ignore
let only inventory = List.hd_exn (I.list inventory ~max_items:1 ~checkpoint:ignore |> get)

let%test_unit "canonical resource identity is independent of input member order" =
  let inputs =
    [ "main.chatml", "let main input = Task.pure(input)"
    ; "helper.chatml", "let value = 1"
    ]
  in
  let a = only (admit (raw inputs) |> get) in
  let b = only (admit (raw (List.rev inputs)) |> get) in
  assert (Chatml_source.Digest.equal (I.Resource.digest a) (I.Resource.digest b));
  [%test_eq: string list]
    (List.map (I.Resource.members a) ~f:(fun member ->
       Source.Portable_path.to_string (I.Member.path member)))
    [ "helper.chatml"; "main.chatml" ]
;;

let%test_unit "raw admission rejects ambiguous paths before publishing resources" =
  List.iter
    [ [ "main.chatml", ""; "main.chatml", "" ]
    ; [ "main.chatml", ""; "MAIN.chatml", "" ]
    ; [ "main.chatml", ""; "a", ""; "a/child", "" ]
    ; [ "main.chatml", ""; "../escape", "" ]
    ; [ "other.chatml", "" ]
    ]
    ~f:(fun members ->
      match admit (raw members) with
      | Error error -> assert (I.Error.equal_code (I.Error.code error) Invalid_inventory)
      | Ok _ -> failwith "ambiguous raw inventory was accepted")
;;

let%test_unit
    "admission limits and caller cancellation cannot publish a partial inventory"
  =
  let limits =
    I.Limits.create
      ~max_resources:1
      ~max_members:1
      ~max_member_bytes:8
      ~max_total_bytes:1024
      ~max_work_steps:1000
    |> get
  in
  (match I.admit [ raw [ "main.chatml", "oversized" ] ] ~limits ~checkpoint:ignore with
   | Error error -> assert (I.Error.equal_code (I.Error.code error) Limit_exceeded)
   | Ok _ -> failwith "member limit was ignored");
  let exception Cancelled in
  match
    I.admit [ raw [ "main.chatml", "" ] ] ~limits ~checkpoint:(fun () -> raise Cancelled)
  with
  | exception Cancelled -> ()
  | Ok _ | Error _ -> failwith "caller exception was swallowed"
;;

let%test_unit
    "duplicate resource IDs reject and changing exact bytes changes tree identity"
  =
  let original = raw [ "main.chatml", "one" ] in
  (match I.admit [ original; original ] ~limits:I.Limits.default ~checkpoint:ignore with
   | Error error -> assert (I.Error.equal_code (I.Error.code error) Invalid_inventory)
   | Ok _ -> failwith "duplicate resource IDs were accepted");
  let before = only (admit original |> get) in
  let after = only (admit (raw [ "main.chatml", "two" ]) |> get) in
  assert (
    not (Chatml_source.Digest.equal (I.Resource.digest before) (I.Resource.digest after)))
;;
