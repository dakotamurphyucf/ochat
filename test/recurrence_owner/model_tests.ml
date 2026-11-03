open! Core
open Support

module Model = struct
  type t =
    | Manual
    | Enabled
    | Stopped
    | Source
    | Retired
  [@@deriving equal]

  let actual entry =
    match E.control entry with
    | Enabled -> Enabled
    | Paused Operator_pause -> Manual
    | Paused Controller_stop -> Stopped
    | Paused Source_review -> Source
    | Paused Recovery_review -> failwith "unexpected recovery hold"
    | Retired _ -> Retired
  ;;

  let stop = function
    | Enabled -> Stopped
    | (Manual | Stopped | Source | Retired) as value -> value
  ;;

  let source = function
    | Enabled -> Source
    | (Manual | Stopped | Source | Retired) as value -> value
  ;;
end

let%expect_test "independent bulk-control model, immutable receipts, and lifecycle epochs"
  =
  let catalog, _ = create C.empty "a" in
  let catalog, _ = create catalog "b" in
  let a = ref Model.Manual
  and b = ref Model.Manual in
  let catalog =
    List.fold (List.init 40 ~f:Fn.id) ~init:catalog ~f:(fun catalog step ->
      let catalog =
        match step % 5 with
        | 0 ->
          a := Enabled;
          fst (fresh catalog ("enable-a-" ^ Int.to_string step) (Enable (id "a")))
        | 1 ->
          b := Enabled;
          fst (fresh catalog ("enable-b-" ^ Int.to_string step) (Enable (id "b")))
        | 2 ->
          a := Manual;
          fst (fresh catalog ("pause-a-" ^ Int.to_string step) (Pause (id "a")))
        | 3 ->
          a := Model.stop !a;
          b := Model.stop !b;
          C.pause_for_stop catalog |> protocol_ok |> Option.value ~default:catalog
        | _ ->
          a := Model.source !a;
          b := Model.source !b;
          C.hold_for_source catalog |> protocol_ok |> Option.value ~default:catalog
      in
      assert (Model.equal !a (Model.actual (entry catalog "a")));
      assert (Model.equal !b (Model.actual (entry catalog "b")));
      let restored = roundtrip catalog in
      assert (
        D.Json.equal
          (C.to_jsonaf catalog |> protocol_ok)
          (C.to_jsonaf restored |> protocol_ok));
      restored)
  in
  let enabled, _ = fresh catalog "enable-before-epoch" (Enable (id "a")) in
  let lifecycle = Lifecycle.of_int64 3L |> protocol_ok in
  let old_request = request enabled "old-enable" (Enable (id "a")) in
  assert (Result.is_error (apply enabled ~lifecycle old_request));
  let retired =
    C.retire_stale_births enabled ~current_revision:lifecycle ~archived:false
    |> protocol_ok
    |> Option.value_exn
  in
  assert (
    List.for_all (C.entries retired) ~f:(fun entry ->
      match E.control entry with
      | Retired Lifecycle_changed -> true
      | _ -> false));
  assert (
    Result.is_error
      (apply
         retired
         ~lifecycle
         (request retired ~lifecycle "never-resurrect" (Enable (id "a")))));
  assert (
    Result.is_error
      (apply
         retired
         ~lifecycle
         (request retired ~lifecycle "never-edit" (Edit (id "a", capture ())))));
  let next, _ = create retired ~lifecycle "next" in
  assert (
    Lifecycle.equal (F.birth_lifecycle_revision (E.current (entry next "next"))) lifecycle);
  assert (
    Option.is_none
      (C.retire_stale_births next ~current_revision:lifecycle ~archived:false
       |> protocol_ok));
  assert (
    E.equal_control
      (E.effective_control (entry enabled "a") ~current_revision:lifecycle ~archived:false
       |> protocol_ok)
      (Retired Lifecycle_changed));
  let original = List.hd_exn (C.receipts next) in
  assert (
    Option.value_exn
      (C.receipt
         next
         ~principal_id:original.principal_id
         ~key:original.key
         ~request_sha256:original.request_sha256
       |> protocol_ok)
    |> A.Recurrence_command_receipt.equal original);
  print_endline
    "40 model steps; holds preserved; old births never resurrect; exact receipts retained";
  [%expect
    {| 40 model steps; holds preserved; old births never resurrect; exact receipts retained |}]
;;
