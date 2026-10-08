open! Core
module F = Runtime_host_tests
module P = Agent_protocol
module DTO = P.Provider_operator
module Actor = Operator_authorization
module Runtime = Provider_runtime
module Secret = Provider_secret_store
module B = Inference_host.Credential_bridge

let%expect_test "original actor expires during secure input without replacing working key"
  =
  F.with_fixture (fun _ sw _ create _ ->
    let runtime = create () in
    ignore (F.setup runtime : P.Method_result.t);
    let reads = ref 0 in
    ignore (F.enroll runtime ~sw "one" "working-key" reads : DTO.Configuration_result.t);
    let original = List.hd_exn (F.status runtime).profiles in
    let now = ref (P.Timestamp.of_string "2026-10-08T12:00:00Z" |> F.ok) in
    let expires_at = P.Timestamp.of_string "2026-10-08T12:00:01Z" |> F.ok in
    let actor =
      Actor.bounded ~principal:F.principal ~now:(fun () -> !now) ~expires_at |> F.ok
    in
    assert (Actor.is_current actor);
    (match
       Runtime.enroll_private_key
         runtime
         ~actor
         ~profile:(F.profile "one")
         ~key:(F.key "expired-input")
         ~source_reference:"synthetic:expired-input"
         ~sw
         ~read:(fun ~sw:_ ->
           incr reads;
           Eio.Fiber.yield ();
           now := expires_at;
           Secret.Secret.of_bytes
             (Bytes.of_string "synthetic-replacement-never-published")
           |> Result.map_error ~f:(fun _ -> B.Error.Invalid_credential))
     with
     | Error Denied -> ()
     | _ -> failwith "expired actor replaced the working key");
    assert (!reads = 2 && not (Actor.is_current actor));
    let after = List.hd_exn (F.status runtime).profiles in
    assert (Option.equal Int64.equal original.auth_epoch after.auth_epoch);
    assert (
      Option.equal
        DTO.Revision.equal
        original.credential_revision
        after.credential_revision);
    (match
       Runtime.dispatch runtime ~actor (P.Command.Provider_status { profile = None })
     with
     | Error Denied -> ()
     | _ -> failwith "expired actor read provider status");
    (* Denial cancels only this candidate; future explicitly authorized enrollment
       can proceed instead of finding a stranded pending replacement. *)
    ignore
      (F.enroll runtime ~sw "one" "authorized-after-denial" reads
       : DTO.Configuration_result.t);
    assert (!reads = 3);
    Runtime.close runtime);
  print_endline
    "expired original proof denied; working key retained; pending candidate retired";
  [%expect
    {| expired original proof denied; working key retained; pending candidate retired |}]
;;
