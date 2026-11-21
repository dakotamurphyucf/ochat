open! Core
open Fixtures
module A = Agent_session
module P = Agent_protocol
module H = A.Recovery_rollover_hooks
module S = P.Recovery_window.Scope

let window () =
  S.create
    ~session:Recovery_note_actor_tests.owner
    ~generation:1
    ~window:(P.Id.Context_window.create ())
  |> protocol_ok
;;

let observer revision : P.Invocation.observer =
  { script_id = "installed-moderator"
  ; source_sha256 =
      Chatml_source.Digest.of_contents revision ~checkpoint:ignore
      |> Chatml_source.Digest.to_hex
  }
;;

let claim ~owner ~window ~revision ~observer =
  H.Claim.create
    ~execution:(P.Id.Moderator_execution.create ())
    ~predecessor:owner
    ~window
    ~revision
    ~observer
  |> protocol_ok
;;

let replay hooks =
  let restored = H.to_json hooks |> H.of_json |> protocol_ok in
  assert (H.equal hooks restored);
  restored
;;

let%expect_test "claims fence replay and preserve the explicit empty hint selection" =
  let owner = P.Id.Operation.create () in
  let previous = window () in
  let next = window () in
  let observer = observer "v1" in
  let before = claim ~owner ~window:previous ~revision:18L ~observer in
  let after = claim ~owner ~window:next ~revision:22L ~observer in
  let apply hooks change = H.apply hooks change |> protocol_ok |> replay in
  let hooks = apply H.empty (Claim (Before, before)) in
  assert (Result.is_error (H.apply hooks (Claim (Before, before))));
  assert (Result.is_error (H.apply hooks (Claim (After, after))));
  let hints = Some A.Recovery_rollover_hint_ref.Selection.empty in
  let decision = H.Proceed { hints; continuation = Some Wait } in
  let hooks = apply hooks (Complete_before (before.execution, decision)) in
  let foreign =
    claim
      ~owner
      ~window:next
      ~revision:22L
      ~observer:{ observer with script_id = "elsewhere" }
  in
  assert (Result.is_error (H.apply hooks (Claim (After, foreign))));
  let hooks = apply hooks (Claim (After, after)) in
  assert (Result.is_error (H.apply hooks (Claim (After, after))));
  let hooks = apply hooks (Stop (After, after.execution, Interrupted)) in
  assert (Result.is_error (H.apply hooks (Complete_after (after.execution, Continue))));
  match H.before hooks, H.after hooks with
  | ( Completed (previous, Proceed { hints = Some selection; continuation = Some Wait })
    , Stopped (next, Interrupted) )
    when H.Claim.equal previous before
         && H.Claim.equal next after
         && List.is_empty (A.Recovery_rollover_hint_ref.Selection.references selection) ->
    ()
  | _ -> failwith "replay lost the ordered dispositions or the explicit empty selection"
;;
