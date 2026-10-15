open! Core
module Store = Agent_store.Developer_run_store

module Acquired = struct
  type t =
    { handle : Store.handle
    ; claim : Store.Run_claim.t
    ; window : Store.Recovery.t
    }

  let handle t = t.handle
  let claim t = t.claim
  let window t = t.window
end

let open_existing store ~run_id ~on_claim =
  let open Result.Let_syntax in
  let%bind root = Store.Result_root.find_existing store run_id in
  match root with
  | None -> Ok None
  | Some root ->
    let%bind observed = Store.Result_root.observe_latest root in
    let checkpoint = Store.Observation.checkpoint observed in
    let%bind handle, _ =
      Store.admit_epoch
        store
        ~owner:(Store.Checkpoint.owner checkpoint)
        ~capture_fingerprint:(Store.Checkpoint.capture_fingerprint checkpoint)
        ~previous:(Some observed)
    in
    let%bind claim = Store.Run_claim.acquire ~store ~handle in
    on_claim claim;
    let%map window = Store.Recovery.open_ ~claim ~handle in
    Some { Acquired.handle; claim; window }
;;

let finish acquired ~recover ~map_store_error =
  if Store.Recovery.is_finished (Acquired.window acquired)
  then Ok ()
  else
    let open Result.Let_syntax in
    let%bind () =
      recover
        ~claim:(Acquired.claim acquired)
        ~handle:(Acquired.handle acquired)
        ~window:(Acquired.window acquired)
    in
    Store.Recovery.finish (Acquired.window acquired)
    |> Result.map_error ~f:map_store_error
;;
