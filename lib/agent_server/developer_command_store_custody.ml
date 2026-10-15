open! Core
module Store = Agent_store.Developer_run_store

type kind =
  | Reserve
  | Start
  | Finish

type reconciliation =
  | Installed of Store.Command.View.t
  | Retired_absent

type t =
  { handle : Store.handle
  ; mutable token : Store.Command.Pending.t
  ; kind : kind
  ; admit : unit -> bool
  }

let create ~handle ~token ~kind ~admit = { handle; token; kind; admit }
let handle t = t.handle
let kind t = t.kind

let rec commit t ~remaining =
  match Store.Command.commit t.handle t.token ~admit:t.admit with
  | Ok installed -> Ok installed
  | Error Store.Error.Prewrite_conflict when remaining > 0 ->
    (match Store.Command.reprepare_after_prewrite_conflict t.handle t.token with
     | Ok token ->
       t.token <- token;
       commit t ~remaining:(remaining - 1)
     | Error _ as error -> error)
  | Error _ as error -> error
;;

let reconcile t =
  let open Result.Let_syntax in
  let%bind resolution = Store.Command.reconcile t.handle t.token in
  match resolution with
  | Store.Command.Installed view -> Ok (Installed view)
  | Diverged ->
    (match Store.Command.reprepare_after_prewrite_conflict t.handle t.token with
     | Error _ -> Error Store.Error.Conflict
     | Ok token ->
       t.token <- token;
       (match t.kind with
        | Reserve | Start ->
          let%map () = Store.Command.retire_absent_admission t.handle token in
          Retired_absent
        | Finish ->
          let%map installed = commit t ~remaining:3 in
          Installed installed.view))
  | Absent ->
    (match t.kind with
     | Reserve | Start ->
       let%map () = Store.Command.retire_absent_admission t.handle t.token in
       Retired_absent
     | Finish ->
       let%bind token = Store.Command.reprepare_same t.handle t.token in
       t.token <- token;
       let%map installed = commit t ~remaining:3 in
       Installed installed.view)
;;
