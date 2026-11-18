open! Core
module P = Agent_protocol
module W = P.Instruction_control

type t =
  { connection : Connection.t
  ; session_id : P.Id.Session.t
  ; attachment : unit -> P.Id.Attachment.t
  }

let create ~connection ~session_id ~attachment = { connection; session_id; attachment }

let request t request =
  let%bind.Result result =
    Connection.request_without_history t.connection (P.Command.Instructions request)
  in
  match result with
  | P.Method_result.Instructions result
    when String.equal (W.Result.method_name result) (W.Request.method_name request) ->
    Ok result
  | _ -> Error (P.Error.invalid_request "unexpected instruction control result")
;;

let inspect t =
  let%bind.Result result =
    request
      t
      (W.Request.Inspect
         { session_id = t.session_id; attachment_id = Some (t.attachment ()) })
  in
  match result with
  | W.Result.Inspect view -> Ok view
  | _ -> Error (P.Error.invalid_request "unexpected instruction inspection result")
;;

let refresh t ~selection =
  let%bind.Result result =
    request t (W.Request.Refresh { session_id = t.session_id; selection })
  in
  match result with
  | W.Result.Refresh discovery -> Ok discovery
  | _ -> Error (P.Error.invalid_request "unexpected instruction discovery result")
;;

let capture t source =
  let%bind.Result result =
    request t (W.Request.Capture { session_id = t.session_id; source })
  in
  match result with
  | W.Result.Capture capture -> Ok capture
  | _ -> Error (P.Error.invalid_request "unexpected instruction capture result")
;;

let activate t ~expected_generation ~expected_revision ~key (capture : W.Capture.t) =
  let%bind.Result result =
    request
      t
      (W.Request.Activate
         { session_id = t.session_id
         ; attachment_id = t.attachment ()
         ; expected_generation
         ; expected_revision
         ; source = capture.source
         ; capture_id = capture.capture_id
         ; key
         })
  in
  match result with
  | W.Result.Activate receipt -> Ok receipt
  | _ -> Error (P.Error.invalid_request "unexpected instruction activation result")
;;

let remove t ~expected_generation ~expected_revision ~key change =
  let%bind.Result result =
    request
      t
      (W.Request.Remove
         { session_id = t.session_id
         ; attachment_id = t.attachment ()
         ; expected_generation
         ; expected_revision
         ; change
         ; key
         })
  in
  match result with
  | W.Result.Remove receipt -> Ok receipt
  | _ -> Error (P.Error.invalid_request "unexpected instruction removal result")
;;
