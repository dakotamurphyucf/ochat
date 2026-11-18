open! Core
module P = Agent_protocol
module CO = Agent_session.Close_observation
module D = Agent_session.Cleanup_diagnostic
module Failure = Agent_session.Callback_cleanup.Failure

type 'a t = ('a, P.Error.t) result CO.t

let return value = CO.create (Returned (Ok value)) ~diagnostics:[] ()

let failure = function
  | CO.Returned (Ok _) -> None
  | Returned (Error error) -> Some (Failure.Protocol error)
  | Raised (exn, trace) -> Some (Failure.Raised (exn, trace))
;;

let capture_observed f =
  let observed =
    match f () with
    | observed -> observed
    | exception exn ->
      CO.create (Raised (exn, Stdlib.Printexc.get_raw_backtrace ())) ~diagnostics:[] ()
  in
  match failure (CO.exit observed), CO.primary observed with
  | None, _ -> observed
  | Some _, Some primary when not (List.is_empty (D.failures primary)) -> observed
  | Some actual, (None | Some _) ->
    let diagnostic = D.create [ actual ] in
    CO.create
      (CO.exit observed)
      ~diagnostics:(CO.diagnostics observed @ [ diagnostic ])
      ~primary:diagnostic
      ()
;;

let capture f =
  capture_observed (fun () -> CO.create (Returned (f ())) ~diagnostics:[] ())
;;

let is_negative observed = Option.is_some (failure (CO.exit observed))

let combine first second =
  let selected = if is_negative first then first else second in
  CO.create
    (CO.exit selected)
    ~diagnostics:(CO.diagnostics first @ CO.diagnostics second)
    ?primary:(if is_negative selected then CO.primary selected else None)
    ()
;;
