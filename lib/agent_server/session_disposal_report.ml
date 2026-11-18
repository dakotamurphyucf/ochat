open! Core
module Failure = Agent_session.Callback_cleanup.Failure

type t = Disposal_report_data.t

let disposed t =
  Disposal_report_data.equal_disposition (Disposal_report_data.disposition t) Disposed
;;

let failures = Disposal_report_data.failures
