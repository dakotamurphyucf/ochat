open! Core
module Failure = Callback_cleanup_data.Failure
module Outcome = Callback_cleanup_data.Outcome

(* Alias the SAME immutable slot-bearing snapshots; no second observation fold. *)
module Completion_snapshot = Callback_cleanup_data.Completion_snapshot
module Completion = Callback_cleanup_data.Completion
module Completion_observation = Callback_cleanup_data.Completion_observation
module Cleanup_snapshot = Callback_cleanup_data.Cleanup_snapshot
module Initialization = Callback_cleanup_data.Initialization
module Operation = Callback_cleanup_data.Operation
