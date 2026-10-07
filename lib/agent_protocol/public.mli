(** Explicit public read boundary. Private snapshot/history/event/result types
    also serve durable storage and must not be changed to redact a client view. *)
module History = Public_history

module Snapshot = Public_snapshot
module Durable = Public_durable_event
module Result = Public_result
