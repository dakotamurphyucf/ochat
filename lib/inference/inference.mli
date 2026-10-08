(** Pure inference contracts shared by adapters, runtime owners and accounting.
    These values perform no network access, credential lookup, history commit or
    tool execution. Request data is private; safe observations have a separate
    explicit representation. *)
module Request = Request

module Selection = Selection
module Event = Event
module Observation = Observation
