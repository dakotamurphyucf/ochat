open! Core
module Input = Authoring_assistance_input
module Error = Input.Error
module Limits = Input.Limits
module Captured_input = Input.Captured_input
module Path_candidate = Input.Path_candidate
module Selected_tool = Input.Selected_tool
module Facts = Input.Facts
module Wire = Agent_protocol.Authoring_assistance.Assistance

let produce = Authoring_assistance_candidates.produce
