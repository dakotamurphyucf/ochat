(** Thin public facade for the portable authoring wire contracts. DTO modules
    are split by ownership so codecs do not become one monolithic component. *)

module Features = Authoring_common.Features
module Encoding = Authoring_common.Encoding
module Method = Authoring_common.Method
module Failure = Authoring_common.Failure
module Limits = Authoring_common.Limits
module Capabilities = Authoring_common.Capabilities
module Source = Authoring_common.Source
module Position = Authoring_common.Position
module Site = Authoring_common.Site
module Context = Authoring_context.Context
module Document = Authoring_context.Document
module Source_read = Authoring_context.Source_read
module Bundle = Authoring_bundle.Bundle
module Analysis = Authoring_analysis
module Assistance = Authoring_assistance
module Wire = Authoring_wire
