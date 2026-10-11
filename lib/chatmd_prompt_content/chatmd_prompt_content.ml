open Core
module Input = Chatmd_parser_input
module Parsed = Prompt.Chat_markdown.Parsed
module Occurrence = Prompt.Chat_markdown.Document_occurrence
module Projection = Prompt.Chat_markdown.Document_projection
module Storage = Agent_store.Prompt_program_bundle
module Limits = Content_budget.Limits
module Error = Content_budget.Error
module Accounting = Content_budget.Accounting
module Rendered = Content_rendered
module B = Content_budget
module Document = Content_document

type t =
  { document : Document.t
  ; accounting : Accounting.t
  }

let to_storage t = Document.to_storage t.document
let accounting t = t.accounting

let finish budget result =
  Result.map result ~f:(fun document -> { document; accounting = B.accounting budget })
;;

let capture ~parsed_contexts ~rendered ~limits ~checkpoint =
  let open Result.Let_syntax in
  let budget = B.create limits ~checkpoint in
  let result =
    let%bind inventory = Content_inventory.capture ~parsed_contexts ~rendered ~budget in
    Document.create inventory ~budget
  in
  finish budget result
;;

let of_storage storage ~inputs ~limits ~checkpoint =
  let budget = B.create limits ~checkpoint in
  finish budget (Document.of_storage storage ~inputs ~budget)
;;

module Materialized = struct
  type t =
    { projection : Projection.t
    ; accounting : Accounting.t
    }

  let projection t = t.projection
  let accounting t = t.accounting
end

let materialize t ~parsed ~limits ~checkpoint =
  let budget = B.create limits ~checkpoint in
  Content_inventory.materialize (Document.inventory t.document) ~parsed ~budget
  |> Result.map ~f:(fun projection ->
    { Materialized.projection; accounting = B.accounting budget })
;;

let adopt t ~previous ~limits ~checkpoint =
  let budget = B.create limits ~checkpoint in
  finish budget (Document.adopt t.document ~previous:previous.document ~budget)
;;
