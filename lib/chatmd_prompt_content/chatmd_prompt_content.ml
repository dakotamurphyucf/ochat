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

let capture_with_originals ~parsed_contexts ~rendered ~limits ~checkpoint =
  let open Result.Let_syntax in
  let budget = B.create limits ~checkpoint in
  let%bind inventory, originals =
    Content_inventory.capture ~parsed_contexts ~rendered ~budget
  in
  let%map content = finish budget (Document.create inventory ~budget) in
  content, originals
;;

let capture ~parsed_contexts ~rendered ~limits ~checkpoint =
  capture_with_originals ~parsed_contexts ~rendered ~limits ~checkpoint
  |> Result.map ~f:fst
;;

let of_storage storage ~inputs ~limits ~checkpoint =
  let budget = B.create limits ~checkpoint in
  finish budget (Document.of_storage storage ~inputs ~budget)
;;

type content = t

module Materialized = struct
  type t =
    { projection : Projection.t
    ; content : content
    ; accounting : Accounting.t
    }

  let projection t = t.projection
  let accounting t = t.accounting
  let belongs_to t ~content = phys_equal t.content content
end

let materialize t ~parsed ~limits ~checkpoint =
  let budget = B.create limits ~checkpoint in
  Content_inventory.materialize (Document.inventory t.document) ~parsed ~budget
  |> Result.map ~f:(fun projection ->
    { Materialized.projection; content = t; accounting = B.accounting budget })
;;

let original_key t ~parsed ~occurrence ~limits ~checkpoint =
  let budget = B.create limits ~checkpoint in
  Content_inventory.original_key
    (Document.inventory t.document)
    ~parsed
    ~occurrence
    ~budget
;;

let adopt t ~previous ~limits ~checkpoint =
  let budget = B.create limits ~checkpoint in
  finish budget (Document.adopt t.document ~previous:previous.document ~budget)
;;
