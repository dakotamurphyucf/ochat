open! Core
module Evaluator = Chatml_repl_execution
module Typing = Chatml_repl_typing
module Display = Chatml_type_display

module Statement = struct
  type task_retention =
    | Not_a_task
    | Retained
    | Limit_reached
  [@@deriving equal, sexp_of]

  type t =
    { outcome : Evaluator.Receipt.outcome
    ; value_preview : string option
    ; diagnostic : Evaluator.Diagnostic.t option
    ; expression_type : Display.t option
    ; binding_count : int
    ; task_retention : task_retention
    ; text_limited : bool
    }

  let outcome t = t.outcome
  let value_preview t = t.value_preview
  let diagnostic t = t.diagnostic
  let expression_type t = t.expression_type
  let binding_count t = t.binding_count
  let task_retention t = t.task_retention
  let text_limited t = t.text_limited
end

module Presentation = struct
  type t =
    { statements : Statement.t list
    ; total_statements : int
    ; complete : bool
    ; charged_bytes : int
    }

  let statements t = t.statements
  let total_statements t = t.total_statements
  let complete t = t.complete
  let charged_bytes t = t.charged_bytes
end

module Error = struct
  type t =
    | Invalid_max_bytes
    | Invalid_selection
    | Foreign_snapshot
    | Output_limit
  [@@deriving sexp_of]
end

let display_error = function
  | Display.Error.Foreign_root -> Error.Foreign_snapshot
  | Invalid_max_bytes -> Invalid_max_bytes
;;

let add_text remaining = function
  | None -> None, remaining, false
  | Some text when String.length text <= remaining ->
    Some text, remaining - String.length text, false
  | Some _ -> None, remaining, true
;;

let location_charge location =
  let source = Chatml_source.Location.source location in
  128
  + String.length (Chatml_source.Id.package_namespace source)
  + String.length (Chatml_source.Id.relative_path source)
;;

let diagnostic_charge diagnostic =
  let location_bytes =
    Option.value_map
      (Evaluator.Diagnostic.location diagnostic)
      ~default:0
      ~f:location_charge
  in
  String.length (Evaluator.Diagnostic.message diagnostic) + location_bytes
;;

let add_diagnostic remaining = function
  | None -> None, remaining, false
  | Some diagnostic when diagnostic_charge diagnostic <= remaining ->
    Some diagnostic, remaining - diagnostic_charge diagnostic, false
  | Some _ -> None, remaining, true
;;

let render_type ~session ~remaining receipt ~checkpoint =
  let open Result.Let_syntax in
  match Evaluator.Receipt.expression_type receipt with
  | None -> Ok (None, remaining, false)
  | Some root ->
    let frozen = Evaluator.Receipt.frozen_types receipt in
    let%map rendered =
      Display.Session.render
        session
        ~max_bytes:remaining
        (Typing.Frozen_types.snapshot frozen)
        ~root
        ~kind:Display.Kind.Type
        ~checkpoint
      |> Result.map_error ~f:display_error
    in
    let limited =
      match Display.completeness rendered with
      | Complete -> false
      | Truncated _ -> true
    in
    Some rendered, remaining - String.length (Display.text rendered), limited
;;

let present ~display_limits ~max_bytes receipts ~checkpoint =
  let open Result.Let_syntax in
  if max_bytes < 0
  then Error Error.Invalid_max_bytes
  else (
    let session = Display.Session.create ~limits:display_limits in
    let total_statements = List.length receipts in
    let rec loop remaining limited acc = function
      | [] ->
        Ok
          { Presentation.statements = List.rev acc
          ; total_statements
          ; complete = not limited
          ; charged_bytes = max_bytes - remaining
          }
      | _ when remaining < 64 ->
        Ok
          { Presentation.statements = List.rev acc
          ; total_statements
          ; complete = false
          ; charged_bytes = max_bytes - remaining
          }
      | receipt :: rest ->
        checkpoint ();
        let remaining = remaining - 64 in
        let value_preview, remaining, preview_limited =
          add_text remaining (Evaluator.Receipt.value_preview receipt)
        in
        let diagnostic, remaining, diagnostic_limited =
          add_diagnostic remaining (Evaluator.Receipt.diagnostic receipt)
        in
        let%bind expression_type, remaining, type_limited =
          render_type ~session ~remaining receipt ~checkpoint
        in
        let frozen = Evaluator.Receipt.frozen_types receipt in
        let statement =
          { Statement.outcome = Evaluator.Receipt.outcome receipt
          ; value_preview
          ; diagnostic
          ; expression_type
          ; binding_count = List.length (Typing.Frozen_types.statement_bindings frozen)
          ; task_retention =
              (match Evaluator.Receipt.task_retention receipt with
               | Not_a_task -> Not_a_task
               | Retained _ -> Retained
               | Limit_reached -> Limit_reached)
          ; text_limited = preview_limited || diagnostic_limited || type_limited
          }
        in
        loop remaining (limited || statement.text_limited) (statement :: acc) rest
    in
    loop max_bytes false [] receipts)
;;

module Selector = struct
  type t =
    | Expression
    | Binding_index of int
  [@@deriving equal, sexp_of]
end

module Inspected = struct
  type t =
    { binding_name : string option
    ; rendered : Display.t
    }

  let binding_name t = t.binding_name
  let rendered t = t.rendered
end

let inspect ~display_limits ~max_bytes receipt ~selector ~checkpoint =
  let open Result.Let_syntax in
  if max_bytes < 0
  then Error Error.Invalid_max_bytes
  else (
    let frozen = Evaluator.Receipt.frozen_types receipt in
    let selected =
      match selector with
      | Selector.Expression ->
        Option.map (Evaluator.Receipt.expression_type receipt) ~f:(fun root -> None, root)
      | Binding_index index when index >= 0 ->
        Option.map
          (List.nth (Typing.Frozen_types.statement_bindings frozen) index)
          ~f:(fun (name, root) -> Some name, root)
      | Binding_index _ -> None
    in
    let%bind name, root = Result.of_option selected ~error:Error.Invalid_selection in
    let identity_bytes = 64 + Option.value_map name ~default:0 ~f:String.length in
    let%bind () =
      if identity_bytes > max_bytes then Error Error.Output_limit else Ok ()
    in
    let session = Display.Session.create ~limits:display_limits in
    let%map rendered =
      Display.Session.render
        session
        ~max_bytes:(max_bytes - identity_bytes)
        (Typing.Frozen_types.snapshot frozen)
        ~root
        ~kind:Display.Kind.Type
        ~checkpoint
      |> Result.map_error ~f:display_error
    in
    { Inspected.binding_name = name; rendered })
;;
