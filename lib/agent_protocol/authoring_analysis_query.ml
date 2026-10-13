open! Core
module C = Authoring_codec
module A = Authoring_analysis_core
module Source = Authoring_common.Source
module Digest = Chatml_source.Digest

let ( let* ) result f = Result.bind result ~f
let obj fields = `Object fields
let str value = `String value
let field fields name decode = C.required_as fields name decode
let opt fields name decode = C.optional_as fields name decode
let list ~max decode json = C.list ~max_items:max decode json
let enum ~name choices = Json_codec.enum ~name choices

module Query = struct
  include Authoring_analysis_query_facts.Query

  type payload =
    | References_result of Item.reference page found
    | Definitions_result of Item.symbol page found
    | Type_result of Display.t selected
    | Candidates_result of Item.binding page selected
    | Configured_modules_result of Item.configured_module page selected
    | Syntax_result of A.Syntax_site.t page found

  type t =
    { request : Request.t
    ; payload : payload
    }

  let payload_to_json = function
    | References_result value ->
      obj
        [ "kind", str "references"
        ; "result", found_to_json (page_to_json Item.reference_to_json) value
        ]
    | Definitions_result value ->
      obj
        [ "kind", str "definitions"
        ; "result", found_to_json (page_to_json Item.symbol_to_json) value
        ]
    | Type_result value ->
      obj [ "kind", str "type_at"; "result", selected_to_json Display.to_json value ]
    | Candidates_result value ->
      obj
        [ "kind", str "candidates"
        ; "result", selected_to_json (page_to_json Item.binding_to_json) value
        ]
    | Configured_modules_result value ->
      obj
        [ "kind", str "configured_modules"
        ; "result", selected_to_json (page_to_json Item.configured_module_to_json) value
        ]
    | Syntax_result value ->
      obj
        [ "kind", str "syntax_at"
        ; "result", found_to_json (page_to_json A.Syntax_site.to_json) value
        ]
  ;;

  let payload_of_json json =
    let* fields = C.fields ~allowed:[ "kind"; "result" ] json in
    let* kind = field fields "kind" (C.string ~max_bytes:32) in
    match kind with
    | "references" ->
      let* value =
        field
          fields
          "result"
          (found_of_json
             (page_of_json ~max:A.Limits.max_query_items Item.reference_of_json))
      in
      Ok (References_result value)
    | "definitions" ->
      let* value =
        field
          fields
          "result"
          (found_of_json (page_of_json ~max:A.Limits.max_query_items Item.symbol_of_json))
      in
      Ok (Definitions_result value)
    | "type_at" ->
      let* value =
        field
          fields
          "result"
          (selected_of_json ~max:A.Limits.max_query_items Display.of_json)
      in
      Ok (Type_result value)
    | "candidates" ->
      let* value =
        field
          fields
          "result"
          (selected_of_json
             ~max:A.Limits.max_query_items
             (page_of_json ~max:A.Limits.max_query_items Item.binding_of_json))
      in
      Ok (Candidates_result value)
    | "configured_modules" ->
      let* value =
        field
          fields
          "result"
          (selected_of_json
             ~max:A.Limits.max_query_items
             (page_of_json ~max:A.Limits.max_query_items Item.configured_module_of_json))
      in
      Ok (Configured_modules_result value)
    | "syntax_at" ->
      let* value =
        field
          fields
          "result"
          (found_of_json
             (page_of_json ~max:A.Limits.max_query_items A.Syntax_site.of_json))
      in
      Ok (Syntax_result value)
    | _ -> C.invalid "unknown query payload kind"
  ;;

  let page_count ~max_items page = A.Analyze.bounded_list ~max:max_items page.items

  let selected_count ~max_items = function
    | Selected _ | Missing _ -> Ok ()
    | Ambiguous page -> page_count ~max_items page
  ;;

  let check_nested_selected ~max_items scan_item selected =
    let budget = Item.budget () in
    let scan_page page =
      let* () = page_count ~max_items page in
      let rec loop = function
        | [] -> Ok ()
        | item :: rest ->
          let* () = scan_item budget item in
          loop rest
      in
      loop page.items
    in
    match selected with
    | Missing _ -> Ok ()
    | Selected page -> scan_page page
    | Ambiguous outer ->
      let* () = page_count ~max_items outer in
      let rec loop = function
        | [] -> Ok ()
        | page :: rest ->
          let* () = scan_page page in
          loop rest
      in
      loop outer.items
  ;;

  let found_count ~max_items = function
    | Available page -> page_count ~max_items page
    | Unavailable _ -> Ok ()
  ;;

  let preflight_payload request payload =
    let* budget = C.Budget.create ~max_bytes:A.Limits.max_result_bytes in
    (* Root and payload/status wrappers have at most 32 nodes and 8192 escaped
       bytes; the request subtree is charged separately. Each page charges its
       object, items array and
       truncated boolean separately; each list element charges a separator. *)
    let* () = C.Budget.reserve budget ~bytes:8192 ~nodes:32 in
    let charge_json json = C.Budget.charge_json budget ~parent_depth:7 json in
    let rec items encode = function
      | [] -> Ok ()
      | item :: rest ->
        let* () = C.Budget.reserve budget ~bytes:1 ~nodes:0 in
        let* () = charge_json (encode item) in
        items encode rest
    in
    let charge_page encode page =
      let* () = C.Budget.reserve budget ~bytes:64 ~nodes:3 in
      items encode page.items
    in
    let found encode = function
      | Available value -> charge_page encode value
      | Unavailable _ -> Ok ()
    in
    let selected encode = function
      | Selected value -> charge_json (encode value)
      | Missing _ -> Ok ()
      | Ambiguous values -> charge_page encode values
    in
    let* () = C.Budget.charge_json budget ~parent_depth:1 (Request.to_json request) in
    match payload with
    | References_result value -> found Item.reference_to_json value
    | Definitions_result value -> found Item.symbol_to_json value
    | Syntax_result value -> found A.Syntax_site.to_json value
    | Type_result value -> selected Display.to_json value
    | Candidates_result value ->
      (match value with
       | Selected page -> charge_page Item.binding_to_json page
       | Missing _ -> Ok ()
       | Ambiguous outer ->
         let* () = C.Budget.reserve budget ~bytes:64 ~nodes:3 in
         let rec loop = function
           | [] -> Ok ()
           | page :: rest ->
             let* () = C.Budget.reserve budget ~bytes:1 ~nodes:0 in
             let* () = charge_page Item.binding_to_json page in
             loop rest
         in
         loop outer.items)
    | Configured_modules_result value ->
      (match value with
       | Selected page -> charge_page Item.configured_module_to_json page
       | Missing _ -> Ok ()
       | Ambiguous outer ->
         let* () = C.Budget.reserve budget ~bytes:64 ~nodes:3 in
         let rec loop = function
           | [] -> Ok ()
           | page :: rest ->
             let* () = C.Budget.reserve budget ~bytes:1 ~nodes:0 in
             let* () = charge_page Item.configured_module_to_json page in
             loop rest
         in
         loop outer.items)
  ;;

  let to_json t =
    obj [ "request", Request.to_json t.request; "payload", payload_to_json t.payload ]
  ;;

  let create ~request ~payload =
    let max_items = Request.max_items request in
    let* () =
      match Request.kind request, payload with
      | References, References_result value -> found_count ~max_items value
      | Definitions, Definitions_result value -> found_count ~max_items value
      | Syntax_at, Syntax_result value -> found_count ~max_items value
      | Type_at, Type_result value -> selected_count ~max_items value
      | Candidates _, Candidates_result value ->
        check_nested_selected
          ~max_items
          (fun budget binding -> Item.scan_binding budget 0 binding)
          value
      | Configured_modules, Configured_modules_result value ->
        check_nested_selected ~max_items Item.scan_configured value
      | _ -> C.invalid "query result does not match request kind"
    in
    let* () = preflight_payload request payload in
    Ok { request; payload }
  ;;

  let request t = t.request
  let payload t = t.payload
  let sexp_of_t _ = Sexp.Atom "authoring_query_result"

  let of_json json =
    let* () = C.admit_json ~max_bytes:A.Limits.max_result_bytes json in
    let* fields = C.fields ~allowed:[ "request"; "payload" ] json in
    let* request = field fields "request" Request.of_json in
    let* payload = field fields "payload" payload_of_json in
    create ~request ~payload
  ;;
end
