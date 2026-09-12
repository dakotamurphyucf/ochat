open Core
open Runner
module Q = Chat_response.Authoring_context
module V = Chat_response.Authoring_validation

let request ?task ?topic_id ?cursor operation =
  let string = Option.value_map ~default:`Null ~f:(fun s -> `String s) in
  `Object
    [ "version", `Number "1"
    ; "operation", `String operation
    ; "task", string task
    ; "query", `Null
    ; "topic_id", string topic_id
    ; "features", `Null
    ; "cursor", string cursor
    ; "max_tokens", `Number "32000"
    ]
;;

let classification (report : V.report) =
  match V.valid report with
  | true -> Valid
  | false ->
    let codes = List.map report.diagnostics ~f:(fun d -> d.diagnostic.code) in
    let has code = List.mem codes code ~equal:String.equal in
    let category =
      match has "chatml.parse_error" with
      | true -> Syntax
      | false ->
        (match
           List.exists codes ~f:(function
             | "delegation.tool_reconfiguration"
             | "delegation.execution_configuration"
             | "delegation.metadata_reconfiguration"
             | "delegation.message_admission"
             | "delegation.history_admission"
             | "delegation.implicit_agent"
             | "authoring.unavailable_target"
             | "invocation.unselected_tool"
             | "capability.not_selected"
             | "capability.stale_reference"
             | "delegation.native_context_unavailable" -> true
             | _ -> false)
         with
         | true -> Capability
         | false -> Semantics)
    in
    Invalid (category, Jsonaf.to_string (V.to_json report))
;;

(* Uses the same bounded, installed query/validation services as native tools.
   The execution oracle is deliberately supplied separately: static success is
   never enough to award runtime success for any family. *)
let create ~env ~context ~host ~capabilities ~scope ~primer ~tool_descriptions ~execute =
  let query request = Q.query context ~host ~capabilities ~scope request in
  let as_message json = { category = Documentation; text = Jsonaf.to_string json } in
  let complete initial_request =
    let rec pages remaining seen current_request =
      match remaining with
      | 0 -> failwith "evaluation initial reference exceeded 100 pages"
      | _ ->
        let page = query current_request in
        (match Jsonaf.member "error" page with
         | Some _ ->
           failwith ("evaluation reference setup failed: " ^ Jsonaf.to_string page)
         | None -> ());
        let item = as_message page in
        (match Jsonaf.member "next_cursor" page, Jsonaf.member "complete" page with
         | Some `Null, Some `True -> [ item ]
         | Some (`String cursor), _ ->
           (match Set.mem seen cursor with
            | true -> failwith "evaluation reference repeated a cursor"
            | false ->
              item
              :: pages (remaining - 1) (Set.add seen cursor) (request ~cursor "continue"))
         | _ -> failwith "evaluation reference setup returned an incomplete page")
    in
    pages 100 String.Set.empty initial_request
  in
  { primer = { category = Primer; text = primer }
  ; tool_descriptions = { category = Tool_descriptions; text = tool_descriptions }
  ; prepare = (fun task -> complete (request ~task:task.family "prepare"))
  ; preload =
      (fun task ->
        List.concat_map task.preload_topics ~f:(fun topic_id ->
          complete (request ~task:task.family ~topic_id "topic")))
  ; retrieve = (fun request -> [ as_message (query request) ])
  ; validate =
      (fun candidate -> V.validate ~env ~host ~capabilities candidate |> classification)
  ; execute
  }
;;
