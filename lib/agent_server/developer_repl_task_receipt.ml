open! Core
module P = Agent_protocol
module Receipt = Agent_store.Developer_run_store.Command.Receipt
module Repl = Chatml_repl_execution

let error code message = P.Error.create code ~message ~retryable:false ()

let limited_fallback =
  Receipt.Task
    (Receipt.Task.Limited
       { error =
           error
             P.Error.Resource_limit
             "developer REPL Task result exceeds its admitted limit"
       ; diagnostic = None
       })
;;

let project ~limits ~poll result =
  let diagnostic value =
    Option.map (Repl.Task_result.diagnostic value) ~f:(fun diagnostic ->
      let location =
        match
          Repl.Diagnostic.location diagnostic, Repl.Diagnostic.source_bytes diagnostic
        with
        | Some location, Some source_bytes ->
          let span = Chatml_source.Location.span location in
          Some
            { Receipt.Diagnostic.Location.source = Chatml_source.Location.source location
            ; digest = Chatml_source.Location.source_digest location
            ; source_bytes
            ; start_byte = span.left.offset
            ; end_byte = span.right.offset
            }
        | None, None | Some _, None | None, Some _ -> None
      in
      { Receipt.Diagnostic.code = "developer.repl.task.failed"
      ; message = Repl.Diagnostic.message diagnostic
      ; location
      })
  in
  let value =
    match Repl.Task_result.outcome result with
    | Succeeded ->
      let value_preview = Repl.Task_result.value_preview result in
      Receipt.Task (Receipt.Task.Succeeded { value_preview })
    | Failed Runtime ->
      Receipt.Task
        (Receipt.Task.Failed
           { error = error P.Error.Invalid_request "developer REPL Task execution failed"
           ; diagnostic = diagnostic result
           })
    | Failed Budget ->
      Receipt.Task
        (Receipt.Task.Limited
           { error =
               error
                 P.Error.Resource_limit
                 "developer REPL Task execution reached its admitted limit"
           ; diagnostic = diagnostic result
           })
  in
  match Receipt.create value ~limits ~poll with
  | Ok receipt -> Ok receipt
  | Error (Document_schema.Error.Limit_exceeded _) ->
    Receipt.create limited_fallback ~limits ~poll
    |> Result.map_error ~f:(fun _ ->
      P.Error.create
        P.Error.Persistence_error
        ~message:"developer REPL Task result cannot be retained"
        ~retryable:true
        ())
  | Error _ ->
    Error
      (P.Error.create
         P.Error.Persistence_error
         ~message:"developer REPL Task result cannot be retained"
         ~retryable:true
         ())
;;
