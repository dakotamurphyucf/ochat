open! Core
open Authoring_context_service_core
open Authoring_context_activity
open Authoring_context_methods_context
open Authoring_context_methods_source

let handle_core t ~actor ~connection request =
  let open Result.Let_syntax in
  let%bind () = check_domain t in
  let checkpoint () = Eio.Fiber.yield () in
  let request_bytes = String.length (Jsonaf.to_string (W.Request.params request)) in
  if request_bytes > t.protocol_limits.max_request_bytes
  then Error Error.Resource_limit
  else (
    match request with
    | W.Request.Roots -> handle_roots t ~actor ~connection ~checkpoint
    | W.Request.Host_list request ->
      handle_host_list t ~actor ~connection request ~checkpoint
    | W.Request.Host_read request ->
      handle_host_read t ~actor ~connection request ~checkpoint
    | W.Request.Context_open request ->
      handle_context_open t ~actor ~connection request ~checkpoint
    | W.Request.Context_refresh request ->
      handle_context_refresh t ~actor ~connection request ~checkpoint
    | W.Request.Context_close request ->
      handle_context_close t ~actor ~connection request ~checkpoint
    | W.Request.Source_list request ->
      handle_source_list t ~actor ~connection request ~checkpoint
    | W.Request.Source_read request ->
      handle_source_read t ~actor ~connection request ~checkpoint
    | W.Request.Document_version request ->
      handle_document_version t ~actor ~connection request ~checkpoint
    | W.Request.Document_replace request ->
      handle_document_replace t ~actor ~connection request ~checkpoint
    | W.Request.Document_close request ->
      handle_document_close t ~actor ~connection request ~checkpoint
    | W.Request.Capabilities
    | W.Request.Bundle_begin _
    | W.Request.Bundle_chunk _
    | W.Request.Bundle_commit _
    | W.Request.Bundle_status _
    | W.Request.Analyze _
    | W.Request.Prefix _
    | W.Request.Query _
    | W.Request.Lexical _
    | W.Request.Assistance _ -> Error Error.Uninstalled)
;;
