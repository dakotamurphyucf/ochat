open Core
module Store = Agent_store
module Reader = Store.Retention_reader
module Scan = Store.Blob_reference_scan
module Publisher = Store.Job_result_store.Publisher

let protocol result =
  Result.map_error result ~f:(fun error ->
    Store.Store_error.Corrupt error.Agent_protocol.Error.message)
;;

(* Provider logs can contain several JSON documents or SSE records. Inspect each
   quoted JSON string as well as raw text, preserving escaped IDs without treating
   an incomplete string as evidence of reference absence. *)
let text_strings contents ~feed =
  let rec scan start =
    match String.index_from contents start '"' with
    | None -> Ok ()
    | Some first ->
      let rec finish index escaped =
        match index < String.length contents with
        | false -> Error (Store.Store_error.Corrupt "incomplete retained text string")
        | true ->
          (match contents.[index], escaped with
           | _, true -> finish (index + 1) false
           | '\\', false -> finish (index + 1) true
           | '"', false ->
             let literal = String.sub contents ~pos:first ~len:(index - first + 1) in
             (match Result.try_with (fun () -> Jsonaf.of_string literal) with
              | Ok (`String value) ->
                feed value;
                scan (index + 1)
              | _ -> Error (Store.Store_error.Corrupt "invalid retained text string"))
           | _ -> finish (index + 1) false)
      in
      finish (first + 1) false
  in
  scan 0
;;

let provider_log contents ~feed =
  List.fold_result (String.split_lines contents) ~init:() ~f:(fun () line ->
    let line = String.strip line in
    match line with
    | "" -> Ok ()
    | _
      when List.exists [ "event:"; "id:"; "retry:"; ":" ] ~f:(fun prefix ->
             String.is_prefix line ~prefix) -> text_strings line ~feed
    | _ ->
      let payload =
        Option.value (String.chop_prefix line ~prefix:"data:") ~default:line
        |> String.strip
      in
      (match payload with
       | "[DONE]" -> Ok ()
       | _ ->
         let payload =
           Option.value
             (String.chop_prefix payload ~prefix:"Error parsing JSON from line: ")
             ~default:payload
         in
         (match Result.try_with (fun () -> Jsonaf.of_string payload) with
          | Ok json ->
            feed (Jsonaf.to_string json);
            Ok ()
          | Error _ ->
            Error
              (Store.Store_error.Corrupt "incomplete or invalid retained provider log"))))
;;

let auxiliary_roots ~reader ~handle ~candidates ~max_file_bytes =
  let open Result.Let_syntax in
  let%bind scanner = Scan.create candidates |> protocol in
  let feed text =
    Scan.begin_root scanner;
    Scan.feed scanner text
  in
  let inspect path =
    let%bind contents = Reader.read reader ~path ~max_bytes:max_file_bytes in
    feed contents;
    match Filename.basename path with
    | "metadata.sexp" when String.equal path "metadata.sexp" ->
      let%bind metadata =
        Result.try_with (fun () ->
          Store.Session_store.Metadata.t_of_sexp (Sexp.of_string contents))
        |> Result.map_error ~f:(fun _ ->
          Store.Store_error.Corrupt "invalid retained session metadata")
      in
      let%bind _ =
        Agent_protocol.Session.of_json (Agent_protocol.Session.to_json metadata.session)
        |> protocol
      in
      (match
         Agent_protocol.Id.Session.equal
           metadata.session.id
           (Store.Session_store.Handle.session_id handle)
       with
       | false ->
         Error (Store.Store_error.Corrupt "retained metadata belongs to another session")
       | true ->
         feed (Store.Session_store.Metadata.sexp_of_t metadata |> Sexp.to_string_mach);
         Ok ())
    | "cache.bin" ->
      let%bind texts =
        Chat_response.Cache.retained_text contents
        |> Result.map_error ~f:(fun _ ->
          Store.Store_error.Corrupt "invalid retained agent cache")
      in
      List.fold_result texts ~init:() ~f:(fun () text ->
        let%map () = Reader.charge_bytes reader (String.length text) in
        feed text)
    | name when String.is_suffix name ~suffix:".json" ->
      (match Result.try_with (fun () -> Jsonaf.of_string contents) with
       | Ok json ->
         feed (Jsonaf.to_string json);
         Ok ()
       | Error _ -> Error (Store.Store_error.Corrupt "invalid retained JSON file"))
    | name when String.is_suffix name ~suffix:".sexp" ->
      (match Result.try_with (fun () -> Sexp.of_string contents) with
       | Ok sexp ->
         feed (Sexp.to_string_mach sexp);
         Ok ()
       | Error _ -> Error (Store.Store_error.Corrupt "invalid retained S-expression file"))
    | "raw-openai-response.txt"
    | "raw-openai-streaming-response.txt"
    | "raw-openai-chat-streaming-response.txt" -> provider_log contents ~feed
    | name when String.is_suffix name ~suffix:".txt" -> text_strings contents ~feed
    | _ -> Error (Store.Store_error.Corrupt "unknown retained auxiliary file format")
  in
  let rec tree directory =
    let%bind names = Reader.list reader ~directory in
    List.fold_result names ~init:() ~f:(fun () name ->
      let path = Filename.concat directory name in
      let%bind kind = Reader.kind reader ~path in
      match kind with
      | `Directory -> tree path
      | `File -> inspect path)
  in
  let%bind () =
    List.fold_result
      [ "cache"; "responses"; "exports"; "audit"; "idempotency" ]
      ~init:()
      ~f:(fun () directory -> tree directory)
  in
  let%map () = inspect "metadata.sexp" in
  Scan.references scanner
;;

let collect
      ~runtime
      ~actor
      ~publisher
      ~handle
      ~journal
      ~persistence
      ~durable_events
      ~idempotency_store
      ~limits
      ~max_frame_bytes
      ~max_events
  =
  Runtime_owner.with_unloaded runtime (fun () ->
    Agent_session.Session_actor.with_quiescent_state actor ~f:(fun state ->
      Publisher.collect
        publisher
        ~jobs:state.jobs
        ~generation:state.identity.generation
        ~limits
        ~with_roots:(fun ~reader ~candidates ~f ->
          let open Result.Let_syntax in
          let%bind history =
            Agent_session.Retained_history.scan
              ~reader
              ~handle
              ~state
              ~journal_current:(Store.Journal.current_segment journal)
              ~transaction_hash:
                (Agent_session.Session_persistence.transaction_hash persistence)
              ~max_file_bytes:limits.max_file_bytes
              ~max_frame_bytes
              ~candidates
          in
          let%bind replay =
            Agent_session.Durable_event_log.retained_references
              durable_events
              ~session_id:state.identity.session_id
              ~candidates
              ~max_events
              ~max_bytes:limits.max_bytes
            |> protocol
          in
          let%bind auxiliary =
            auxiliary_roots
              ~reader
              ~handle
              ~candidates
              ~max_file_bytes:limits.max_file_bytes
          in
          Store.Idempotency_store.with_retained_references
            idempotency_store
            ~candidates
            ~max_records:limits.max_entries
            ~max_bytes:limits.max_bytes
            ~f:(fun cached -> f (history @ replay @ auxiliary @ cached))
          |> Result.map ~f:Option.join)
      |> Result.map_error ~f:Store.Store_error.to_protocol_error)
    |> Result.map ~f:Option.join)
  |> Result.map ~f:Option.join
;;
