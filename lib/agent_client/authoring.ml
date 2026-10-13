open! Core
module P = Agent_protocol.Authoring
module W = P.Wire
module Id = Agent_protocol.Id

type t =
  { endpoint : Authoring_connection.t
  ; host : Id.Server.t
  ; contexts : (Id.Authoring_context.t, P.Context.t) Hashtbl.t
  ; seen_contexts : Id.Authoring_context.t Hash_set.t
  ; mutex : Eio.Mutex.t
  }

let with_request_lock t f =
  (* A cancelled request keeps the local CAS table usable for explicit
     recovery. No result is published after cancellation. *)
  match
    Eio.Mutex.use_rw ~protect:false t.mutex (fun () ->
      match f () with
      | result -> Ok result
      | exception (Eio.Cancel.Cancelled _ as exn) ->
        Hashtbl.clear t.contexts;
        Error (exn, Stdlib.Printexc.get_raw_backtrace ()))
  with
  | Ok result -> result
  | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
;;

let with_context_mutation t context_id f =
  with_request_lock t (fun () ->
    match f () with
    | Ok _ as success -> success
    | Error _ as failure ->
      (* An interrupted or malformed reply may follow a committed mutation.
         Reauthorization/open establishes a fresh view before reuse. *)
      Hashtbl.remove t.contexts context_id;
      failure)
;;

let incompatible message =
  Error (Agent_protocol.Error.create Incompatible_protocol ~message ~retryable:false ())
;;

let stale message =
  Error (Agent_protocol.Error.create Conflict ~message ~retryable:false ())
;;

let require_connection t = Authoring_connection.require_current t.endpoint
let send t request = Authoring_connection.send t.endpoint request

let create connection =
  let open Result.Let_syntax in
  let%bind endpoint = Authoring_connection.create connection ~feature:P.Features.core in
  Ok
    { endpoint
    ; host = Authoring_connection.host endpoint
    ; contexts = Hashtbl.create (module Id.Authoring_context)
    ; seen_contexts = Hash_set.create (module Id.Authoring_context)
    ; mutex = Eio.Mutex.create ()
    }
;;

let capabilities t = Authoring_connection.capabilities t.endpoint

let is_current_context t (expected : P.Context.Expected.t) =
  Eio.Mutex.use_ro t.mutex (fun () ->
    match require_connection t, Hashtbl.find t.contexts expected.context_id with
    | Ok (), Some current ->
      P.Context.Expected.equal (P.Context.Expected.of_view current) expected
    | _ -> false)
;;

let require_current t (expected : P.Context.Expected.t) =
  match Hashtbl.find t.contexts expected.context_id with
  | Some current
    when P.Context.Expected.equal (P.Context.Expected.of_view current) expected -> Ok ()
  | _ -> stale "authoring context generation is not current on this connection"
;;

let same_origin_locator old next =
  match old, next with
  | ( P.Context.Host_document
        { root = old_root; source = old_source; default_environment = old_environment }
    , P.Context.Host_document
        { root = next_root; source = next_source; default_environment = next_environment }
    ) ->
    Id.Authoring_root.equal old_root next_root
    && String.equal
         (Chatml_source.Id.relative_path (P.Source.id old_source))
         (Chatml_source.Id.relative_path (P.Source.id next_source))
    && Option.equal String.equal old_environment next_environment
  | _ -> P.Context.equal_origin old next
;;

let matches_open_request request origin =
  match request, origin with
  | P.Context.Open_request.Host_project requested, P.Context.Host_project admitted ->
    Id.Authoring_root.equal requested.root admitted.root
    && List.equal
         Id.Authoring_root.equal
         requested.dependency_roots
         admitted.dependency_roots
    && String.equal requested.project_member admitted.project_member
    && P.Context.Selection.equal requested.selection admitted.selection
  | P.Context.Open_request.Uploaded_project requested, P.Context.Uploaded_project admitted
    -> Id.Authoring_bundle.equal requested admitted
  | P.Context.Open_request.Host_document requested, P.Context.Host_document admitted ->
    Id.Authoring_root.equal requested.root admitted.root
    && String.equal
         requested.member
         (Chatml_source.Id.relative_path (P.Source.id admitted.source))
    && Option.equal
         String.equal
         requested.default_environment
         admitted.default_environment
  | ( P.Context.Open_request.Supplied_document requested
    , P.Context.Supplied_document admitted ) ->
    let source = admitted.source in
    String.equal requested.member (Chatml_source.Id.relative_path (P.Source.id source))
    && Option.equal
         String.equal
         requested.default_environment
         admitted.default_environment
    && P.Source.byte_length source = String.length requested.contents
    && Chatml_source.Digest.equal
         (P.Source.digest source)
         (Chatml_source.Digest.of_contents requested.contents ~checkpoint:(fun () ->
            Eio.Fiber.check ()))
  | _ -> false
;;

let accept_context t view =
  if not (Id.Server.equal (P.Context.host view) t.host)
  then incompatible "authoring result belongs to another host"
  else if
    Option.exists
      (Hashtbl.find t.contexts (P.Context.id view))
      ~f:(fun old ->
        not (same_origin_locator (P.Context.origin old) (P.Context.origin view)))
  then incompatible "authoring context origin changed"
  else (
    Hashtbl.set t.contexts ~key:(P.Context.id view) ~data:view;
    Ok view)
;;

let same_next_context (expected : P.Context.Expected.t) view =
  Id.Authoring_context.equal expected.context_id (P.Context.id view)
  && Id.Server.equal expected.host (P.Context.host view)
  && expected.generation < P.Limits.maximum_wire_integer
  && P.Context.generation view = expected.generation + 1
;;

let roots t =
  with_request_lock t (fun () ->
    match send t W.Request.Roots with
    | Ok (W.Response.Roots result) -> Ok result
    | Ok _ -> incompatible "unexpected authoring roots result"
    | Error _ as failure -> failure)
;;

let list_host_members t (request : P.Source_read.Host_list_request.t) =
  with_request_lock t (fun () ->
    match send t (W.Request.Host_list request) with
    | Ok (W.Response.Host_list result)
      when Id.Authoring_root.equal result.root request.root -> Ok result
    | Ok _ -> incompatible "authoring host listing changed root"
    | Error _ as failure -> failure)
;;

let read_host_member t (request : P.Source_read.Host_read_request.t) =
  with_request_lock t (fun () ->
    match send t (W.Request.Host_read request) with
    | Ok (W.Response.Host_read result)
      when Id.Authoring_root.equal
             (P.Source_read.Host_read_result.root result)
             request.root
           && String.equal (P.Source_read.Host_read_result.member result) request.member
      -> Ok result
    | Ok _ -> incompatible "authoring host read changed root or member"
    | Error _ as failure -> failure)
;;

let open_context t (request : P.Context.Open_request.t) =
  with_request_lock t (fun () ->
    match send t (W.Request.Context_open request) with
    | Ok (W.Response.Context_open view)
      when P.Context.generation view = 0
           && matches_open_request request (P.Context.origin view) ->
      if Hash_set.mem t.seen_contexts (P.Context.id view)
      then incompatible "authoring context ID was reused"
      else if Hash_set.length t.seen_contexts >= 4096
      then incompatible "authoring client context history is full; reconnect required"
      else (
        match accept_context t view with
        | Ok _ as admitted ->
          Hash_set.add t.seen_contexts (P.Context.id view);
          admitted
        | Error _ as failure -> failure)
    | Ok _ -> incompatible "invalid initial authoring context result"
    | Error _ as failure -> failure)
;;

let refresh_context t (request : P.Context.Refresh_request.t) =
  with_context_mutation t request.expected.context_id (fun () ->
    let open Result.Let_syntax in
    let%bind () = require_current t request.expected in
    match send t (W.Request.Context_refresh request) with
    | Ok (W.Response.Context_refresh view) when same_next_context request.expected view ->
      accept_context t view
    | Ok _ -> incompatible "authoring refresh returned a foreign generation"
    | Error _ as failure -> failure)
;;

let close_context t (request : P.Context.Close_request.t) =
  with_context_mutation t request.expected.context_id (fun () ->
    let open Result.Let_syntax in
    let%bind () = require_current t request.expected in
    match send t (W.Request.Context_close request) with
    | Ok (W.Response.Context_close result)
      when Id.Authoring_context.equal result.id request.expected.context_id
           && Id.Server.equal result.host t.host
           && result.generation = request.expected.generation + 1 ->
      Hashtbl.remove t.contexts result.id;
      Ok result
    | Ok _ -> incompatible "authoring close returned a foreign generation"
    | Error _ as failure -> failure)
;;

let list_sources t (request : P.Source_read.List_request.t) =
  with_request_lock t (fun () ->
    let open Result.Let_syntax in
    let%bind () = require_current t request.context in
    match send t (W.Request.Source_list request) with
    | Ok (W.Response.Source_list result)
      when P.Context.Expected.equal
             (P.Source_read.List_result.context result)
             request.context -> Ok result
    | Ok _ -> incompatible "authoring source list belongs to another context"
    | Error _ as failure -> failure)
;;

let read_source t (request : P.Source_read.Read_request.t) =
  with_request_lock t (fun () ->
    let open Result.Let_syntax in
    let%bind () = require_current t request.context in
    match send t (W.Request.Source_read request) with
    | Ok (W.Response.Source_read result)
      when P.Context.Expected.equal
             (P.Source_read.Read_result.context result)
             request.context
           && Chatml_source.Id.equal
                (P.Source.id (P.Source_read.Read_result.source result))
                request.source_id
           &&
           match Hashtbl.find t.contexts request.context.context_id with
           | Some view ->
             (match P.Context.basis view with
              | P.Context.Project _ -> true
              | P.Context.Document { source; _ } ->
                P.Source.equal source (P.Source_read.Read_result.source result))
           | None -> false -> Ok result
    | Ok _ -> incompatible "authoring source read changed context or source"
    | Error _ as failure -> failure)
;;

let document_version t (request : P.Document.Version_request.t) =
  with_request_lock t (fun () ->
    let open Result.Let_syntax in
    let%bind () = require_current t request.context in
    match send t (W.Request.Document_version request) with
    | Ok (W.Response.Document_version version)
      when P.Context.Expected.equal (P.Document.Version.context version) request.context
           && Chatml_source.Id.equal
                (P.Source.id (P.Document.Version.source version))
                request.source_id
           &&
           match Hashtbl.find t.contexts request.context.context_id with
           | Some view ->
             (match P.Context.basis view with
              | P.Context.Project _ -> true
              | P.Context.Document { source; _ } ->
                P.Source.equal source (P.Document.Version.source version))
           | None -> false -> Ok version
    | Ok _ -> incompatible "authoring document version changed context or source"
    | Error _ as failure -> failure)
;;

let accept_document_result t expected result ~overlay_open ~last_editor_revision =
  let view = result.P.Document.Result.context in
  let version = result.version in
  if
    (not (same_next_context (P.Document.Version.context expected) view))
    || (not
          (P.Context.Expected.equal
             (P.Document.Version.context version)
             (P.Context.Expected.of_view view)))
    || (not
          (Chatml_source.Id.equal
             (P.Source.id (P.Document.Version.source version))
             (P.Source.id (P.Document.Version.source expected))))
    || (not (Bool.equal (P.Document.Version.overlay_open version) overlay_open))
    || not
         (Option.equal
            Int.equal
            (P.Document.Version.last_editor_revision version)
            last_editor_revision)
  then incompatible "authoring document mutation changed source or generation"
  else Result.map (accept_context t view) ~f:(fun _ -> result)
;;

let replace_document t (request : P.Document.Replace_request.t) =
  with_context_mutation
    t
    (P.Document.Version.context request.expected).context_id
    (fun () ->
       let open Result.Let_syntax in
       let%bind () = require_current t (P.Document.Version.context request.expected) in
       match send t (W.Request.Document_replace request) with
       | Ok (W.Response.Document_replace result) ->
         let source = P.Document.Version.source result.version in
         let submitted_digest =
           Chatml_source.Digest.of_contents request.contents ~checkpoint:(fun () ->
             Eio.Fiber.check ())
         in
         if
           P.Source.byte_length source <> String.length request.contents
           || not (Chatml_source.Digest.equal (P.Source.digest source) submitted_digest)
         then incompatible "authoring replacement source does not match submitted bytes"
         else
           accept_document_result
             t
             request.expected
             result
             ~overlay_open:true
             ~last_editor_revision:(Some request.next_editor_revision)
       | Ok _ -> incompatible "unexpected authoring document replacement"
       | Error _ as failure -> failure)
;;

let close_document t (request : P.Document.Close_request.t) =
  with_context_mutation
    t
    (P.Document.Version.context request.expected).context_id
    (fun () ->
       let open Result.Let_syntax in
       let%bind () = require_current t (P.Document.Version.context request.expected) in
       match send t (W.Request.Document_close request) with
       | Ok (W.Response.Document_close result) ->
         accept_document_result
           t
           request.expected
           result
           ~overlay_open:false
           ~last_editor_revision:
             (P.Document.Version.last_editor_revision request.expected)
       | Ok _ -> incompatible "unexpected authoring document close"
       | Error _ as failure -> failure)
;;

let current_target t expected target =
  match Hashtbl.find t.contexts expected.P.Context.Expected.context_id with
  | None -> false
  | Some view ->
    (match target, P.Context.basis view with
     | P.Analysis.Target.Project_input, P.Context.Project _ -> true
     | P.Analysis.Target.Script_input stamp, P.Context.Document { source; _ } ->
       P.Source.equal source (P.Analysis.Target.document_source stamp)
     | _ -> false)
;;

let selected_target_matches selector resolved =
  P.Analysis.Target.equal_selector selector (P.Analysis.Target.selector resolved)
;;

let require_semantic t =
  Authoring_connection.require_feature t.endpoint P.Features.semantic
;;

let analyze t (request : P.Analysis.Analyze.Request.t) =
  with_request_lock t (fun () ->
    let open Result.Let_syntax in
    let expected = P.Analysis.Analyze.Request.context request in
    let selector = P.Analysis.Analyze.Request.target request in
    let%bind () = require_semantic t in
    let%bind () = require_current t expected in
    match send t (W.Request.Analyze request) with
    | Ok (W.Response.Analyze result) ->
      let valid =
        match result with
        | P.Analysis.Analyze.Complete { capture; _ }
        | P.Analysis.Analyze.Failed { capture; _ } ->
          P.Context.Expected.equal (P.Analysis.Capture.context capture) expected
          && selected_target_matches selector (P.Analysis.Capture.target capture)
          && current_target t expected (P.Analysis.Capture.target capture)
        | P.Analysis.Analyze.Unavailable { context; target; _ } ->
          P.Context.Expected.equal context expected
          && P.Analysis.Target.equal_selector target selector
      in
      if valid then Ok result else incompatible "analysis response changed its target"
    | Ok _ -> incompatible "unexpected authoring analysis result"
    | Error _ as failure -> failure)
;;

let analyze_prefix t (request : P.Analysis.Prefix.Request.t) =
  with_request_lock t (fun () ->
    let open Result.Let_syntax in
    let expected = P.Analysis.Prefix.Request.context request in
    let selector = P.Analysis.Prefix.Request.target request in
    let source = P.Analysis.Prefix.Request.source request in
    let cursor_byte = P.Analysis.Prefix.Request.cursor_byte request in
    let%bind () = require_semantic t in
    let%bind () = require_current t expected in
    match send t (W.Request.Prefix request) with
    | Ok (W.Response.Prefix result) ->
      let valid =
        match result with
        | P.Analysis.Prefix.Provisional prefix ->
          let capture = P.Analysis.Prefix.capture prefix in
          P.Context.Expected.equal (P.Analysis.Capture.context capture) expected
          && selected_target_matches selector (P.Analysis.Capture.target capture)
          && current_target t expected (P.Analysis.Capture.target capture)
          && P.Source.equal (P.Analysis.Prefix.full_source prefix) source
          && Int.equal (P.Analysis.Prefix.cursor_byte prefix) cursor_byte
        | P.Analysis.Prefix.Unavailable result ->
          P.Context.Expected.equal result.context expected
          && P.Analysis.Target.equal_selector result.target selector
          && P.Source.equal result.source source
          && Int.equal result.cursor_byte cursor_byte
      in
      if valid then Ok result else incompatible "prefix response changed its target"
    | Ok _ -> incompatible "unexpected authoring prefix result"
    | Error _ as failure -> failure)
;;

let query_context = function
  | P.Analysis.Query.Certified { capture; _ } | P.Analysis.Query.Syntax_only capture ->
    P.Analysis.Capture.context capture
  | P.Analysis.Query.Provisional prefix ->
    P.Analysis.Capture.context (P.Analysis.Prefix.capture prefix)
;;

let query_target = function
  | P.Analysis.Query.Certified { capture; _ } | P.Analysis.Query.Syntax_only capture ->
    P.Analysis.Capture.target capture
  | P.Analysis.Query.Provisional prefix ->
    P.Analysis.Capture.target (P.Analysis.Prefix.capture prefix)
;;

let query_semantics t (request : P.Analysis.Query.Request.t) =
  with_request_lock t (fun () ->
    let open Result.Let_syntax in
    let evidence = P.Analysis.Query.Request.evidence request in
    let expected = query_context evidence in
    let%bind () = require_semantic t in
    let%bind () = require_current t expected in
    if not (current_target t expected (query_target evidence))
    then incompatible "query evidence is not current for this context"
    else (
      match send t (W.Request.Query request) with
      | Ok (W.Response.Query result)
        when P.Analysis.Query.Request.equal (P.Analysis.Query.request result) request ->
        Ok result
      | Ok _ -> incompatible "query response changed its request"
      | Error _ as failure -> failure))
;;

let lexical t (request : P.Assistance.Lexical.Request.t) =
  with_request_lock t (fun () ->
    let open Result.Let_syntax in
    let%bind () = Authoring_connection.require_feature t.endpoint P.Features.lexical in
    let%bind () = require_current t (P.Assistance.Lexical.Request.context request) in
    match send t (W.Request.Lexical request) with
    | Ok (W.Response.Lexical result)
      when P.Assistance.Lexical.Request.equal
             (P.Assistance.Lexical.Result.request result)
             request -> Ok result
    | Ok _ -> incompatible "lexical response changed request source or view"
    | Error _ as failure -> failure)
;;

let assist t (request : P.Assistance.Assistance.Request.t) =
  with_request_lock t (fun () ->
    let open Result.Let_syntax in
    let%bind () = Authoring_connection.require_feature t.endpoint P.Features.assistance in
    let%bind () = require_current t (P.Assistance.Assistance.Request.context request) in
    match send t (W.Request.Assistance request) with
    | Ok (W.Response.Assistance result)
      when P.Assistance.Assistance.Request.equal
             (P.Assistance.Assistance.Result.request result)
             request -> Ok result
    | Ok _ -> incompatible "assistance response changed request or source"
    | Error _ as failure -> failure)
;;
