open! Core
module P = Agent_protocol.Authoring
module W = P.Wire
module Id = Agent_protocol.Id

type t =
  { connection : Connection.t
  ; initialization : Agent_protocol.Initialize.Response.t
  ; host : Id.Server.t
  ; capabilities : P.Capabilities.t
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

let require_connection t =
  match Connection.initialization t.connection with
  | Some response when phys_equal response t.initialization -> Ok ()
  | Some _ -> incompatible "authoring connection was initialized again"
  | None -> incompatible "authoring connection is no longer initialized"
;;

let require_method t request =
  let method_name = W.Request.method_name request in
  match P.Method.of_string method_name with
  | Error _ -> incompatible "unknown authoring client method"
  | Ok method_ ->
    if P.Capabilities.supports t.capabilities method_
    then Ok ()
    else incompatible ("authoring method is not advertised: " ^ method_name)
;;

let send_raw connection request =
  let method_name = W.Request.method_name request in
  match
    Connection.request_without_history
      connection
      (Agent_protocol.Command.Authoring request)
  with
  | Error _ as failure -> failure
  | Ok (Agent_protocol.Method_result.Authoring response)
    when String.equal (W.Response.method_name response) method_name -> Ok response
  | Ok _ -> incompatible ("unexpected authoring result for " ^ method_name)
;;

let send t request =
  let open Result.Let_syntax in
  let%bind () = require_connection t in
  let%bind () = require_method t request in
  let%bind response = send_raw t.connection request in
  let%bind () = require_connection t in
  Ok response
;;

let create connection =
  let open Result.Let_syntax in
  let%bind initialization =
    match Connection.initialization connection with
    | Some response -> Ok response
    | None -> incompatible "authoring requires a successful initialization"
  in
  let%bind () =
    if List.mem initialization.enabled_features P.Features.core ~equal:String.equal
    then Ok ()
    else incompatible "authoring core feature was not selected"
  in
  let%bind capabilities =
    match send_raw connection W.Request.Capabilities with
    | Ok (W.Response.Capabilities capabilities) -> Ok capabilities
    | Ok _ -> incompatible "unexpected authoring capabilities result"
    | Error _ as failure -> failure
  in
  let%bind () =
    match Connection.initialization connection with
    | Some response when phys_equal response initialization -> Ok ()
    | _ -> incompatible "authoring connection changed during capability negotiation"
  in
  let%bind () =
    if P.Capabilities.supports capabilities P.Method.Capabilities
    then Ok ()
    else incompatible "authoring capabilities omits its own method"
  in
  Ok
    { connection
    ; initialization
    ; host = initialization.server_id
    ; capabilities
    ; contexts = Hashtbl.create (module Id.Authoring_context)
    ; seen_contexts = Hash_set.create (module Id.Authoring_context)
    ; mutex = Eio.Mutex.create ()
    }
;;

let capabilities t = t.capabilities

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
