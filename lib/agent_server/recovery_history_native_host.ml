open! Core
module P = Agent_protocol
module A = Agent_session
module Store = Agent_store.Session_store
module Access = A.Session_actor.Recovery_history_access
module Text = A.Recovery_item_text
module Asset = A.Recovery_asset
module Source = A.Recovery_source_reader
module Request = A.Recovery_history_native_request
module Delivery = Inference_runtime.Protected_delivery
module Payload = History_entry.Payload
module Digest = Chatml_source.Digest

type source =
  { with_source :
      'a.
      P.Session_ref.t
      -> authorize:(A.Session_state.t -> (unit, P.Error.t) result)
      -> f:
           (Store.Handle.t
            -> observation:A.Session_actor.Resource_observation.t
            -> current:(unit -> (A.Session_state.t, P.Error.t) result)
            -> check_owner:(unit -> (unit, P.Error.t) result)
            -> ('a, Source.error) result)
      -> ('a, Source.error) result
  ; custody : P.Session_ref.t -> Delivery.custody
  }

type projection =
  { output : Payload.Output.t
  ; current : unit -> bool
  }

let denied () =
  P.Error.create
    Permission_denied
    ~message:"recovery history authority unavailable"
    ~retryable:false
    ()
;;

let unavailable () =
  P.Error.create
    Invalid_state
    ~message:"recovery history selection unavailable"
    ~retryable:false
    ()
;;

let source_error = function
  | Source.Denied -> denied ()
  | Limit | Budget_exhausted ->
    P.Error.create
      Resource_limit
      ~message:"recovery history read limit"
      ~retryable:false
      ()
  | Unavailable | Withdrawn | Expired | Stale | Missing | Corrupt | Unsupported ->
    unavailable ()
;;

let checkpoint now =
  let started = now () in
  fun () ->
    if
      Mtime.Span.compare
        (Mtime.span started (now ()))
        (Mtime.Span.of_uint64_ns 5_000_000_000L)
      >= 0
    then raise Eio.Time.Timeout
;;

let text_output json =
  Payload.Output.Text (Jsonaf.to_string (P.Invocation.outcome_to_json (Complete json)))
;;

let guarded_projection store handle ~check_owner ~current ~selection output =
  let check () =
    Store.retained_lifecycle_available store handle
    && Result.is_ok (check_owner ())
    && Result.ok (current ()) |> Option.exists ~f:selection
  in
  { output; current = check }
;;

let checked_projection load ~custody =
  let latest = ref None in
  let materialize () =
    match load () with
    | Ok projection when projection.current () ->
      latest := Some projection.current;
      Ok projection.output
    | Ok _ | Error _ -> Error Delivery.Unavailable
  in
  let refresh ~scope:_ () = Option.exists !latest ~f:(fun check -> check ()) in
  A.Recovery_history_native_service.{ custody; materialize; refresh }
;;

let output_digest output =
  Digest.of_contents (Jsonaf.to_string (Payload.Output.to_json output)) ~checkpoint:ignore
;;

let reconstruct load ~custody =
  let open Result.Let_syntax in
  let%map first = load () in
  let expected = output_digest first.output in
  let still_current = first.current in
  checked_projection ~custody (fun () ->
    if not (still_current ())
    then Error (unavailable ())
    else (
      let%bind fresh = load () in
      if Digest.equal expected (output_digest fresh.output)
      then Ok fresh
      else Error (unavailable ())))
;;

let prepare_text store ~max_payload_bytes ~now ~current_for_read ~guard request =
  let module Range = P.Recovery_text_range in
  let selection =
    match request.Range.preparation_revision with
    | None -> Text.Canonical request.citation
    | Some source_revision -> Prepared { citation = request.citation; source_revision }
  in
  let read handle ~current ~check_owner =
    let open Result.Let_syntax in
    let checkpoint = checkpoint now in
    Source.with_allowance store ~handle ~checkpoint ~f:(fun allowance ->
      let%bind item =
        Text.resolve
          ~allowance
          store
          ~handle
          ~current:(fun _ _ -> current ())
          ~max_frame_bytes:max_payload_bytes
          ~max_payload_bytes
          selection
          ~view:Ordinary
      in
      let%bind selected =
        List.find (Text.parts item) ~f:(fun candidate ->
          Int.equal candidate.index request.part)
        |> Result.of_option ~error:Source.Unavailable
      in
      let reply =
        P.History_text.read
          selected
          ~start:request.start
          ~stop:request.stop
          ~max_bytes:request.max_bytes
        |> Result.map ~f:(fun slice ->
          Range.Reply.of_slice slice |> Range.Reply.to_json |> text_output)
      in
      checkpoint ();
      let%bind accepted = current () |> Result.map_error ~f:(fun _ -> Source.Denied) in
      let%map selection =
        Text.check_ordinary_release accepted selection ~checkpoint:(fun () -> ())
      in
      Result.map reply ~f:(fun output -> guard handle ~check_owner ~selection output))
  in
  current_for_read read
;;

let image_output ~media_type bytes =
  let uri = "data:" ^ media_type ^ ";base64," ^ Base64.encode_string bytes in
  Payload.Output.Content [ Image { uri; detail = Value "auto" } ]
;;

let prepare_media store ~blobs ~max_payload_bytes ~current_for_read ~guard kind request =
  let module Ref = Agent_store.Recovery_asset_reference in
  let module Wire = P.Recovery_asset_read in
  let module Reply = Wire.Reply in
  let requested_kind, max_bytes =
    match kind with
    | Request.Image -> Reply.Image, 128 * 1024
    | Document -> Document, 1024 * 1024
  in
  let read handle ~current ~check_owner =
    let open Result.Let_syntax in
    let%bind asset =
      Asset.create blobs ~sessions:store ~handle ~max_frame_bytes:max_payload_bytes
    in
    let%bind (reference, bytes), selection =
      Eio.Switch.run (fun sw ->
        Asset.read_ordinary
          asset
          ~sw
          ~current
          request.Wire.citation
          ~part:request.part
          ~max_bytes)
    in
    let blob = Ref.blob reference in
    let selected_kind =
      match blob.kind with
      | Image -> Some Reply.Image
      | File -> Some Document
      | Audio | Binary -> None
    in
    if
      (not (Option.exists selected_kind ~f:(Reply.equal_kind requested_kind)))
      || (not
            (P.Recovery_window.Citation.equal request.citation (Ref.citation reference)))
      || not (Int.equal request.part (Ref.part reference))
    then Error Source.Unavailable
    else (
      let reply =
        let%map verified =
          Reply.of_verified_original
            request
            ~kind:requested_kind
            ~media_type:blob.media_type
            ~sha256:blob.digest
            bytes
        in
        match kind with
        | Image -> image_output ~media_type:verified.media_type bytes
        | Document -> Reply.to_json verified |> text_output
      in
      Ok (Result.map reply ~f:(fun output -> guard handle ~check_owner ~selection output)))
  in
  current_for_read read
;;

let prepare
      store
      ~blobs
      ~max_payload_bytes
      ~now
      source
      ~actor
      ~owner
      ~tool
      admission
      ~binding
      json
  =
  let open Result.Let_syntax in
  let%bind observation = A.Session_actor.observe_resource_state actor in
  let%bind proof =
    Access.capture_native observation admission ~binding ~source:owner ~tool
  in
  let principal = Access.native_principal proof in
  let authorize (state : A.Session_state.t) =
    let%bind captured = Access.native_current proof in
    if
      phys_equal state captured
      && Authorization.session_creator_visible_to
           principal
           ~creator:state.identity.creating_principal
    then Ok ()
    else Error (denied ())
  in
  let retained_current () =
    let%bind state = Access.native_current proof in
    let%map () = authorize state in
    state
  in
  let current_for_read f =
    let%bind projected =
      source.with_source
        owner
        ~authorize
        ~f:(fun handle ~observation:_ ~current ~check_owner ->
          f handle ~current ~check_owner)
      |> Result.map_error ~f:source_error
    in
    let%bind projection = projected in
    if projection.current () then Ok projection else Error (denied ())
  in
  let guard handle = guarded_projection store handle ~current:retained_current in
  let%bind request = Request.decode tool ~source:owner json in
  let custody = source.custody owner in
  match request with
  | Windows page ->
    let%map selected =
      current_for_read (fun handle ~current:_ ~check_owner ->
        Ok
          (let%map accepted = Access.list_native_windows proof page in
           let output = P.Recovery_query.Summary.page_to_json accepted |> text_output in
           let selection _ =
             let equal_row
                   (a : P.Recovery_query.Summary.t)
                   (b : P.Recovery_query.Summary.t)
               =
               P.Recovery_window.Scope.equal a.scope b.scope
               && P.Recovery_query.State.equal a.state b.state
             in
             match Access.list_native_windows proof page with
             | Ok latest ->
               List.equal equal_row accepted.items latest.items
               && Option.equal P.Page.Cursor.equal accepted.next_cursor latest.next_cursor
             | Error _ -> false
           in
           guard handle ~check_owner ~selection output))
    in
    checked_projection ~custody (fun () -> Ok selected)
  | Items request ->
    let%bind query =
      A.Recovery_history_query.of_request ~include_image_parts:true request
    in
    let release = ref None in
    let%bind reply =
      Recovery_history_host.query
        store
        ~now
        ~max_payload_bytes
        ~with_source:source.with_source
        ~access:(Access.model admission ~binding)
        ?cursor:request.page.cursor
        ~on_release:(fun current -> release := Some current)
        query
    in
    let%map current = Result.of_option !release ~error:(denied ()) in
    let output = text_output reply in
    checked_projection ~custody (fun () -> Ok { output; current })
  | Text request ->
    reconstruct ~custody (fun () ->
      prepare_text store ~max_payload_bytes ~now ~current_for_read ~guard request)
  | Media (kind, request) ->
    reconstruct ~custody (fun () ->
      prepare_media store ~blobs ~max_payload_bytes ~current_for_read ~guard kind request)
;;
