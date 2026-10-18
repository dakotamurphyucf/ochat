open! Core
module Error = Protocol_error
module C = Developer_codec
module Common = Developer_common
module A = Developer_approval

let ( let* ) result f = Result.bind result ~f
let get fields name decode = C.required fields name |> Result.bind ~f:decode
let run_id_params run_id = `Object [ "run_id", Id.Developer_run.to_json run_id ]

let run_id_of_json json =
  let* fields = C.fields ~allowed:[ "run_id" ] json in
  get fields "run_id" Id.Developer_run.of_json
;;

module Request = struct
  type t =
    | Capabilities
    | Submit of Common.Submit.t
    | Observe of Common.Lookup.t
    | Await_terminal of Common.Lookup.t
    | Cancel_and_wait of Common.Lookup.t
    | Approval_list of Id.Developer_run.t
    | Approval_respond of A.Response.t
    | Approval_cancel of Id.Developer_run.t * Id.Permission.t
    | Watch of Id.Developer_run.t
    | Unwatch of Id.Developer_run.t

  let method_name = function
    | Capabilities -> "developer.capabilities"
    | Submit _ -> "developer.submit"
    | Observe _ -> "developer.observe"
    | Await_terminal _ -> "developer.await_terminal"
    | Cancel_and_wait _ -> "developer.cancel_and_wait"
    | Approval_list _ -> "developer.approval.list"
    | Approval_respond _ -> "developer.approval.respond"
    | Approval_cancel _ -> "developer.approval.cancel"
    | Watch _ -> "developer.watch"
    | Unwatch _ -> "developer.unwatch"
  ;;

  let params = function
    | Capabilities -> `Object []
    | Submit x -> Common.Submit.to_json x
    | Observe x | Await_terminal x | Cancel_and_wait x -> Common.Lookup.to_json x
    | Approval_list x | Watch x | Unwatch x -> run_id_params x
    | Approval_respond x -> A.Response.to_json x
    | Approval_cancel (run_id, id) ->
      `Object
        [ "run_id", Id.Developer_run.to_json run_id; "id", Id.Permission.to_json id ]
  ;;

  let of_method_and_params ~limits ~method_ ~params =
    let* () = C.validate ~limits:(Common.Limits.request limits) params in
    let* method_ = Common.Method.of_string method_ in
    match method_ with
    | Capabilities ->
      let* () = C.fields ~allowed:[] params |> Result.map ~f:ignore in
      Ok Capabilities
    | Submit -> Result.map (Common.Submit.of_json ~limits params) ~f:(fun x -> Submit x)
    | Observe ->
      Result.map
        (Common.Lookup.of_json ~limits:(Common.Limits.request limits) params)
        ~f:(fun x -> Observe x)
    | Await_terminal ->
      Result.map
        (Common.Lookup.of_json ~limits:(Common.Limits.request limits) params)
        ~f:(fun x -> Await_terminal x)
    | Cancel_and_wait ->
      Result.map
        (Common.Lookup.of_json ~limits:(Common.Limits.request limits) params)
        ~f:(fun x -> Cancel_and_wait x)
    | Approval_list -> Result.map (run_id_of_json params) ~f:(fun x -> Approval_list x)
    | Approval_respond ->
      Result.map
        (A.Response.of_json
           ~max_reason_bytes:(Common.Limits.max_approval_decision_reason_bytes limits)
           params)
        ~f:(fun x -> Approval_respond x)
    | Approval_cancel ->
      let* fields = C.fields ~allowed:[ "run_id"; "id" ] params in
      let* run_id = get fields "run_id" Id.Developer_run.of_json in
      let* id = get fields "id" Id.Permission.of_json in
      Ok (Approval_cancel (run_id, id))
    | Watch -> Result.map (run_id_of_json params) ~f:(fun x -> Watch x)
    | Unwatch -> Result.map (run_id_of_json params) ~f:(fun x -> Unwatch x)
  ;;

  let supported_methods =
    [ "developer.capabilities"
    ; "developer.submit"
    ; "developer.observe"
    ; "developer.await_terminal"
    ; "developer.cancel_and_wait"
    ; "developer.approval.list"
    ; "developer.approval.respond"
    ; "developer.approval.cancel"
    ; "developer.watch"
    ; "developer.unwatch"
    ]
  ;;
end

module Response = struct
  type t =
    | Capabilities of Common.Capabilities.t
    | Submit of Common.View.t
    | Observe of Common.View.t option
    | Await_terminal of Common.View.t
    | Cancel_and_wait of Common.View.t
    | Approval_list of A.Page.t
    | Approval_respond
    | Approval_cancel
    | Watch
    | Unwatch

  let method_name = function
    | Capabilities _ -> "developer.capabilities"
    | Submit _ -> "developer.submit"
    | Observe _ -> "developer.observe"
    | Await_terminal _ -> "developer.await_terminal"
    | Cancel_and_wait _ -> "developer.cancel_and_wait"
    | Approval_list _ -> "developer.approval.list"
    | Approval_respond -> "developer.approval.respond"
    | Approval_cancel -> "developer.approval.cancel"
    | Watch -> "developer.watch"
    | Unwatch -> "developer.unwatch"
  ;;

  let raw_json = function
    | Capabilities x -> Common.Capabilities.to_json x
    | Submit x | Await_terminal x | Cancel_and_wait x -> Common.View.to_json x
    | Observe None -> `Object [ "found", `False ]
    | Observe (Some x) -> `Object [ "found", `True; "view", Common.View.to_json x ]
    | Approval_list x -> A.Page.to_json x
    | Approval_respond | Approval_cancel | Watch | Unwatch -> `Object []
  ;;

  let to_json response ~limits =
    let* () =
      match response with
      | Submit view | Await_terminal view | Cancel_and_wait view | Observe (Some view) ->
        Common.View.create
          ~limits
          ~command_id:(Common.View.command_id view)
          ~owner:(Common.View.owner view)
          ~key:(Common.View.key view)
          ~kind:(Common.View.kind view)
          ~phase:(Common.View.phase view)
          ~request_fingerprint:(Common.View.request_fingerprint view)
          ~capture_fingerprint:(Common.View.capture_fingerprint view)
        |> Result.map ~f:ignore
      | Approval_list page ->
        A.Page.create ~limits (A.Page.pending page) |> Result.map ~f:ignore
      | Capabilities _
      | Observe None
      | Approval_respond | Approval_cancel | Watch | Unwatch -> Ok ()
    in
    let json = raw_json response in
    let* () = C.validate ~limits:(Common.Limits.response limits) json in
    Ok json
  ;;

  let of_method_and_json ~limits ~method_ json =
    let* () = C.validate ~limits:(Common.Limits.response limits) json in
    let* method_ = Common.Method.of_string method_ in
    match method_ with
    | Capabilities ->
      Result.map (Common.Capabilities.of_json json) ~f:(fun x -> Capabilities x)
    | Submit -> Result.map (Common.View.of_json ~limits json) ~f:(fun x -> Submit x)
    | Await_terminal ->
      Result.map (Common.View.of_json ~limits json) ~f:(fun x -> Await_terminal x)
    | Cancel_and_wait ->
      Result.map (Common.View.of_json ~limits json) ~f:(fun x -> Cancel_and_wait x)
    | Observe ->
      let* fields = C.fields ~allowed:[ "found"; "view" ] json in
      let* found = get fields "found" C.boolean in
      if found
      then
        let* view = get fields "view" (Common.View.of_json ~limits) in
        Ok (Observe (Some view))
      else
        let* () = C.fields ~allowed:[ "found" ] json |> Result.map ~f:ignore in
        Ok (Observe None)
    | Approval_list ->
      Result.map (A.Page.of_json ~limits json) ~f:(fun x -> Approval_list x)
    | Approval_respond ->
      let* () = C.fields ~allowed:[] json |> Result.map ~f:ignore in
      Ok Approval_respond
    | Approval_cancel ->
      let* () = C.fields ~allowed:[] json |> Result.map ~f:ignore in
      Ok Approval_cancel
    | Watch ->
      let* () = C.fields ~allowed:[] json |> Result.map ~f:ignore in
      Ok Watch
    | Unwatch ->
      let* () = C.fields ~allowed:[] json |> Result.map ~f:ignore in
      Ok Unwatch
  ;;

  let supported_methods = Request.supported_methods
end

module Change = struct
  type kind =
    | Command_changed of Idempotency_key.t
    | Approval_changed

  type t =
    { run_id : Id.Developer_run.t
    ; kind : kind
    }

  let to_json t =
    `Object
      ([ "run_id", Id.Developer_run.to_json t.run_id ]
       @
       match t.kind with
       | Command_changed key ->
         [ "kind", `String "command_changed"; "key", Idempotency_key.to_json key ]
       | Approval_changed -> [ "kind", `String "approval_changed" ])
  ;;

  let create ~limits ~run_id ~kind =
    let t = { run_id; kind } in
    let json = to_json t in
    let* bytes = C.measure ~limits:(Common.Limits.response limits) json in
    if bytes > Common.Limits.max_event_bytes limits
    then C.limit "developer change exceeds event bound"
    else Ok t
  ;;

  let of_json ~limits json =
    let* () = C.validate ~limits:(Common.Limits.response limits) json in
    let* fields = C.fields ~allowed:[ "run_id"; "kind"; "key" ] json in
    let* run_id = get fields "run_id" Id.Developer_run.of_json in
    let* kind = get fields "kind" (C.string ~max_bytes:32) in
    let* kind =
      match kind with
      | "command_changed" ->
        let* key = get fields "key" Idempotency_key.of_json in
        Ok (Command_changed key)
      | "approval_changed" ->
        let* () = C.fields ~allowed:[ "run_id"; "kind" ] json |> Result.map ~f:ignore in
        Ok Approval_changed
      | _ -> C.invalid "unknown developer change"
    in
    create ~limits ~run_id ~kind
  ;;
end
