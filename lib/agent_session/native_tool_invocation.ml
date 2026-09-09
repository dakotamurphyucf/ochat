open Core
module I = Agent_protocol.Invocation
module C = Chat_response.Tool_capability
module S = Chatmd_shell_spec.Tool_schema

type executor =
  invocation:I.t
  -> (dispatched:I.t -> (I.outcome, Agent_protocol.Error.t) result)
  -> (I.t, Agent_protocol.Error.t) result

type scope =
  | Unbound
  | Active of I.t
  | Expired

type borrowed =
  { invocation : I.t
  ; active : bool Atomic.t
  ; execute : executor
  }

let scope_key = Eio.Fiber.create_key ()

let current_scope () =
  match Eio.Fiber.get scope_key with
  | None -> Unbound
  | Some scope ->
    (match Atomic.get scope.active with
     | true -> Active scope.invocation
     | false -> Expired)
;;

let borrow () =
  match Eio.Fiber.get scope_key with
  | Some scope when Atomic.get scope.active -> Ok scope
  | None | Some _ ->
    Error (Agent_protocol.Error.invalid_request "native invocation scope is not active")
;;

let borrowed_invocation scope = scope.invocation

let with_scope ~execute invocation f =
  match Eio.Fiber.get scope_key with
  | Some scope when Atomic.get scope.active && I.equal scope.invocation invocation ->
    (* Borrowed admission already installed the exact child scope with the actor
       executor. Replacing it with the caller's direct-child-only adapter would
       incorrectly constrain this child's own descendants to its parent. *)
    f ()
  | None | Some _ ->
    let active = Atomic.make true in
    Exn.protect
      ~finally:(fun () -> Atomic.set active false)
      ~f:(fun () -> Eio.Fiber.with_binding scope_key { invocation; active; execute } f)
;;

let execute_borrowed scope ~invocation f =
  let check_active () =
    match Atomic.get scope.active with
    | true -> Ok ()
    | false ->
      Error (Agent_protocol.Error.invalid_request "borrowed invocation scope expired")
  in
  let open Result.Let_syntax in
  let%bind () = check_active () in
  let parent = scope.invocation.context in
  let child = invocation.I.context in
  let deadline_within_parent =
    match parent.deadline, child.deadline with
    | None, _ -> true
    | Some parent, Some child -> Agent_protocol.Timestamp.compare child parent <= 0
    | Some _, None -> false
  in
  let%bind () =
    match child.origin, child.parent_invocation with
    | Script, Some parent_id
      when Agent_protocol.Id.Invocation.equal parent_id parent.id
           && Agent_protocol.Id.Session.equal child.session_id parent.session_id
           && Int.equal child.generation parent.generation
           && Option.is_none child.provider_call_id
           && Option.is_none child.call_entry_id
           && Option.is_none child.parent_job
           && Option.is_none invocation.parent_event
           && deadline_within_parent -> Ok ()
    | _ ->
      Error
        (Agent_protocol.Error.invalid_request
           "borrowed execution requires a script child of the current invocation")
  in
  scope.execute ~invocation (fun ~dispatched ->
    (* Actor admission can yield while the native callback returns. A retained
       executor must not start effects after its lending scope has expired. *)
    let%bind () = check_active () in
    with_scope ~execute:scope.execute dispatched (fun () -> f ~dispatched))
;;

let fail code message = I.Fail { code; message; retryable = false; details = `Null }

let checked failure f =
  match f () with
  | Ok value -> Ok value
  | Error _ -> Error failure
  | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
  | exception _ -> Error failure
;;

let run_scoped
      ~execute
      ~registry
      ~(reference : C.reference)
      ~invocation
      ~is_halted
      ~authorize
      ~prepare_output
  =
  execute ~invocation (fun ~dispatched ->
    let check_halted () =
      let failure = fail "invocation.session_ended" "The session has ended." in
      match checked failure (fun () -> Ok (is_halted ())) with
      | Ok false -> Ok ()
      | Ok true | Error _ -> Error failure
    in
    let execute_native () =
      let open Result.Let_syntax in
      let%bind () = check_halted () in
      let resolve () =
        checked
          (fail
             "invocation.stale_binding"
             "The selected tool capability is no longer valid.")
          (fun () ->
             let selected = registry () in
             let%bind binding =
               C.resolve selected ~id:reference.id ~fingerprint:reference.fingerprint
             in
             let c = dispatched.I.context in
             let kind_matches =
               match dispatched.routing, (C.implementation binding).info.type_ with
               | None, ("function" | "custom") -> true
               | Some { kind = I.Function; _ }, "function"
               | Some { kind = I.Custom; _ }, "custom" -> true
               | _ -> false
             in
             if
               kind_matches
               && String.equal c.tool_name reference.name
               && String.equal c.implementation_revision reference.implementation_revision
               && String.equal c.capability_fingerprint (C.fingerprint selected)
             then Ok binding
             else
               Error
                 C.
                   { code = "capability.stale_context"
                   ; message = "invocation context does not match selected capability"
                   })
      in
      let%bind binding = resolve () in
      let%bind () =
        checked
          (fail
             "invocation.invalid_input"
             "The tool arguments do not satisfy its input schema.")
          (fun () ->
             let%bind schema = S.compile reference.input_schema in
             S.validate schema dispatched.context.input)
      in
      let%bind () =
        checked
          (fail "invocation.permission_denied" "Tool execution was not authorized.")
          (fun () -> authorize dispatched binding)
      in
      (* An approval wait may have replaced or narrowed the selected registry.
         Never dispatch the binding captured before that wait. *)
      let%bind () = check_halted () in
      let%bind binding = resolve () in
      let implementation = C.implementation binding in
      let%bind payload =
        match implementation.info.type_, dispatched.context.input with
        | "function", input -> Ok (Jsonaf.to_string input)
        | "custom", `String input -> Ok input
        | "custom", _ ->
          Error (fail "invocation.invalid_input" "Custom tools require string input.")
        | _ ->
          Error
            (fail
               "invocation.unsupported_kind"
               "The registered tool kind is unsupported.")
      in
      let%bind output =
        checked (fail "invocation.handler_failed" "Tool execution failed.") (fun () ->
          Ok
            (implementation.run_with_progress
               ~invocation:Ochat_function.Invocation.silent
               payload))
      in
      let%bind value =
        checked
          (fail
             "invocation.disclosure_rejected"
             "The tool result could not be disclosed.")
          (fun () -> prepare_output output)
      in
      let outcome = I.Complete value in
      let%map () =
        checked
          (fail "invocation.invalid_output" "The tool returned an invalid result.")
          (fun () -> I.validate_outcome outcome)
      in
      outcome
    in
    with_scope ~execute dispatched (fun () ->
      match
        Option.bind dispatched.routing ~f:(fun routing ->
          Stream_invocation.rejection_outcome routing.preparation)
      with
      | Some outcome -> Ok outcome
      | None ->
        Ok
          (match execute_native () with
           | Ok outcome | Error outcome -> outcome)))
;;

let run ~capabilities =
  run_scoped ~execute:capabilities.Operation_worker.Capabilities.with_invocation
;;
