open! Core
module P = Agent_protocol
module I = Instruction_content
module Port = Instruction_source_port
module Request = Instruction_source_request
module Admission = Instruction_native_admission
module Command = Instruction_command

type t =
  { state : unit -> (Session_state.t, P.Error.t) result
  ; apply :
      admission:Admission.t
      -> source:Instruction_command_admission.Source.t option
      -> request:Command.t
      -> (Command.Receipt.t, P.Error.t) result
  ; sources : unit -> Port.t option
  ; selection_current : Session_state.t -> bool
  }

let create ~state ~apply ~sources ~selection_current =
  { state; apply; sources; selection_current }
;;

let selection (state : Session_state.t) =
  Option.value state.instruction_selection ~default:Instruction_selection.empty
;;

let checked_state t ~admission ~effect_kind =
  let open Result.Let_syntax in
  let%bind state = t.state () in
  let%map () = Admission.check_now admission state ~effect_kind in
  state
;;

let with_source t ~admission ~effect_kind f =
  let open Result.Let_syntax in
  let%bind _ = checked_state t ~admission ~effect_kind in
  match t.sources () with
  | None ->
    Error
      (P.Error.create
         Method_not_found
         ~message:"instruction sources are not installed"
         ~retryable:false
         ())
  | Some sources ->
    Port.with_use
      sources
      ~actor:(Admission.actor admission)
      ~checkpoint:Eio.Fiber.check
      (fun use ->
         let result = f use in
         match effect_kind with
         | Admission.Effect.Activate -> result
         | Refresh | Read | Status | Deactivate ->
           let%bind _ = checked_state t ~admission ~effect_kind in
           let%bind () = Port.Use.check_now use ~roots:(Port.Use.roots use) in
           result)
;;

let observation_json observed =
  let id = I.Observed.id observed in
  let metadata = I.Observed.metadata observed in
  `Object
    [ "root", `String (I.Root_id.to_string (I.Id.root id))
    ; "member", `String (Source.Portable_path.to_string (I.Id.member id))
    ; "sha256", `String (Chatml_source.Digest.to_hex (I.Observed.digest observed))
    ; "bytes", `Number (Int.to_string (I.Observed.byte_length observed))
    ; ( "name"
      , Option.value_map (I.Metadata.name metadata) ~default:`Null ~f:(fun s -> `String s)
      )
    ; ( "description"
      , Option.value_map (I.Metadata.description metadata) ~default:`Null ~f:(fun s ->
          `String s) )
    ]
;;

let refresh t ~admission ~selection =
  with_source t ~admission ~effect_kind:Refresh (fun use ->
    let open Result.Let_syntax in
    let%map discovery =
      match selection with
      | None -> Ok None
      | Some selection ->
        Port.refresh use ~selection ~checkpoint:Eio.Fiber.check
        |> Result.map ~f:Option.some
    in
    let roots = Port.Use.roots use in
    `Object
      [ "roots", `Array (List.map roots ~f:(fun id -> `String (I.Root_id.to_string id)))
      ; ( "members"
        , `Array
            (Option.value_map discovery ~default:[] ~f:(fun d ->
               List.map (Request.Discovery.observations d) ~f:observation_json)) )
      ; ( "issues"
        , `Array
            (Option.value_map discovery ~default:[] ~f:(fun d ->
               List.map (Request.Discovery.issues d) ~f:(fun (root, member, code) ->
                 `Object
                   [ "root", `String (I.Root_id.to_string root)
                   ; "member", `String (Source.Portable_path.to_string member)
                   ; "code", `String code
                   ]))) )
      ])
;;

let read t ~admission ~request =
  with_source t ~admission ~effect_kind:Read (fun use ->
    let%map.Result captured = Port.capture use ~request ~checkpoint:Eio.Fiber.check in
    let contents = Port.Captured.contents captured in
    `Object
      [ "capture_id", `String (Instruction_capture.id contents)
      ; ( "members"
        , `Array
            (List.map (Instruction_capture.loaded contents) ~f:(fun loaded ->
               `Object
                 [ "source", observation_json (I.Loaded.observed loaded)
                 ; "contents", `String (I.Loaded.contents loaded)
                 ])) )
      ])
;;

let command state ~change ~expected_revision ~key =
  Command.create
    ~session_id:state.Session_state.identity.session_id
    ~generation:state.identity.generation
    ~expected_revision
    ~key
    ~change
;;

let activate t ~admission ~request ~capture_id ~expected_revision ~key =
  let open Result.Let_syntax in
  let%bind _ =
    Chatml_source.Digest.of_hex capture_id
    |> Result.map_error ~f:(fun _ ->
      P.Error.invalid_request "invalid instruction capture id")
  in
  with_source t ~admission ~effect_kind:Activate (fun use ->
    let%bind state = checked_state t ~admission ~effect_kind:Activate in
    let current = selection state in
    let retained =
      List.find (Instruction_selection.captures current) ~f:(fun capture ->
        String.equal (Instruction_capture.id capture) capture_id
        && Request.matches request capture)
    in
    let principal_id =
      (Operator_authorization.principal (Admission.actor admission)).id
    in
    let retry =
      List.exists (Instruction_selection.receipts current) ~f:(fun receipt ->
        P.Id.Principal.equal receipt.principal_id principal_id
        && P.Idempotency_key.equal receipt.key key)
    in
    let%bind capture, source =
      match retained, retry with
      | Some capture, true ->
        Ok (capture, Instruction_command_admission.Source.retry use ~request)
      | None, true ->
        Error
          (P.Error.create
             Idempotency_conflict
             ~message:"instruction key already acknowledges a different request"
             ~retryable:false
             ())
      | (None | Some _), false ->
        let%bind captured = Port.capture use ~request ~checkpoint:Eio.Fiber.check in
        let capture = Port.Captured.contents captured in
        if not (String.equal (Instruction_capture.id capture) capture_id)
        then
          Error
            (P.Error.create
               Conflict
               ~message:"instruction capture changed; read explicitly"
               ~retryable:false
               ())
        else (
          let%map source =
            Instruction_command_admission.Source.fresh use ~request ~captured
          in
          capture, source)
    in
    let change =
      match Request.selection request with
      | Project _ -> Command.Change.Replace_project capture
      | Skills _ -> Replace_skills capture
    in
    let%bind request = command state ~change ~expected_revision ~key in
    t.apply ~admission ~source:(Some source) ~request)
;;

let deactivate t ~admission ~change ~expected_revision ~key =
  let open Result.Let_syntax in
  let%bind state = checked_state t ~admission ~effect_kind:Deactivate in
  let%bind () =
    match change with
    | Command.Change.Remove_project | Remove_skills _ | Clear -> Ok ()
    | Replace_project _ | Replace_skills _ ->
      Error (P.Error.invalid_request "deactivate cannot replace instructions")
  in
  let%bind request = command state ~change ~expected_revision ~key in
  t.apply ~admission ~source:None ~request
;;

let status t ~admission =
  let%map.Result state = checked_state t ~admission ~effect_kind:Status in
  Instruction_selection.summary (selection state)
;;

let remove_skills t ~admission ~members ~expected_revision ~key =
  let open Result.Let_syntax in
  let%bind state = checked_state t ~admission ~effect_kind:Deactivate in
  if List.length members > 64
  then Error (P.Error.invalid_request "instruction member limit")
  else (
    let%bind ids =
      List.map members ~f:(fun expected ->
        List.find
          (List.concat_map
             (Instruction_selection.captures (selection state))
             ~f:Instruction_capture.loaded)
          ~f:(fun loaded ->
            Request.Member.equal
              expected
              (Request.Member.of_observed (I.Loaded.observed loaded)))
        |> Result.of_option
             ~error:
               (P.Error.create
                  Conflict
                  ~message:"selected skill changed"
                  ~retryable:false
                  ())
        |> Result.map ~f:(fun loaded -> I.Observed.id (I.Loaded.observed loaded)))
      |> Result.all
    in
    deactivate t ~admission ~change:(Remove_skills ids) ~expected_revision ~key)
;;

let agent_guidance t ~borrow_actor : Chat_response.Agent_guidance.t =
  { with_guidance =
      (fun f ->
        let open Result.Let_syntax in
        let%bind basis = t.state () in
        let%bind text =
          Instruction_guidance_text.create
            (selection basis)
            ~session_id:basis.identity.session_id
            ~generation:basis.identity.generation
        in
        let with_guard ~source_current ~actor_current =
          let guard =
            Inference_runtime.Submission_guard.create ~refresh:(fun () ->
              (* This refresh may yield; every guard's final check runs only after
             all refreshes. The final comparison and source/owner checks do not. *)
              let latest = t.state () in
              let same =
                match latest with
                | Error _ -> false
                | Ok latest ->
                  Int.equal latest.identity.generation basis.identity.generation
                  && Option.equal
                       phys_equal
                       latest.instruction_selection
                       basis.instruction_selection
              in
              fun () ->
                if
                  (not same)
                  || (not (t.selection_current basis))
                  || (not (actor_current ()))
                  || Result.is_error (source_current ())
                then raise Chat_response.Ctx.Inference_admission_rejected)
          in
          let%map prepared =
            Chat_response.Agent_guidance.Prepared.create
              ~guidance:text.guidance
              ~guard
              ~reserved_input_tokens:text.reserved_input_tokens
          in
          f prepared
        in
        if List.is_empty text.roots
        then with_guard ~source_current:(fun () -> Ok ()) ~actor_current:(fun () -> true)
        else (
          let%bind actor = borrow_actor () in
          let%bind actor =
            Result.of_option
              actor
              ~error:
                (P.Error.create
                   Permission_denied
                   ~message:
                     "active instructions require an admitted agent execution owner"
                   ~retryable:false
                   ())
          in
          match t.sources () with
          | None ->
            Error
              (P.Error.create
                 Method_not_found
                 ~message:"instruction sources are not installed"
                 ~retryable:false
                 ())
          | Some sources ->
            Port.with_use sources ~actor ~checkpoint:Eio.Fiber.check (fun use ->
              let%bind () = Port.Use.check_now use ~roots:text.roots in
              with_guard
                ~source_current:(fun () -> Port.Use.check_now use ~roots:text.roots)
                ~actor_current:(fun () -> Operator_authorization.is_current actor))))
  }
;;
