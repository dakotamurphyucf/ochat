open! Core
module J = Json_codec
module Error = Protocol_error

module Mode = struct
  type t =
    | Single_turn
    | Workflow
  [@@deriving compare, equal, sexp]

  let to_json = function
    | Single_turn -> `String "single_turn"
    | Workflow -> `String "workflow"
  ;;

  let of_json =
    J.enum ~name:"run mode" [ "single_turn", Single_turn; "workflow", Workflow ]
  ;;
end

module Terminal = struct
  type t =
    | Completed of Run_result_reference.t option
    | Failed of Error.code option
    | Cancelled
    | Limited
    | Interrupted
  [@@deriving equal]

  let to_json = function
    | Completed result ->
      `Object
        ([ "kind", `String "completed" ]
         @ Projection_codec.optional "result" result Run_result_reference.to_json)
    | Failed code ->
      `Object
        ([ "kind", `String "failed" ]
         @ Projection_codec.optional "code" code (fun code ->
           `String (Error.code_to_string code)))
    | Cancelled -> `Object [ "kind", `String "cancelled" ]
    | Limited -> `Object [ "kind", `String "limited" ]
    | Interrupted -> `Object [ "kind", `String "interrupted" ]
  ;;

  let validate = function
    | Completed (Some result) ->
      let open Result.Let_syntax in
      let%bind () = Run_result_reference.validate result in
      (match result with
       | Run_result_reference.Job reference ->
         if Stored_completion.equal_outcome reference.outcome Succeeded
         then Ok ()
         else
           Error
             (Protocol_error.invalid_request
                "failed job result cannot complete run successfully")
       | Operation _ -> Ok ())
    | Completed None | Failed _ | Cancelled | Limited | Interrupted -> Ok ()
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind f = J.fields json in
    let%bind kind = J.required_as f "kind" J.string in
    match kind with
    | "completed" ->
      let%bind result = J.optional_as f "result" Run_result_reference.of_json in
      let terminal = Completed result in
      let%map () = validate terminal in
      terminal
    | "failed" ->
      let%map code =
        J.optional_as f "code" (fun json ->
          Result.bind (J.string json) ~f:Error.code_of_string)
      in
      Failed code
    | "cancelled" -> Ok Cancelled
    | "limited" -> Ok Limited
    | "interrupted" -> Ok Interrupted
    | _ -> Error (Protocol_error.invalid_request "unsupported run terminal state")
  ;;

  let sexp_of_t t = Jsonaf.sexp_of_t (to_json t)

  let t_of_sexp sexp =
    match of_json (Jsonaf.t_of_sexp sexp) with
    | Ok t -> t
    | Error e -> Sexplib.Conv.of_sexp_error e.message sexp
  ;;
end

module Lifecycle = struct
  type t =
    | Admitted
    | Active
    | Waiting of Run_wake.t
    | Terminal of Terminal.t
  [@@deriving equal]

  let to_json = function
    | Admitted -> `Object [ "kind", `String "admitted" ]
    | Active -> `Object [ "kind", `String "active" ]
    | Waiting wake -> `Object [ "kind", `String "waiting"; "wake", Run_wake.to_json wake ]
    | Terminal terminal ->
      `Object [ "kind", `String "terminal"; "terminal", Terminal.to_json terminal ]
  ;;

  let validate = function
    | Admitted | Active -> Ok ()
    | Waiting wake -> Run_wake.validate wake
    | Terminal terminal -> Terminal.validate terminal
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind f = J.fields json in
    let%bind kind = J.required_as f "kind" J.string in
    match kind with
    | "admitted" -> Ok Admitted
    | "active" -> Ok Active
    | "waiting" ->
      let%map wake = J.required_as f "wake" Run_wake.of_json in
      Waiting wake
    | "terminal" ->
      let%map terminal = J.required_as f "terminal" Terminal.of_json in
      Terminal terminal
    | _ -> Error (Protocol_error.invalid_request "unsupported run lifecycle")
  ;;

  let sexp_of_t t = Jsonaf.sexp_of_t (to_json t)

  let t_of_sexp sexp =
    match of_json (Jsonaf.t_of_sexp sexp) with
    | Ok t -> t
    | Error e -> Sexplib.Conv.of_sexp_error e.message sexp
  ;;
end

type t =
  { id : Id.Run.t
  ; session : Session_ref.t
  ; principal_id : Id.Principal.t
  ; source : Run_source.t
  ; mode : Mode.t
  ; lifecycle : Lifecycle.t
  ; revision : int64
  ; owned_work : Run_work.t list
  ; relinquished_work : Run_work.t list
  ; terminal_work : Run_work.Terminal.t list
  ; created_at : Timestamp.t
  ; updated_at : Timestamp.t
  }
[@@deriving equal]

let validate_invariants t =
  let open Result.Let_syntax in
  let%bind () =
    Run_limits.check_count (List.length t.owned_work + List.length t.relinquished_work)
  in
  let%bind () = Run_limits.check_count (List.length t.terminal_work) in
  let%bind () = Extension_codec.validate_id Id.Run.to_json Id.Run.of_json t.id in
  let%bind () =
    Extension_codec.validate_id
      Id.Server.to_json
      Id.Server.of_json
      (Session_ref.server_id t.session)
  in
  let%bind () =
    Extension_codec.validate_id
      Id.Session.to_json
      Id.Session.of_json
      (Session_ref.session_id t.session)
  in
  let%bind () =
    Extension_codec.validate_id Id.Principal.to_json Id.Principal.of_json t.principal_id
  in
  let%bind () = Run_source.validate t.source in
  let%bind () = Lifecycle.validate t.lifecycle in
  let work = t.owned_work @ t.relinquished_work in
  let%bind () =
    List.fold_result work ~init:() ~f:(fun () work -> Run_work.validate work)
  in
  let%bind () =
    List.fold_result t.terminal_work ~init:() ~f:(fun () evidence ->
      Run_work.Terminal.validate evidence)
  in
  let owned = Set.of_list (module Run_work) t.owned_work in
  let relinquished = Set.of_list (module Run_work) t.relinquished_work in
  let evidence =
    Map.of_alist
      (module Run_work)
      (List.map t.terminal_work ~f:(fun evidence ->
         evidence.Run_work.Terminal.work, evidence))
  in
  let%bind evidence =
    match evidence with
    | `Ok evidence -> Ok evidence
    | `Duplicate_key _ ->
      Error (Protocol_error.invalid_request "duplicate immutable run outcome evidence")
  in
  let%bind () =
    match t.lifecycle with
    | Lifecycle.Waiting wake ->
      if Id.Run.equal wake.run_id t.id && Run_source.equal wake.source t.source
      then Ok ()
      else Error (Protocol_error.invalid_request "run wake belongs to another scope")
    | Admitted | Active | Terminal _ -> Ok ()
  in
  let terminal_allowed =
    match t.lifecycle with
    | Lifecycle.Terminal terminal ->
      Set.for_all owned ~f:(Map.mem evidence)
      &&
        (match terminal with
        | Terminal.Completed _ ->
          Map.for_all evidence ~f:(fun evidence ->
            Run_work.Terminal.equal_outcome evidence.outcome Succeeded)
        | Failed _ | Cancelled | Limited | Interrupted -> true)
    | Admitted | Active | Waiting _ -> true
  in
  let terminal_known =
    Map.for_alli evidence ~f:(fun ~key ~data ->
      Set.mem owned key && Int64.(data.revision <= t.revision))
  in
  let same_generation =
    List.for_all work ~f:(fun work ->
      Int.equal work.Run_work.generation t.source.generation)
  in
  if
    Int64.(t.revision < 0L)
    || Timestamp.compare t.updated_at t.created_at < 0
    || Set.length owned <> List.length t.owned_work
    || Set.length relinquished <> List.length t.relinquished_work
    || (not (Set.is_empty (Set.inter owned relinquished)))
    || not (same_generation && terminal_known && terminal_allowed)
  then
    Error (Protocol_error.invalid_request "invalid run ownership, evidence or lifecycle")
  else Ok ()
;;

let to_json t =
  `Object
    [ "id", Id.Run.to_json t.id
    ; "session", Session_ref.to_json t.session
    ; "principal_id", Id.Principal.to_json t.principal_id
    ; "source", Run_source.to_json t.source
    ; "mode", Mode.to_json t.mode
    ; "lifecycle", Lifecycle.to_json t.lifecycle
    ; "revision", `String (Int64.to_string t.revision)
    ; "owned_work", `Array (List.map t.owned_work ~f:Run_work.to_json)
    ; "relinquished_work", `Array (List.map t.relinquished_work ~f:Run_work.to_json)
    ; "terminal_work", `Array (List.map t.terminal_work ~f:Run_work.Terminal.to_json)
    ; "created_at", Timestamp.to_json t.created_at
    ; "updated_at", Timestamp.to_json t.updated_at
    ]
;;

let validate t =
  let open Result.Let_syntax in
  let%bind () = validate_invariants t in
  Extension_codec.validate_json
    ~max_bytes:Run_limits.max_document_bytes
    ~max_depth:Run_limits.max_depth
    (to_json t)
;;

let create
      ~id
      ~session
      ~principal_id
      ~source
      ~mode
      ~lifecycle
      ~revision
      ~owned_work
      ~relinquished_work
      ~terminal_work
      ~created_at
      ~updated_at
  =
  let t =
    { id
    ; session
    ; principal_id
    ; source
    ; mode
    ; lifecycle
    ; revision
    ; owned_work
    ; relinquished_work
    ; terminal_work
    ; created_at
    ; updated_at
    }
  in
  Result.map (validate t) ~f:(fun () -> t)
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind () =
    Extension_codec.validate_json
      ~max_bytes:Run_limits.max_document_bytes
      ~max_depth:Run_limits.max_depth
      json
  in
  let%bind f = J.fields json in
  let%bind id = J.required_as f "id" Id.Run.of_json in
  let%bind session = J.required_as f "session" Session_ref.of_json in
  let%bind principal_id = J.required_as f "principal_id" Id.Principal.of_json in
  let%bind source = J.required_as f "source" Run_source.of_json in
  let%bind mode = J.required_as f "mode" Mode.of_json in
  let%bind lifecycle = J.required_as f "lifecycle" Lifecycle.of_json in
  let%bind revision = J.required_as f "revision" History.Content_revision.of_json in
  let%bind owned_work = J.required_as f "owned_work" (Run_limits.list Run_work.of_json) in
  let%bind relinquished_work =
    J.required_as f "relinquished_work" (Run_limits.list Run_work.of_json)
  in
  let%bind terminal_work =
    J.required_as f "terminal_work" (Run_limits.list Run_work.Terminal.of_json)
  in
  let%bind created_at = J.required_as f "created_at" Timestamp.of_json in
  let%bind updated_at = J.required_as f "updated_at" Timestamp.of_json in
  create
    ~id
    ~session
    ~principal_id
    ~source
    ~mode
    ~lifecycle
    ~revision:(History.Content_revision.to_int64 revision)
    ~owned_work
    ~relinquished_work
    ~terminal_work
    ~created_at
    ~updated_at
;;

let sexp_of_t t = Jsonaf.sexp_of_t (to_json t)

let t_of_sexp sexp =
  match of_json (Jsonaf.t_of_sexp sexp) with
  | Ok t -> t
  | Error e -> Sexplib.Conv.of_sexp_error e.message sexp
;;

let validate_transition ~previous next =
  let open Result.Let_syntax in
  let%bind () = validate next in
  if equal previous next
  then Ok ()
  else (
    let immutable =
      Id.Run.equal previous.id next.id
      && Session_ref.equal previous.session next.session
      && Id.Principal.equal previous.principal_id next.principal_id
      && Run_source.equal previous.source next.source
      && Mode.equal previous.mode next.mode
      && Timestamp.equal previous.created_at next.created_at
      && Timestamp.compare next.updated_at previous.updated_at >= 0
    in
    (* [validate next] established unique evidence keys above. *)
    let outcomes =
      Map.of_alist_exn
        (module Run_work)
        (List.map next.terminal_work ~f:(fun evidence ->
           evidence.Run_work.Terminal.work, evidence))
    in
    let relinquished = Set.of_list (module Run_work) next.relinquished_work in
    let owned = Set.of_list (module Run_work) next.owned_work in
    let retained =
      List.for_all previous.terminal_work ~f:(fun old ->
        Option.exists (Map.find outcomes old.work) ~f:(Run_work.Terminal.equal old))
      && List.for_all previous.relinquished_work ~f:(Set.mem relinquished)
      && List.for_all previous.owned_work ~f:(fun old ->
        Set.mem owned old || Set.mem relinquished old)
    in
    let open_state =
      match previous.lifecycle with
      | Lifecycle.Admitted | Active | Waiting _ -> true
      | Terminal _ -> false
    in
    if
      immutable
      && retained
      && open_state
      && (not (Int64.equal previous.revision Int64.max_value))
      && Int64.equal next.revision (Int64.succ previous.revision)
    then Ok ()
    else
      Error
        (Protocol_error.create
           Conflict
           ~message:"run transition changes committed identity, outcome or receipt"
           ~retryable:false
           ()))
;;
