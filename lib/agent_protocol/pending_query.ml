open! Core
module Error = Protocol_error
module J = Json_codec

module Request = struct
  type t =
    { session_id : Id.Session.t
    ; page : Page.Request.t
    }
  [@@deriving sexp]

  let create ~session_id ~page =
    let%map.Result page =
      Page.Request.create ~limit:page.Page.Request.limit ?cursor:page.cursor ()
    in
    { session_id; page }
  ;;

  let to_json t =
    `Object
      (("session_id", Id.Session.to_json t.session_id) :: Page.Request.to_fields t.page)
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = J.fields json in
    let%bind session_id = J.required_as fields "session_id" Id.Session.of_json in
    let%bind page = Page.Request.of_fields fields in
    create ~session_id ~page
  ;;

  let unchecked_of_sexp = t_of_sexp

  let t_of_sexp sexp =
    match of_json (to_json (unchecked_of_sexp sexp)) with
    | Ok value -> value
    | Error error -> Sexplib.Conv.of_sexp_error error.message sexp
  ;;
end

module Lookup_request = struct
  type t =
    { session_id : Id.Session.t
    ; history_id : History.Id.t
    }
  [@@deriving sexp]

  let to_json t =
    `Object
      [ "session_id", Id.Session.to_json t.session_id
      ; "history_id", History.Id.to_json t.history_id
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = J.fields json in
    let%bind session_id = J.required_as fields "session_id" Id.Session.of_json in
    let%map history_id = J.required_as fields "history_id" History.Id.of_json in
    { session_id; history_id }
  ;;

  let unchecked_of_sexp = t_of_sexp

  let t_of_sexp sexp =
    match of_json (to_json (unchecked_of_sexp sexp)) with
    | Ok value -> value
    | Error error -> Sexplib.Conv.of_sexp_error error.message sexp
  ;;
end

module Retirement_reason = struct
  type t =
    | Source_reset
    | Source_replaced
    | Canonical_history_retired
  [@@deriving equal, sexp]

  let to_json = function
    | Source_reset -> `String "source_reset"
    | Source_replaced -> `String "source_replaced"
    | Canonical_history_retired -> `String "canonical_history_retired"
  ;;

  let of_json =
    J.enum
      ~name:"pending retirement reason"
      [ "source_reset", Source_reset
      ; "source_replaced", Source_replaced
      ; "canonical_history_retired", Canonical_history_retired
      ]
  ;;
end

module Item = struct
  type t =
    { history : Public_history.t
    ; generation : int
    ; binding : Pending_input.Binding.t
    }

  let create ~history ~generation ~(binding : Pending_input.Binding.t) =
    if generation < 0
    then Error (Error.invalid_request "pending projection generation must be nonnegative")
    else (
      match binding with
      | After_root { generation = bound; _ } when not (Int.equal generation bound) ->
        Error (Error.invalid_request "pending projection barrier generation differs")
      | Safe_boundary | Await_idle | After_root _ -> Ok { history; generation; binding })
  ;;

  let to_json t =
    `Object
      [ "history", Public_history.to_json t.history
      ; "generation", `Number (Int.to_string t.generation)
      ; "binding", Pending_input.Binding.to_json t.binding
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind () = Projection_codec.validate json in
    let%bind fields = J.fields json in
    let%bind history = J.required_as fields "history" Public_history.of_json in
    let%bind generation =
      J.required_as fields "generation" (J.bounded_int ~min:0 ~max:Int.max_value)
    in
    let%bind binding = J.required_as fields "binding" Pending_input.Binding.of_json in
    create ~history ~generation ~binding
  ;;

  let sexp_of_t t = Jsonaf.sexp_of_t (to_json t)

  let t_of_sexp sexp =
    match of_json (Jsonaf.t_of_sexp sexp) with
    | Ok value -> value
    | Error error -> Sexplib.Conv.of_sexp_error error.message sexp
  ;;
end

module Outcome = struct
  type t =
    | Pending of Item.t
    | Adopted of
        { history_id : History.Id.t
        ; admitted_content_revision : History.Content_revision.t
        ; current : Public_history.t option
        }
    | Cancelled of History.Id.t
    | Retired of History.Id.t * Retirement_reason.t
    | Unavailable of History.Id.t

  let pending input = Pending input

  let adopted ~history_id ~admitted_content_revision ~(current : Public_history.t option) =
    match current with
    | Some entry when not (History.Id.equal history_id entry.id) ->
      Error (Error.invalid_request "pending adoption current occurrence identity differs")
    | Some entry
      when History.Content_revision.compare
             entry.content_revision
             admitted_content_revision
           < 0 ->
      Error
        (Error.invalid_request
           "pending adoption current content revision precedes admission")
    | Some _ | None -> Ok (Adopted { history_id; admitted_content_revision; current })
  ;;

  let cancelled id = Cancelled id
  let retired id ~reason = Retired (id, reason)
  let unavailable id = Unavailable id

  let history_id = function
    | Pending input -> input.Item.history.id
    | Adopted { history_id; _ } -> history_id
    | Cancelled id | Retired (id, _) | Unavailable id -> id
  ;;

  let to_json t =
    let detail =
      match t with
      | Pending input -> [ "kind", `String "pending"; "input", Item.to_json input ]
      | Adopted { admitted_content_revision; current; _ } ->
        [ "kind", `String "adopted"
        ; ( "admitted_content_revision"
          , History.Content_revision.to_json admitted_content_revision )
        ; "current", Option.value_map current ~default:`Null ~f:Public_history.to_json
        ]
      | Cancelled _ -> [ "kind", `String "cancelled" ]
      | Retired (_, reason) ->
        [ "kind", `String "retired"; "reason", Retirement_reason.to_json reason ]
      | Unavailable _ -> [ "kind", `String "unavailable" ]
    in
    `Object (("history_id", History.Id.to_json (history_id t)) :: detail)
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind () = Projection_codec.validate json in
    let%bind fields = J.fields json in
    let%bind id = J.required_as fields "history_id" History.Id.of_json in
    let%bind kind = J.required_as fields "kind" J.string in
    match kind with
    | "pending" ->
      let%bind input = J.required_as fields "input" Item.of_json in
      if History.Id.equal id input.Item.history.id
      then Ok (pending input)
      else Error (Error.invalid_request "pending lookup identity differs from input")
    | "adopted" ->
      let%bind admitted_content_revision =
        J.required_as fields "admitted_content_revision" History.Content_revision.of_json
      in
      let%bind current =
        J.required_as fields "current" (function
          | `Null -> Ok None
          | json -> Public_history.of_json json |> Result.map ~f:Option.some)
      in
      adopted ~history_id:id ~admitted_content_revision ~current
    | "cancelled" -> Ok (cancelled id)
    | "retired" ->
      let%map reason = J.required_as fields "reason" Retirement_reason.of_json in
      retired id ~reason
    | "unavailable" -> Ok (unavailable id)
    | _ -> Error (Error.invalid_request "unknown pending lookup outcome")
  ;;

  let sexp_of_t t = Jsonaf.sexp_of_t (to_json t)

  let t_of_sexp sexp =
    match of_json (Jsonaf.t_of_sexp sexp) with
    | Ok value -> value
    | Error error -> Sexplib.Conv.of_sexp_error error.message sexp
  ;;
end

module View = struct
  type t =
    { pending_revision : Pending_input.Revision.t
    ; page : Item.t Page.t
    }

  let to_json t =
    `Object
      [ "pending_revision", Pending_input.Revision.to_json t.pending_revision
      ; "page", Page.to_json Item.to_json t.page
      ]
  ;;

  let create ~pending_revision ~page =
    let ids = Hash_set.create (module History.Id) in
    let open Result.Let_syntax in
    let%bind () =
      List.fold_result page.Page.items ~init:() ~f:(fun () input ->
        let id = input.Item.history.id in
        if Hash_set.mem ids id
        then Error (Error.invalid_request "duplicate pending page occurrence")
        else (
          Hash_set.add ids id;
          Ok ()))
    in
    let value = { pending_revision; page } in
    let%map () = Projection_codec.validate (to_json value) in
    value
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind () = Projection_codec.validate json in
    let%bind fields = J.fields json in
    let%bind pending_revision =
      J.required_as fields "pending_revision" Pending_input.Revision.of_json
    in
    let%bind page = J.required_as fields "page" (Page.of_json Item.of_json) in
    create ~pending_revision ~page
  ;;

  let sexp_of_t t = Jsonaf.sexp_of_t (to_json t)

  let t_of_sexp sexp =
    match of_json (Jsonaf.t_of_sexp sexp) with
    | Ok value -> value
    | Error error -> Sexplib.Conv.of_sexp_error error.message sexp
  ;;
end
