open! Core
module J = Json_codec

module Request = struct
  type t =
    { query : Search_query.t
    ; hit : Search_hit.t
    }

  let create ~query ~hit =
    if
      not
        (Id.Server.equal
           (Search_query.server_id query)
           (Session_ref.server_id (Search_hit.session hit)))
    then Error (Protocol_error.invalid_request "navigation hit names another host")
    else Ok { query; hit }
  ;;

  let query t = t.query
  let hit t = t.hit

  let to_json t =
    `Object [ "query", Search_query.to_json t.query; "hit", Search_hit.to_json t.hit ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = J.fields json in
    let%bind query = J.required_as fields "query" Search_query.of_json in
    let%bind hit = J.required_as fields "hit" Search_hit.of_json in
    create ~query ~hit
  ;;

  let sexp_of_t t = Jsonaf.sexp_of_t (to_json t)

  let t_of_sexp sexp =
    match of_json (Jsonaf.t_of_sexp sexp) with
    | Ok t -> t
    | Error error -> Sexplib.Conv.of_sexp_error error.Protocol_error.message sexp
  ;;
end

module Entry = struct
  type t =
    { history_id : History.Id.t
    ; content_revision : History.Content_revision.t
    ; text : string
    ; truncated : bool
    }

  let create ~history_id ~content_revision ~text ~truncated =
    if String.length text > 2048 || not (Stdlib.String.is_valid_utf_8 text)
    then Error (Protocol_error.invalid_request "invalid navigation context text")
    else Ok { history_id; content_revision; text; truncated }
  ;;

  let history_id t = t.history_id
  let content_revision t = t.content_revision
  let text t = t.text
  let truncated t = t.truncated

  let to_json t =
    `Object
      [ "history_id", History.Id.to_json t.history_id
      ; "content_revision", History.Content_revision.to_json t.content_revision
      ; "text", `String t.text
      ; ("truncated", if t.truncated then `True else `False)
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = J.fields json in
    let%bind history_id = J.required_as fields "history_id" History.Id.of_json in
    let%bind content_revision =
      J.required_as fields "content_revision" History.Content_revision.of_json
    in
    let%bind text = J.required_as fields "text" J.string in
    let%bind truncated = J.required_as fields "truncated" J.bool in
    create ~history_id ~content_revision ~text ~truncated
  ;;
end

module Response = struct
  type t =
    | Current of
        { hit : Search_hit.t
        ; context : Entry.t list
        }
    | Changed of History.Content_revision.t
    | Unavailable

  let to_json = function
    | Current { hit; context } ->
      `Object
        [ "status", `String "current"
        ; "hit", Search_hit.to_json hit
        ; "context", `Array (List.map context ~f:Entry.to_json)
        ]
    | Changed revision ->
      `Object
        [ "status", `String "changed"
        ; "content_revision", History.Content_revision.to_json revision
        ]
    | Unavailable -> `Object [ "status", `String "unavailable" ]
  ;;

  let current ~hit ~context =
    if
      List.length context > 5
      || List.contains_dup context ~compare:(fun a b ->
        History.Id.compare (Entry.history_id a) (Entry.history_id b))
    then Error (Protocol_error.invalid_request "invalid navigation context bounds or IDs")
    else (
      match
        List.find context ~f:(fun entry ->
          History.Id.equal (Entry.history_id entry) (Search_hit.history_id hit))
      with
      | Some entry
        when History.Content_revision.equal
               (Entry.content_revision entry)
               (Search_hit.content_revision hit) ->
        let t = Current { hit; context } in
        Result.map
          (J.validate_limits ~max_depth:24 ~max_bytes:65536 (to_json t))
          ~f:(fun () -> t)
      | Some _ | None ->
        Error
          (Protocol_error.invalid_request
             "navigation context does not contain the current target"))
  ;;

  let changed revision = Changed revision
  let unavailable = Unavailable

  let of_json json =
    let open Result.Let_syntax in
    let%bind () = J.validate_limits ~max_depth:24 ~max_bytes:65536 json in
    let%bind fields = J.fields json in
    let%bind status = J.required_as fields "status" J.string in
    match status with
    | "unavailable" -> Ok unavailable
    | "changed" ->
      J.required_as fields "content_revision" History.Content_revision.of_json
      |> Result.map ~f:changed
    | "current" ->
      let%bind hit = J.required_as fields "hit" Search_hit.of_json in
      let%bind context = J.required_as fields "context" (J.list Entry.of_json) in
      current ~hit ~context
    | _ -> Error (Protocol_error.invalid_request "invalid navigation status")
  ;;

  let sexp_of_t t = Jsonaf.sexp_of_t (to_json t)

  let t_of_sexp sexp =
    match of_json (Jsonaf.t_of_sexp sexp) with
    | Ok t -> t
    | Error error -> Sexplib.Conv.of_sexp_error error.Protocol_error.message sexp
  ;;
end
