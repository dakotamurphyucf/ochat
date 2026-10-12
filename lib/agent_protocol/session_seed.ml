open! Core
module Error = Protocol_error
module D = Document_schema

let invalid message = Error (Error.invalid_request message)

let document_error error =
  Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error))
;;

let wire_limits =
  D.Limits.create
    ~max_bytes:(16 * 1024 * 1024)
    ~max_depth:256
    ~max_fields:1_000_000
    ~max_nodes:2_000_000
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let default_entries = D.Limits.max_nodes wire_limits
let default_work = 32 * default_entries

let sexp_result sexp = function
  | Ok value -> value
  | Error error -> Sexplib.Conv.of_sexp_error error.Error.message sexp
;;

let with_work ~max_work_steps ~checkpoint f =
  let module Invocation = struct
    exception Work_limit
  end
  in
  if max_work_steps <= 0
  then invalid "seed work bound must be positive"
  else (
    let remaining = ref max_work_steps in
    let poll () =
      if !remaining = 0 then raise Invocation.Work_limit;
      decr remaining;
      checkpoint ()
    in
    try f poll with
    | Invocation.Work_limit -> invalid "seed exceeds its work bound")
;;

let admit_json json ~limits ~poll =
  D.Encoding.render
    ~limits
    ~byte_limit_context:"seed JSON exceeds its byte bound"
    ~checkpoint:poll
    ~f:(fun encoder -> D.Encoding.raw encoder json)
  |> Result.map ~f:ignore
  |> Result.map_error ~f:document_error
;;

let fields json allowed =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  if
    List.for_all (Json_codec.to_alist fields) ~f:(fun (name, _) ->
      List.mem allowed name ~equal:String.equal)
  then Ok fields
  else invalid "unknown session seed field"
;;

let sequence_to_json value = `String (Int64.to_string value)

let sequence_of_json = function
  | `String value when String.length value <= 19 ->
    (match Int64.of_string_opt value with
     | Some value_int
       when Int64.(value_int >= 0L) && String.equal value (Int64.to_string value_int) ->
       Ok value_int
     | Some _ | None ->
       invalid "seed high water must be a nonnegative canonical decimal string")
  | _ -> invalid "seed high water must be a nonnegative canonical decimal string"
;;

module Transcript = struct
  type t =
    { entries : History.entry list
    ; next_history_sequence : int64
    }
  [@@deriving sexp]

  let entries t = t.entries
  let next_history_sequence t = t.next_history_sequence

  let encode encoder entries next_history_sequence =
    D.Encoding.object_
      encoder
      [ ( "entries"
        , fun encoder ->
            D.Encoding.array encoder entries ~f:(fun encoder entry ->
              D.Encoding.raw encoder (History.entry_to_json entry)) )
      ; ( "next_history_sequence"
        , fun encoder -> D.Encoding.string encoder (Int64.to_string next_history_sequence)
        )
      ]
  ;;

  let create
        entries
        ~next_history_sequence
        ~limits
        ~max_entries
        ~max_work_steps
        ~checkpoint
    =
    with_work ~max_work_steps ~checkpoint (fun poll ->
      let open Result.Let_syntax in
      let%bind () =
        if max_entries <= 0 || Int64.(next_history_sequence < 0L)
        then invalid "invalid seed entry bound or high water"
        else Ok ()
      in
      let%bind _ =
        List.fold_result entries ~init:0 ~f:(fun count (entry : History.entry) ->
          poll ();
          if count >= max_entries
          then invalid "seed exceeds its entry bound"
          else if
            String.length (History_entry.Id.namespace entry.id)
            > D.Limits.max_bytes limits
          then invalid "seed history ID exceeds its byte bound"
          else if
            entry.redacted
            || (not (History.equal_provenance entry.provenance Canonical))
            || not
                 (History.Content_revision.equal
                    entry.content_revision
                    History.Content_revision.zero)
          then
            invalid "seed entries must be nonredacted Canonical entries at revision zero"
          else Ok (count + 1))
      in
      let%bind _ =
        D.Encoding.render
          ~limits
          ~byte_limit_context:"seed transcript exceeds its byte bound"
          ~checkpoint:poll
          ~f:(fun encoder -> encode encoder entries next_history_sequence)
        |> Result.map_error ~f:document_error
      in
      let ids = Hash_set.create (module History_entry.Id) in
      let%bind native =
        List.fold_result entries ~init:[] ~f:(fun reversed (entry : History.entry) ->
          poll ();
          if
            entry.redacted
            || (not (History.equal_provenance entry.provenance Canonical))
            || not
                 (History.Content_revision.equal
                    entry.content_revision
                    History.Content_revision.zero)
          then
            invalid "seed entries must be nonredacted Canonical entries at revision zero"
          else if Hash_set.mem ids entry.id
          then invalid "seed contains duplicate history IDs"
          else (
            let%bind _ = History.Id.of_string (History.Id.to_string entry.id) in
            let%bind payload =
              History_entry.Payload.of_json entry.payload
              |> Result.map_error ~f:Error.invalid_request
            in
            poll ();
            let role, kind =
              History.classification (History_entry.Payload.semantic payload)
            in
            if
              not
                (History.equal_role role entry.role && History.equal_kind kind entry.kind)
            then invalid "seed classification differs from its neutral payload"
            else (
              Hash_set.add ids entry.id;
              Ok (History_entry.create_with_id ~id:entry.id payload :: reversed))))
        |> Result.map ~f:List.rev
      in
      let%map () =
        History_entry.validate_relations ~checkpoint:poll native
        |> Result.map_error ~f:Error.invalid_request
      in
      { entries; next_history_sequence })
  ;;

  let to_json t =
    `Object
      [ "entries", `Array (List.map t.entries ~f:History.entry_to_json)
      ; "next_history_sequence", sequence_to_json t.next_history_sequence
      ]
  ;;

  let of_json json =
    with_work ~max_work_steps:default_work ~checkpoint:Fn.ignore (fun poll ->
      let open Result.Let_syntax in
      let%bind () = admit_json json ~limits:wire_limits ~poll in
      let%bind f = fields json [ "entries"; "next_history_sequence" ] in
      let%bind entries =
        Json_codec.required_as f "entries" (Json_codec.list History.entry_of_json)
      in
      let%bind next_history_sequence =
        Json_codec.required_as f "next_history_sequence" sequence_of_json
      in
      create
        entries
        ~next_history_sequence
        ~limits:wire_limits
        ~max_entries:default_entries
        ~max_work_steps:default_work
        ~checkpoint:poll)
  ;;

  let t_of_sexp sexp =
    let { entries; next_history_sequence } = t_of_sexp sexp in
    sexp_result
      sexp
      (create
         entries
         ~next_history_sequence
         ~limits:wire_limits
         ~max_entries:default_entries
         ~max_work_steps:default_work
         ~checkpoint:Fn.ignore)
  ;;
end

module Code_selection = struct
  type t =
    | Catalog of
        { definition_id : Id.Prompt_definition.t
        ; expected_revision : Id.Prompt_revision.t
        }
    | Retained of Session_lifecycle.Expected.t
  [@@deriving sexp]

  let to_json = function
    | Catalog { definition_id; expected_revision } ->
      `Object
        [ "kind", `String "catalog"
        ; "definition_id", Id.Prompt_definition.to_json definition_id
        ; "expected_revision", Id.Prompt_revision.to_json expected_revision
        ]
    | Retained expected ->
      `Object
        [ "kind", `String "retained"
        ; "expected", Session_lifecycle.Expected.to_json expected
        ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind f =
      fields json [ "kind"; "definition_id"; "expected_revision"; "expected" ]
    in
    let%bind kind = Json_codec.required_as f "kind" Json_codec.string in
    match kind with
    | "catalog" ->
      let%bind f = fields json [ "kind"; "definition_id"; "expected_revision" ] in
      let%bind definition_id =
        Json_codec.required_as f "definition_id" Id.Prompt_definition.of_json
      in
      let%map expected_revision =
        Json_codec.required_as f "expected_revision" Id.Prompt_revision.of_json
      in
      Catalog { definition_id; expected_revision }
    | "retained" ->
      let%bind f = fields json [ "kind"; "expected" ] in
      Json_codec.required_as f "expected" Session_lifecycle.Expected.of_json
      |> Result.map ~f:(fun expected -> Retained expected)
    | _ -> invalid "unknown seed code selection"
  ;;

  let t_of_sexp sexp = sexp_result sexp (of_json (to_json (t_of_sexp sexp)))
end

module Request = struct
  type t =
    { spec : Session.Spec.t
    ; code : Code_selection.t
    ; transcript : Transcript.t
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  let spec t = t.spec
  let code t = t.code
  let transcript t = t.transcript
  let idempotency_key t = t.idempotency_key

  let to_json t =
    `Object
      [ "spec", Session.Spec.to_json t.spec
      ; "code", Code_selection.to_json t.code
      ; "transcript", Transcript.to_json t.transcript
      ; "idempotency_key", Idempotency_key.to_json t.idempotency_key
      ]
  ;;

  let create spec ~code ~transcript ~idempotency_key =
    let open Result.Let_syntax in
    let%bind spec =
      Session.Spec.create
        ~execution_host:spec.Session.Spec.execution_host
        ~prompt:spec.prompt
        ~workspace:spec.workspace
        ~liveness:spec.liveness
        ~persistence:spec.persistence
        ?permission_profile:spec.permission_profile
        ~start_immediately:spec.start_immediately
        ?display_name:spec.display_name
        ~labels:spec.labels
        ()
    in
    let%bind () =
      if spec.start_immediately
      then invalid "seed must create a stopped conversation"
      else Ok ()
    in
    let%bind () =
      match spec.workspace with
      | Configured _ -> Ok ()
      | Current | Local_path _ -> invalid "seed requires a configured workspace"
    in
    let%bind () =
      match spec.prompt, code with
      | Catalog id, Code_selection.Catalog { definition_id; _ }
        when Id.Prompt_definition.equal id definition_id -> Ok ()
      | Catalog _, Retained _ -> Ok ()
      | Catalog _, Catalog _
      | Local_path _, (Catalog _ | Retained _)
      | Generated _, (Catalog _ | Retained _) ->
        invalid "seed requires an agreeing authored Catalog prompt"
    in
    let%bind _ = Idempotency_key.of_string (Idempotency_key.to_string idempotency_key) in
    let t = { spec; code; transcript; idempotency_key } in
    let%map () =
      with_work ~max_work_steps:default_work ~checkpoint:Fn.ignore (fun poll ->
        admit_json (to_json t) ~limits:wire_limits ~poll)
    in
    t
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind () =
      with_work ~max_work_steps:default_work ~checkpoint:Fn.ignore (fun poll ->
        admit_json json ~limits:wire_limits ~poll)
    in
    let%bind f = fields json [ "spec"; "code"; "transcript"; "idempotency_key" ] in
    let%bind spec = Json_codec.required_as f "spec" Session.Spec.of_json in
    let%bind code = Json_codec.required_as f "code" Code_selection.of_json in
    let%bind transcript = Json_codec.required_as f "transcript" Transcript.of_json in
    let%bind idempotency_key =
      Json_codec.required_as f "idempotency_key" Idempotency_key.of_json
    in
    create spec ~code ~transcript ~idempotency_key
  ;;

  let t_of_sexp sexp =
    let { spec; code; transcript; idempotency_key } = t_of_sexp sexp in
    sexp_result sexp (create spec ~code ~transcript ~idempotency_key)
  ;;
end

module Result = struct
  type t = { session_id : Id.Session.t } [@@deriving sexp]

  let to_json t = `Object [ "session_id", Id.Session.to_json t.session_id ]

  let of_json json =
    Core.Result.bind (fields json [ "session_id" ]) ~f:(fun f ->
      Core.Result.map
        (Json_codec.required_as f "session_id" Id.Session.of_json)
        ~f:(fun session_id -> { session_id }))
  ;;
end
