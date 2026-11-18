open! Core
module Error = Protocol_error
module J = Json_codec

module Codec = struct
  let limits max_bytes =
    Document_schema.Limits.create
      ~max_bytes
      ~max_depth:12
      ~max_fields:1024
      ~max_nodes:2048
    |> function
    | Ok limits -> limits
    | Error error -> raise_s [%sexp (error : Document_schema.Error.t)]
  ;;

  let request_limits = limits 131072
  let result_limits = limits 524288
  let invalid message = Error (Error.invalid_request message)

  let admit limits json =
    Document_schema.Json.validate ~limits json
    |> Result.map_error ~f:(fun _ -> Error.invalid_request "instruction wire limit")
  ;;

  let fields json names =
    let open Result.Let_syntax in
    let%bind fields = J.fields json in
    if
      List.exists (J.to_alist fields) ~f:(fun (name, _) ->
        not (List.mem names name ~equal:String.equal))
    then invalid "unknown instruction wire field"
    else Ok fields
  ;;

  let list decode = function
    | `Array values when List.length values <= 64 ->
      List.map values ~f:decode |> Result.all
    | `Null | `True | `False | `Number _ | `String _ | `Object _ | `Array _ ->
      invalid "instruction wire list limit"
  ;;

  let nullable decode = function
    | `Null -> Ok None
    | json -> Result.map (decode json) ~f:Option.some
  ;;

  let option encode value = Option.value_map value ~default:`Null ~f:encode
  let array encode values = `Array (List.map values ~f:encode)

  let domain result =
    Result.map_error result ~f:(fun _ ->
      Error.invalid_request "invalid instruction identity")
  ;;

  let path json =
    let%bind.Result value = J.string json in
    Source.Portable_path.create value |> domain
  ;;

  let path_json value = `String (Source.Portable_path.to_string value)

  let digest json =
    let%bind.Result value = J.string json in
    Chatml_source.Digest.of_hex value |> domain
  ;;

  let digest_json value = `String (Chatml_source.Digest.to_hex value)
  let generation = J.bounded_int ~min:0 ~max:Int.max_value

  let revision json =
    let%bind.Result text = J.string json in
    match Int64.of_string_opt text with
    | Some value when Int64.(value >= 0L) && String.equal text (Int64.to_string value) ->
      Ok value
    | None | Some _ -> invalid "invalid instruction revision"
  ;;

  let revision_json value = `String (Int64.to_string value)

  let unique values ~equal =
    not
      (List.existsi values ~f:(fun index value ->
         List.mem (List.take values index) value ~equal))
  ;;
end

module Root_id = struct
  type t = string [@@deriving equal, sexp_of]

  let alphanumeric = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
    | _ -> false
  ;;

  let of_string value =
    if
      String.is_empty value
      || String.length value > 128
      || (not (alphanumeric value.[0]))
      || not
           (String.for_all value ~f:(fun c ->
              alphanumeric c || Char.equal c '_' || Char.equal c '-'))
    then Codec.invalid "invalid instruction root ID"
    else Ok value
  ;;

  let to_string t = t
  let to_json t = `String t

  let of_json json =
    let%bind.Result text = J.string json in
    of_string text
  ;;
end

module Member = struct
  type t =
    { root : Root_id.t
    ; member : Source.Portable_path.t
    ; digest : Chatml_source.Digest.t
    }
  [@@deriving equal, sexp_of]

  let create ~root ~member ~digest = { root; member; digest }

  let same_identity left right =
    Root_id.equal left.root right.root
    && Source.Portable_path.equal left.member right.member
  ;;

  let to_json t =
    `Object
      [ "root", Root_id.to_json t.root
      ; "member", Codec.path_json t.member
      ; "sha256", Codec.digest_json t.digest
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Codec.fields json [ "root"; "member"; "sha256" ] in
    let%bind root = J.required_as fields "root" Root_id.of_json in
    let%bind member = J.required_as fields "member" Codec.path in
    let%map digest = J.required_as fields "sha256" Codec.digest in
    create ~root ~member ~digest
  ;;
end

module Selection = struct
  type t =
    | Project of
        { root : Root_id.t
        ; scope : Source.Portable_path.t option
        }
    | Skills of Root_id.t list
  [@@deriving sexp_of]

  let roots = function
    | Project { root; _ } -> [ root ]
    | Skills roots -> roots
  ;;

  let validate = function
    | Project _ -> Ok ()
    | Skills roots ->
      if
        List.is_empty roots
        || List.length roots > 64
        || not (Codec.unique roots ~equal:Root_id.equal)
      then Codec.invalid "instruction skills need distinct bounded roots"
      else Ok ()
  ;;

  let to_json = function
    | Project { root; scope } ->
      `Object
        [ "kind", `String "project"
        ; "roots", `Array [ Root_id.to_json root ]
        ; "scope", Codec.option Codec.path_json scope
        ]
    | Skills roots ->
      `Object
        [ "kind", `String "skills"
        ; "roots", Codec.array Root_id.to_json roots
        ; "scope", `Null
        ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Codec.fields json [ "kind"; "roots"; "scope" ] in
    let%bind kind =
      J.required_as
        fields
        "kind"
        (J.enum ~name:"instruction kind" [ "project", `Project; "skills", `Skills ])
    in
    let%bind roots = J.required_as fields "roots" (Codec.list Root_id.of_json) in
    let%bind scope = J.required fields "scope" in
    let%bind selection =
      match kind, roots, scope with
      | `Project, [ root ], scope ->
        let%map scope = Codec.nullable Codec.path scope in
        Project { root; scope }
      | `Skills, roots, `Null -> Ok (Skills roots)
      | `Project, _, _ | `Skills, _, _ ->
        Codec.invalid "instruction selection kind mismatch"
    in
    let%map () = validate selection in
    selection
  ;;
end

module Source_request = struct
  type t =
    { selection : Selection.t
    ; members : Member.t list
    }
  [@@deriving sexp_of]

  let selection t = t.selection
  let members t = t.members

  let create ~selection ~members =
    let open Result.Let_syntax in
    let%bind () = Selection.validate selection in
    if
      List.length members > 64
      || (not (Codec.unique members ~equal:Member.same_identity))
      || List.exists members ~f:(fun member ->
        not (List.mem (Selection.roots selection) member.Member.root ~equal:Root_id.equal))
    then Codec.invalid "invalid bounded instruction manifest"
    else (
      match selection, members with
      | Selection.Skills _, [] -> Codec.invalid "instruction skills need members"
      | Project _, _ | Skills _, _ :: _ -> Ok { selection; members })
  ;;

  let to_json t =
    `Object
      [ "selection", Selection.to_json t.selection
      ; "members", Codec.array Member.to_json t.members
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Codec.fields json [ "selection"; "members" ] in
    let%bind selection = J.required_as fields "selection" Selection.of_json in
    let%bind members = J.required_as fields "members" (Codec.list Member.of_json) in
    create ~selection ~members
  ;;
end

module Capture = struct
  type t =
    { source : Source_request.t
    ; capture_id : Chatml_source.Digest.t
    }
  [@@deriving sexp_of]

  let create ~source ~capture_id = { source; capture_id }

  let to_json t =
    `Object
      [ "source", Source_request.to_json t.source
      ; "capture_id", Codec.digest_json t.capture_id
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Codec.fields json [ "source"; "capture_id" ] in
    let%bind source = J.required_as fields "source" Source_request.of_json in
    let%map capture_id = J.required_as fields "capture_id" Codec.digest in
    create ~source ~capture_id
  ;;
end

module Action = struct
  type t =
    | Inspect
    | Refresh
    | Capture
    | Activate
    | Remove
  [@@deriving equal, sexp_of]

  let names =
    [ "inspect", Inspect
    ; "refresh", Refresh
    ; "capture", Capture
    ; "activate", Activate
    ; "remove", Remove
    ]
  ;;

  let of_json = J.enum ~name:"instruction action" names

  let to_json = function
    | Inspect -> `String "inspect"
    | Refresh -> `String "refresh"
    | Capture -> `String "capture"
    | Activate -> `String "activate"
    | Remove -> `String "remove"
  ;;
end

module View = struct
  type t =
    { session_id : Id.Session.t
    ; generation : int
    ; summary : Instruction_status.t
    ; project : Source_request.t option
    ; skills : Member.t list
    ; permitted_actions : Action.t list
    }
  [@@deriving sexp_of]

  let create ~session_id ~generation ~summary ~project ~skills ~permitted_actions =
    let project_valid =
      match project with
      | None -> not summary.Instruction_status.project_selected
      | Some project ->
        summary.project_selected
        &&
          (match Source_request.selection project with
          | Project _ -> true
          | Skills _ -> false)
    in
    let project_files =
      Option.value_map project ~default:0 ~f:(fun p ->
        List.length (Source_request.members p))
    in
    if
      generation < 0
      || (not project_valid)
      || project_files <> summary.project_files
      || List.length skills <> summary.skills
      || (not (Codec.unique skills ~equal:Member.same_identity))
      || List.length permitted_actions > 5
      || not (Codec.unique permitted_actions ~equal:Action.equal)
    then Codec.invalid "instruction view disagrees with selection summary"
    else Ok { session_id; generation; summary; project; skills; permitted_actions }
  ;;

  let to_json t =
    `Object
      [ "session_id", Id.Session.to_json t.session_id
      ; "generation", `Number (Int.to_string t.generation)
      ; "summary", Instruction_status.to_json t.summary
      ; "project", Codec.option Source_request.to_json t.project
      ; "skills", Codec.array Member.to_json t.skills
      ; "permitted_actions", Codec.array Action.to_json t.permitted_actions
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields =
      Codec.fields
        json
        [ "session_id"
        ; "generation"
        ; "summary"
        ; "project"
        ; "skills"
        ; "permitted_actions"
        ]
    in
    let%bind session_id = J.required_as fields "session_id" Id.Session.of_json in
    let%bind generation = J.required_as fields "generation" Codec.generation in
    let%bind summary = J.required_as fields "summary" Instruction_status.of_json in
    let%bind project =
      J.required_as fields "project" (Codec.nullable Source_request.of_json)
    in
    let%bind skills = J.required_as fields "skills" (Codec.list Member.of_json) in
    let%bind permitted_actions =
      J.required_as fields "permitted_actions" (Codec.list Action.of_json)
    in
    create ~session_id ~generation ~summary ~project ~skills ~permitted_actions
  ;;
end

module Discovery = struct
  module Issue = struct
    type code =
      | Skipped
      | Unavailable
    [@@deriving equal, sexp_of]

    type t =
      { root : Root_id.t
      ; member : Source.Portable_path.t
      ; code : code
      }
    [@@deriving sexp_of]

    let to_json t =
      `Object
        [ "root", Root_id.to_json t.root
        ; "member", Codec.path_json t.member
        ; ( "code"
          , `String
              (match t.code with
               | Skipped -> "skipped"
               | Unavailable -> "unavailable") )
        ]
    ;;

    let of_json json =
      let open Result.Let_syntax in
      let%bind fields = Codec.fields json [ "root"; "member"; "code" ] in
      let%bind root = J.required_as fields "root" Root_id.of_json in
      let%bind member = J.required_as fields "member" Codec.path in
      let%map code =
        J.required_as
          fields
          "code"
          (J.enum
             ~name:"instruction issue"
             [ "skipped", Skipped; "unavailable", Unavailable ])
      in
      { root; member; code }
    ;;
  end

  type t =
    { roots : Root_id.t list
    ; members : Member.t list
    ; issues : Issue.t list
    }
  [@@deriving sexp_of]

  let roots t = t.roots
  let members t = t.members
  let issues t = t.issues

  let create ~roots ~members ~issues =
    if
      List.length roots > 64
      || (not (Codec.unique roots ~equal:Root_id.equal))
      || List.length members + List.length issues > 64
      || (not (Codec.unique members ~equal:Member.same_identity))
      || List.exists members ~f:(fun member ->
        not (List.mem roots member.Member.root ~equal:Root_id.equal))
      || List.exists issues ~f:(fun issue ->
        not (List.mem roots issue.Issue.root ~equal:Root_id.equal))
    then Codec.invalid "invalid instruction discovery"
    else Ok { roots; members; issues }
  ;;

  let to_json t =
    `Object
      [ "roots", Codec.array Root_id.to_json t.roots
      ; "members", Codec.array Member.to_json t.members
      ; "issues", Codec.array Issue.to_json t.issues
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Codec.fields json [ "roots"; "members"; "issues" ] in
    let%bind roots = J.required_as fields "roots" (Codec.list Root_id.of_json) in
    let%bind members = J.required_as fields "members" (Codec.list Member.of_json) in
    let%bind issues = J.required_as fields "issues" (Codec.list Issue.of_json) in
    create ~roots ~members ~issues
  ;;
end

module Receipt = struct
  type t =
    { principal_id : Id.Principal.t
    ; key : Idempotency_key.t
    ; request_sha256 : string
    ; session_id : Id.Session.t
    ; generation : int
    ; revision : int64
    ; accepted_at : Timestamp.t
    }
  [@@deriving equal, sexp_of]

  let create
        ~principal_id
        ~key
        ~request_sha256
        ~session_id
        ~generation
        ~revision
        ~accepted_at
    =
    let open Result.Let_syntax in
    let%bind digest = Chatml_source.Digest.of_hex request_sha256 |> Codec.domain in
    if
      generation < 0
      || Int64.(revision <= 0L)
      || not (String.equal request_sha256 (Chatml_source.Digest.to_hex digest))
    then Codec.invalid "invalid instruction receipt"
    else
      Ok
        { principal_id
        ; key
        ; request_sha256
        ; session_id
        ; generation
        ; revision
        ; accepted_at
        }
  ;;

  let to_json t =
    `Object
      [ "principal_id", Id.Principal.to_json t.principal_id
      ; "key", Idempotency_key.to_json t.key
      ; "request_sha256", `String t.request_sha256
      ; "session_id", Id.Session.to_json t.session_id
      ; "generation", `Number (Int.to_string t.generation)
      ; "revision", Codec.revision_json t.revision
      ; "accepted_at", Timestamp.to_json t.accepted_at
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields =
      Codec.fields
        json
        [ "principal_id"
        ; "key"
        ; "request_sha256"
        ; "session_id"
        ; "generation"
        ; "revision"
        ; "accepted_at"
        ]
    in
    let%bind principal_id = J.required_as fields "principal_id" Id.Principal.of_json in
    let%bind key = J.required_as fields "key" Idempotency_key.of_json in
    let%bind request_sha256 = J.required_as fields "request_sha256" J.string in
    let%bind session_id = J.required_as fields "session_id" Id.Session.of_json in
    let%bind generation = J.required_as fields "generation" Codec.generation in
    let%bind revision = J.required_as fields "revision" Codec.revision in
    let%bind accepted_at = J.required_as fields "accepted_at" Timestamp.of_json in
    create
      ~principal_id
      ~key
      ~request_sha256
      ~session_id
      ~generation
      ~revision
      ~accepted_at
  ;;
end

module Request = struct
  module Inspect = struct
    type t =
      { session_id : Id.Session.t
      ; attachment_id : Id.Attachment.t option
      }
    [@@deriving sexp_of]
  end

  module Refresh = struct
    type t =
      { session_id : Id.Session.t
      ; selection : Selection.t option
      }
    [@@deriving sexp_of]
  end

  module Capture = struct
    type t =
      { session_id : Id.Session.t
      ; source : Source_request.t
      }
    [@@deriving sexp_of]
  end

  module Activate = struct
    type t =
      { session_id : Id.Session.t
      ; attachment_id : Id.Attachment.t
      ; expected_generation : int
      ; expected_revision : int64
      ; source : Source_request.t
      ; capture_id : Chatml_source.Digest.t
      ; key : Idempotency_key.t
      }
    [@@deriving sexp_of]
  end

  module Remove = struct
    type change =
      | All
      | Project
      | Skills of Member.t list
    [@@deriving sexp_of]

    type t =
      { session_id : Id.Session.t
      ; attachment_id : Id.Attachment.t
      ; expected_generation : int
      ; expected_revision : int64
      ; change : change
      ; key : Idempotency_key.t
      }
    [@@deriving sexp_of]

    let change_json = function
      | All -> `Object [ "kind", `String "all"; "members", `Null ]
      | Project -> `Object [ "kind", `String "project"; "members", `Null ]
      | Skills members ->
        `Object
          [ "kind", `String "skills"; "members", Codec.array Member.to_json members ]
    ;;

    let change_of_json json =
      let open Result.Let_syntax in
      let%bind fields = Codec.fields json [ "kind"; "members" ] in
      let%bind kind =
        J.required_as
          fields
          "kind"
          (J.enum
             ~name:"instruction removal"
             [ "all", `All; "project", `Project; "skills", `Skills ])
      in
      let%bind members = J.required fields "members" in
      match kind, members with
      | `All, `Null -> Ok All
      | `Project, `Null -> Ok Project
      | `Skills, members ->
        let%bind members = Codec.list Member.of_json members in
        if Codec.unique members ~equal:Member.same_identity
        then Ok (Skills members)
        else Codec.invalid "duplicate instruction removal member"
      | (`All | `Project), _ -> Codec.invalid "instruction removal kind mismatch"
    ;;
  end

  type t =
    | Inspect of Inspect.t
    | Refresh of Refresh.t
    | Capture of Capture.t
    | Activate of Activate.t
    | Remove of Remove.t
  [@@deriving sexp_of]

  let methods =
    [ "session.instructions_get"
    ; "session.instructions_refresh"
    ; "session.instructions_capture"
    ; "session.instructions_activate"
    ; "session.instructions_remove"
    ]
  ;;

  let method_name = function
    | Inspect _ -> "session.instructions_get"
    | Refresh _ -> "session.instructions_refresh"
    | Capture _ -> "session.instructions_capture"
    | Activate _ -> "session.instructions_activate"
    | Remove _ -> "session.instructions_remove"
  ;;

  let session_id = function
    | Inspect t -> t.session_id
    | Refresh t -> t.session_id
    | Capture t -> t.session_id
    | Activate t -> t.session_id
    | Remove t -> t.session_id
  ;;

  let params = function
    | Inspect t ->
      `Object
        [ "session_id", Id.Session.to_json t.session_id
        ; "attachment_id", Codec.option Id.Attachment.to_json t.attachment_id
        ]
    | Refresh t ->
      `Object
        [ "session_id", Id.Session.to_json t.session_id
        ; "selection", Codec.option Selection.to_json t.selection
        ]
    | Capture t ->
      `Object
        [ "session_id", Id.Session.to_json t.session_id
        ; "source", Source_request.to_json t.source
        ]
    | Activate t ->
      `Object
        [ "session_id", Id.Session.to_json t.session_id
        ; "attachment_id", Id.Attachment.to_json t.attachment_id
        ; "expected_generation", `Number (Int.to_string t.expected_generation)
        ; "expected_revision", Codec.revision_json t.expected_revision
        ; "source", Source_request.to_json t.source
        ; "capture_id", Codec.digest_json t.capture_id
        ; "key", Idempotency_key.to_json t.key
        ]
    | Remove t ->
      `Object
        [ "session_id", Id.Session.to_json t.session_id
        ; "attachment_id", Id.Attachment.to_json t.attachment_id
        ; "expected_generation", `Number (Int.to_string t.expected_generation)
        ; "expected_revision", Codec.revision_json t.expected_revision
        ; "change", Remove.change_json t.change
        ; "key", Idempotency_key.to_json t.key
        ]
  ;;

  let of_method_and_params ~method_ ~params =
    let open Result.Let_syntax in
    let%bind () = Codec.admit Codec.request_limits params in
    let fields names = Codec.fields params ("session_id" :: names) in
    let session fields = J.required_as fields "session_id" Id.Session.of_json in
    match method_ with
    | "session.instructions_get" ->
      let%bind fields = fields [ "attachment_id" ] in
      let%bind session_id = session fields in
      let%map attachment_id =
        J.required_as fields "attachment_id" (Codec.nullable Id.Attachment.of_json)
      in
      Inspect { session_id; attachment_id }
    | "session.instructions_refresh" ->
      let%bind fields = fields [ "selection" ] in
      let%bind session_id = session fields in
      let%map selection =
        J.required_as fields "selection" (Codec.nullable Selection.of_json)
      in
      Refresh { session_id; selection }
    | "session.instructions_capture" ->
      let%bind fields = fields [ "source" ] in
      let%bind session_id = session fields in
      let%map source = J.required_as fields "source" Source_request.of_json in
      Capture { session_id; source }
    | "session.instructions_activate" ->
      let%bind fields =
        fields
          [ "attachment_id"
          ; "expected_generation"
          ; "expected_revision"
          ; "source"
          ; "capture_id"
          ; "key"
          ]
      in
      let%bind session_id = session fields in
      let%bind attachment_id =
        J.required_as fields "attachment_id" Id.Attachment.of_json
      in
      let%bind expected_generation =
        J.required_as fields "expected_generation" Codec.generation
      in
      let%bind expected_revision =
        J.required_as fields "expected_revision" Codec.revision
      in
      let%bind source = J.required_as fields "source" Source_request.of_json in
      let%bind capture_id = J.required_as fields "capture_id" Codec.digest in
      let%map key = J.required_as fields "key" Idempotency_key.of_json in
      Activate
        { session_id
        ; attachment_id
        ; expected_generation
        ; expected_revision
        ; source
        ; capture_id
        ; key
        }
    | "session.instructions_remove" ->
      let%bind fields =
        fields
          [ "attachment_id"; "expected_generation"; "expected_revision"; "change"; "key" ]
      in
      let%bind session_id = session fields in
      let%bind attachment_id =
        J.required_as fields "attachment_id" Id.Attachment.of_json
      in
      let%bind expected_generation =
        J.required_as fields "expected_generation" Codec.generation
      in
      let%bind expected_revision =
        J.required_as fields "expected_revision" Codec.revision
      in
      let%bind change = J.required_as fields "change" Remove.change_of_json in
      let%map key = J.required_as fields "key" Idempotency_key.of_json in
      Remove
        { session_id; attachment_id; expected_generation; expected_revision; change; key }
    | _ ->
      Error
        (Error.create
           Method_not_found
           ~message:"unknown instruction method"
           ~retryable:false
           ())
  ;;

  let to_json t = `Object [ "method", `String (method_name t); "params", params t ]

  let of_json json =
    let open Result.Let_syntax in
    let%bind () = Codec.admit Codec.request_limits json in
    let%bind fields = Codec.fields json [ "method"; "params" ] in
    let%bind method_ = J.required_as fields "method" J.string in
    let%bind params = J.required fields "params" in
    of_method_and_params ~method_ ~params
  ;;
end

module Result = struct
  type t =
    | Inspect of View.t
    | Refresh of Discovery.t
    | Capture of Capture.t
    | Activate of Receipt.t
    | Remove of Receipt.t
  [@@deriving sexp_of]

  let method_name = function
    | Inspect _ -> "session.instructions_get"
    | Refresh _ -> "session.instructions_refresh"
    | Capture _ -> "session.instructions_capture"
    | Activate _ -> "session.instructions_activate"
    | Remove _ -> "session.instructions_remove"
  ;;

  let payload = function
    | Inspect t -> View.to_json t
    | Refresh t -> Discovery.to_json t
    | Capture t -> Capture.to_json t
    | Activate t | Remove t -> Receipt.to_json t
  ;;

  let of_method_and_result ~method_ ~result =
    let open Result.Let_syntax in
    let%bind () = Codec.admit Codec.result_limits result in
    match method_ with
    | "session.instructions_get" ->
      Result.map (View.of_json result) ~f:(fun t -> Inspect t)
    | "session.instructions_refresh" ->
      Result.map (Discovery.of_json result) ~f:(fun t -> Refresh t)
    | "session.instructions_capture" ->
      Result.map (Capture.of_json result) ~f:(fun t -> Capture t)
    | "session.instructions_activate" ->
      Result.map (Receipt.of_json result) ~f:(fun t -> Activate t)
    | "session.instructions_remove" ->
      Result.map (Receipt.of_json result) ~f:(fun t -> Remove t)
    | _ ->
      Error
        (Error.create
           Method_not_found
           ~message:"unknown instruction result"
           ~retryable:false
           ())
  ;;

  let to_json t = `Object [ "method", `String (method_name t); "result", payload t ]

  let of_json json =
    let open Result.Let_syntax in
    let%bind () = Codec.admit Codec.result_limits json in
    let%bind fields = Codec.fields json [ "method"; "result" ] in
    let%bind method_ = J.required_as fields "method" J.string in
    let%bind result = J.required fields "result" in
    of_method_and_result ~method_ ~result
  ;;
end
