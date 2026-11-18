open! Core

type t =
  { revision : int64
  ; project_selected : bool
  ; project_files : int
  ; skills : int
  ; retained_captures : int
  ; retained_receipts : int
  }
[@@deriving equal, sexp]

let decoded_of_sexp = t_of_sexp

let create
      ~revision
      ~project_selected
      ~project_files
      ~skills
      ~retained_captures
      ~retained_receipts
  =
  if
    ((not project_selected) && project_files <> 0)
    || Int64.(revision < 0L)
    || project_files < 0
    || project_files > 64
    || skills < 0
    || skills > 64
    || project_files + skills > 64
    || retained_captures < 0
    || retained_captures > 128
    || retained_receipts < 0
    || retained_receipts > 128
    || (not (Int64.equal revision (Int64.of_int retained_receipts)))
    || ((project_selected || skills > 0) && retained_captures = 0)
  then Error (Protocol_error.invalid_request "invalid instruction selection summary")
  else
    Ok
      { revision
      ; project_selected
      ; project_files
      ; skills
      ; retained_captures
      ; retained_receipts
      }
;;

let t_of_sexp sexp =
  let wire = decoded_of_sexp sexp in
  match
    create
      ~revision:wire.revision
      ~project_selected:wire.project_selected
      ~project_files:wire.project_files
      ~skills:wire.skills
      ~retained_captures:wire.retained_captures
      ~retained_receipts:wire.retained_receipts
  with
  | Ok value -> value
  | Error error -> Sexplib.Conv.of_sexp_error error.Protocol_error.message sexp
;;

let to_json t =
  `Object
    [ "revision", `String (Int64.to_string t.revision)
    ; ("project_selected", if t.project_selected then `True else `False)
    ; "project_files", `Number (Int.to_string t.project_files)
    ; "skills", `Number (Int.to_string t.skills)
    ; "retained_captures", `Number (Int.to_string t.retained_captures)
    ; "retained_receipts", `Number (Int.to_string t.retained_receipts)
    ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind () = Json_codec.validate_limits ~max_depth:8 ~max_bytes:4096 json in
  let%bind fields = Json_codec.fields json in
  let%bind revision =
    Json_codec.required_as fields "revision" (function
      | `String text ->
        (match Int64.of_string_opt text with
         | Some value
           when Int64.(value >= 0L) && String.equal text (Int64.to_string value) ->
           Ok value
         | None | Some _ ->
           Error (Protocol_error.invalid_request "invalid instruction summary revision"))
      | _ ->
        Error
          (Protocol_error.invalid_request
             "instruction summary revision must be decimal text"))
  in
  let count name max =
    Json_codec.required_as fields name (Json_codec.bounded_int ~min:0 ~max)
  in
  let%bind project_selected =
    Json_codec.required_as fields "project_selected" Json_codec.bool
  in
  let%bind project_files = count "project_files" 64 in
  let%bind skills = count "skills" 64 in
  let%bind retained_captures = count "retained_captures" 128 in
  let%bind retained_receipts = count "retained_receipts" 128 in
  create
    ~revision
    ~project_selected
    ~project_files
    ~skills
    ~retained_captures
    ~retained_receipts
;;
