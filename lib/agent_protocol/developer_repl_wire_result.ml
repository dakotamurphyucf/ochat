open! Core
module C = Developer_codec
module D = Document_schema
module Base = Developer_repl_wire_common
module Limits = Base.Limits
module Wire_id = Base.Wire_id

let ( let* ) result f = Result.bind result ~f
let get = Base.get
let optional = Base.optional

module View = struct
  type t =
    { instance : Wire_id.Instance.t
    ; epoch : Wire_id.Epoch.t
    ; revision : Wire_id.Revision.t
    ; run_id : Id.Developer_run.t
    ; capture_fingerprint : Chatml_source.Digest.t
    }

  let to_json t =
    `Object
      [ "instance", `String (Wire_id.Instance.to_string t.instance)
      ; "epoch", `String (Wire_id.Epoch.to_string t.epoch)
      ; "revision", `String (Wire_id.Revision.to_string t.revision)
      ; "run_id", Id.Developer_run.to_json t.run_id
      ; "capture_fingerprint", C.digest_json t.capture_fingerprint
      ]
  ;;

  let create ~limits ~instance ~epoch ~revision ~run_id ~capture_fingerprint =
    let t = { instance; epoch; revision; run_id; capture_fingerprint } in
    Result.map (C.validate ~limits:(Limits.result limits) (to_json t)) ~f:(fun () -> t)
  ;;

  let instance t = t.instance
  let epoch t = t.epoch
  let revision t = t.revision
  let run_id t = t.run_id
  let capture_fingerprint t = t.capture_fingerprint

  let of_json ~limits json =
    let* () = C.validate ~limits:(Limits.result limits) json in
    let* fields =
      C.fields
        ~allowed:[ "instance"; "epoch"; "revision"; "run_id"; "capture_fingerprint" ]
        json
    in
    let* instance =
      get fields "instance" (fun json ->
        let* value = C.string ~max_bytes:96 json in
        Wire_id.Instance.of_string value)
    in
    let* epoch =
      get fields "epoch" (fun json ->
        let* value = C.string ~max_bytes:96 json in
        Wire_id.Epoch.of_string value)
    in
    let* revision =
      get fields "revision" (fun json ->
        let* value = C.string ~max_bytes:96 json in
        Wire_id.Revision.of_string value)
    in
    let* run_id = get fields "run_id" Id.Developer_run.of_json in
    let* capture_fingerprint = get fields "capture_fingerprint" C.digest in
    create ~limits ~instance ~epoch ~revision ~run_id ~capture_fingerprint
  ;;
end

module Submission = struct
  let max_summaries limits = D.Limits.max_nodes (Limits.result limits)

  module Location = struct
    type t =
      { source : Chatml_source.Id.t
      ; digest : Chatml_source.Digest.t
      ; start_byte : int
      ; end_byte : int
      }

    let to_json t =
      `Object
        [ "source", C.source_id_json t.source
        ; "digest", C.digest_json t.digest
        ; "start_byte", C.integer_json t.start_byte
        ; "end_byte", C.integer_json t.end_byte
        ]
    ;;

    let create ~limits ~source ~digest ~start_byte ~end_byte =
      let t = { source; digest; start_byte; end_byte } in
      if
        start_byte < 0
        || end_byte < start_byte
        || end_byte > Authoring_codec.maximum_wire_integer
      then C.invalid "invalid REPL diagnostic byte range"
      else
        let* () = Authoring_codec.validate_source_id source in
        Result.map
          (C.validate ~limits:(Limits.result limits) (to_json t))
          ~f:(fun () -> t)
    ;;

    let source t = t.source
    let digest t = t.digest
    let start_byte t = t.start_byte
    let end_byte t = t.end_byte

    let of_json ~limits json =
      let* fields =
        C.fields ~allowed:[ "source"; "digest"; "start_byte"; "end_byte" ] json
      in
      let* source = get fields "source" C.source_id in
      let* digest = get fields "digest" C.digest in
      let* start_byte = get fields "start_byte" C.nonnegative in
      let* end_byte = get fields "end_byte" C.nonnegative in
      create ~limits ~source ~digest ~start_byte ~end_byte
    ;;
  end

  module Rendered_type = struct
    type kind =
      | Type
      | Signature
    [@@deriving equal, sexp_of]

    type truncation =
      | Node_limit
      | Edge_limit
      | Depth_limit
      | Work_limit
      | Byte_limit
    [@@deriving equal, sexp_of]

    type completeness =
      | Complete
      | Truncated of truncation
    [@@deriving equal, sexp_of]

    type t =
      { kind : kind
      ; completeness : completeness
      ; text : string
      }

    let kind_text = function
      | Type -> "type"
      | Signature -> "signature"
    ;;

    let truncation_text = function
      | Node_limit -> "node_limit"
      | Edge_limit -> "edge_limit"
      | Depth_limit -> "depth_limit"
      | Work_limit -> "work_limit"
      | Byte_limit -> "byte_limit"
    ;;

    let to_json t =
      `Object
        [ "kind", `String (kind_text t.kind)
        ; ( "completeness"
          , match t.completeness with
            | Complete -> `Object [ "kind", `String "complete" ]
            | Truncated reason ->
              `Object
                [ "kind", `String "truncated"
                ; "reason", `String (truncation_text reason)
                ] )
        ; "text", `String t.text
        ]
    ;;

    let create ~limits ~kind ~completeness ~text =
      let t = { kind; completeness; text } in
      if String.length text > Limits.max_output_bytes limits
      then C.limit "REPL type display exceeds output bound"
      else
        Result.map
          (C.validate ~limits:(Limits.result limits) (to_json t))
          ~f:(fun () -> t)
    ;;

    let kind t = t.kind
    let completeness t = t.completeness
    let text t = t.text

    let of_json ~limits json =
      let* fields = C.fields ~allowed:[ "kind"; "completeness"; "text" ] json in
      let* kind = get fields "kind" (C.string ~max_bytes:32) in
      let* kind =
        match kind with
        | "type" -> Ok Type
        | "signature" -> Ok Signature
        | _ -> C.invalid "unknown REPL type display kind"
      in
      let* completeness =
        get fields "completeness" (fun json ->
          let* fields = C.fields ~allowed:[ "kind"; "reason" ] json in
          let* kind = get fields "kind" (C.string ~max_bytes:32) in
          match kind with
          | "complete" ->
            let* () = C.fields ~allowed:[ "kind" ] json |> Result.map ~f:ignore in
            Ok Complete
          | "truncated" ->
            let* reason = get fields "reason" (C.string ~max_bytes:32) in
            let* reason =
              match reason with
              | "node_limit" -> Ok Node_limit
              | "edge_limit" -> Ok Edge_limit
              | "depth_limit" -> Ok Depth_limit
              | "work_limit" -> Ok Work_limit
              | "byte_limit" -> Ok Byte_limit
              | _ -> C.invalid "unknown REPL type truncation"
            in
            Ok (Truncated reason)
          | _ -> C.invalid "unknown REPL type completeness")
      in
      let* text =
        get fields "text" (C.string ~max_bytes:(Limits.max_output_bytes limits))
      in
      create ~limits ~kind ~completeness ~text
    ;;
  end

  module Diagnostic = struct
    type t =
      { message : string
      ; location : Location.t option
      }

    let to_json t =
      `Object
        ([ "message", `String t.message ]
         @ C.optional_field "location" t.location Location.to_json)
    ;;

    let create ~limits ~message ~location =
      let t = { message; location } in
      if String.length message > Limits.max_output_bytes limits
      then C.limit "REPL diagnostic exceeds output bound"
      else
        let* () =
          match location with
          | None -> Ok ()
          | Some location ->
            Location.of_json ~limits (Location.to_json location) |> Result.map ~f:ignore
        in
        Result.map
          (C.validate ~limits:(Limits.result limits) (to_json t))
          ~f:(fun () -> t)
    ;;

    let message t = t.message
    let location t = t.location

    let of_json ~limits json =
      let* fields = C.fields ~allowed:[ "message"; "location" ] json in
      let* message =
        get fields "message" (C.string ~max_bytes:(Limits.max_output_bytes limits))
      in
      let* location = optional fields "location" (Location.of_json ~limits) in
      create ~limits ~message ~location
    ;;
  end

  module Summary = struct
    type failure_kind =
      | Initializer
      | Runtime
      | Budget
    [@@deriving equal, sexp_of]

    type outcome =
      | Succeeded
      | Failed of failure_kind
    [@@deriving equal, sexp_of]

    type task_retention =
      | Not_a_task
      | Retained
      | Limit_reached
    [@@deriving equal, sexp_of]

    type t =
      { outcome : outcome
      ; value_preview : string option
      ; diagnostic : Diagnostic.t option
      ; expression_type : Rendered_type.t option
      ; binding_count : int
      ; task_retention : task_retention
      ; text_limited : bool
      }

    let outcome_json = function
      | Succeeded -> `Object [ "kind", `String "succeeded" ]
      | Failed failure ->
        `Object
          [ "kind", `String "failed"
          ; ( "reason"
            , `String
                (match failure with
                 | Initializer -> "initializer"
                 | Runtime -> "runtime"
                 | Budget -> "budget") )
          ]
    ;;

    let task_retention_text = function
      | Not_a_task -> "not_a_task"
      | Retained -> "retained"
      | Limit_reached -> "limit_reached"
    ;;

    let to_json t =
      `Object
        ([ "outcome", outcome_json t.outcome
         ; "binding_count", C.integer_json t.binding_count
         ; "task_retention", `String (task_retention_text t.task_retention)
         ; ("text_limited", if t.text_limited then `True else `False)
         ]
         @ C.optional_field "value_preview" t.value_preview (fun x -> `String x)
         @ C.optional_field "diagnostic" t.diagnostic Diagnostic.to_json
         @ C.optional_field "expression_type" t.expression_type Rendered_type.to_json)
    ;;

    let create
          ~limits
          ~outcome
          ~value_preview
          ~diagnostic
          ~expression_type
          ~binding_count
          ~task_retention
          ~text_limited
      =
      let t =
        { outcome
        ; value_preview
        ; diagnostic
        ; expression_type
        ; binding_count
        ; task_retention
        ; text_limited
        }
      in
      if binding_count < 0 || binding_count > Authoring_codec.maximum_wire_integer
      then C.invalid "invalid REPL binding count"
      else
        let* () =
          match value_preview with
          | None -> Ok ()
          | Some value when String.length value <= Limits.max_output_bytes limits -> Ok ()
          | Some _ -> C.limit "REPL preview exceeds output bound"
        in
        let* () =
          match diagnostic with
          | None -> Ok ()
          | Some diagnostic ->
            Diagnostic.of_json ~limits (Diagnostic.to_json diagnostic)
            |> Result.map ~f:ignore
        in
        let* () =
          match expression_type with
          | None -> Ok ()
          | Some rendered ->
            Rendered_type.of_json ~limits (Rendered_type.to_json rendered)
            |> Result.map ~f:ignore
        in
        Result.map
          (C.validate ~limits:(Limits.result limits) (to_json t))
          ~f:(fun () -> t)
    ;;

    let outcome t = t.outcome
    let value_preview t = t.value_preview
    let diagnostic t = t.diagnostic
    let expression_type t = t.expression_type
    let binding_count t = t.binding_count
    let task_retention t = t.task_retention
    let text_limited t = t.text_limited

    let of_json ~limits json =
      let* fields =
        C.fields
          ~allowed:
            [ "outcome"
            ; "value_preview"
            ; "diagnostic"
            ; "expression_type"
            ; "binding_count"
            ; "task_retention"
            ; "text_limited"
            ]
          json
      in
      let* outcome =
        get fields "outcome" (fun json ->
          let* fields = C.fields ~allowed:[ "kind"; "reason" ] json in
          let* kind = get fields "kind" (C.string ~max_bytes:32) in
          match kind with
          | "succeeded" ->
            let* () = C.fields ~allowed:[ "kind" ] json |> Result.map ~f:ignore in
            Ok Succeeded
          | "failed" ->
            let* reason = get fields "reason" (C.string ~max_bytes:32) in
            (match reason with
             | "initializer" -> Ok (Failed Initializer)
             | "runtime" -> Ok (Failed Runtime)
             | "budget" -> Ok (Failed Budget)
             | _ -> C.invalid "unknown REPL failure reason")
          | _ -> C.invalid "unknown REPL outcome")
      in
      let* value_preview =
        optional
          fields
          "value_preview"
          (C.string ~max_bytes:(Limits.max_output_bytes limits))
      in
      let* diagnostic = optional fields "diagnostic" (Diagnostic.of_json ~limits) in
      let* expression_type =
        optional fields "expression_type" (Rendered_type.of_json ~limits)
      in
      let* binding_count = get fields "binding_count" C.nonnegative in
      let* task_retention =
        get fields "task_retention" (fun json ->
          let* value = C.string ~max_bytes:32 json in
          match value with
          | "not_a_task" -> Ok Not_a_task
          | "retained" -> Ok Retained
          | "limit_reached" -> Ok Limit_reached
          | _ -> C.invalid "unknown REPL Task retention")
      in
      let* text_limited = get fields "text_limited" C.boolean in
      create
        ~limits
        ~outcome
        ~value_preview
        ~diagnostic
        ~expression_type
        ~binding_count
        ~task_retention
        ~text_limited
    ;;
  end

  type need_more_reason =
    | Parser_eof
    | Comment_eof
    | String_eof

  type status =
    | Need_more of need_more_reason * Location.t option
    | Rejected of Diagnostic.t
    | Finished
    | Stopped of Diagnostic.t option

  type details =
    | Available
    | Output_limited

  type t =
    { status : status
    ; details : details
    ; completed_statements : int
    ; statement_count : int
    ; epoch : Wire_id.Epoch.t
    ; revision : Wire_id.Revision.t
    ; summaries : Summary.t list
    ; task_reference_count : int
    ; task_references : string list
    }

  let status_json = function
    | Need_more (reason, location) ->
      `Object
        ([ "kind", `String "need_more"
         ; ( "reason"
           , `String
               (match reason with
                | Parser_eof -> "parser_eof"
                | Comment_eof -> "comment_eof"
                | String_eof -> "string_eof") )
         ]
         @ C.optional_field "location" location Location.to_json)
    | Rejected diagnostic ->
      `Object [ "kind", `String "rejected"; "diagnostic", Diagnostic.to_json diagnostic ]
    | Finished -> `Object [ "kind", `String "finished" ]
    | Stopped diagnostic ->
      `Object
        ([ "kind", `String "stopped" ]
         @ C.optional_field "diagnostic" diagnostic Diagnostic.to_json)
  ;;

  let to_json t =
    `Object
      [ "status", status_json t.status
      ; ( "details"
        , `String
            (match t.details with
             | Available -> "available"
             | Output_limited -> "output_limited") )
      ; "completed_statements", C.integer_json t.completed_statements
      ; "statement_count", C.integer_json t.statement_count
      ; "epoch", `String (Wire_id.Epoch.to_string t.epoch)
      ; "revision", `String (Wire_id.Revision.to_string t.revision)
      ; "summaries", `Array (List.map t.summaries ~f:Summary.to_json)
      ; "task_reference_count", C.integer_json t.task_reference_count
      ; "task_references", `Array (List.map t.task_references ~f:(fun x -> `String x))
      ]
  ;;

  let create
        ~limits
        ~status
        ~details
        ~completed_statements
        ~statement_count
        ~epoch
        ~revision
        ~summaries
        ~task_reference_count
        ~task_references
    =
    let t =
      { status
      ; details
      ; completed_statements
      ; statement_count
      ; epoch
      ; revision
      ; summaries
      ; task_reference_count
      ; task_references
      }
    in
    if
      completed_statements < 0
      || statement_count < completed_statements
      || statement_count > Authoring_codec.maximum_wire_integer
      || task_reference_count < List.length task_references
      || task_reference_count > Authoring_codec.maximum_wire_integer
      || List.length summaries > statement_count
      || List.length summaries > max_summaries limits
      || List.length task_references > Limits.max_page_items limits
      || List.exists task_references ~f:(fun x ->
        String.is_empty x || String.length x > Limits.max_reference_bytes limits)
    then C.invalid "invalid REPL submission dimensions"
    else
      let* () =
        match status with
        | Need_more (_, Some location) ->
          Location.of_json ~limits (Location.to_json location) |> Result.map ~f:ignore
        | Rejected diagnostic | Stopped (Some diagnostic) ->
          Diagnostic.of_json ~limits (Diagnostic.to_json diagnostic)
          |> Result.map ~f:ignore
        | Need_more (_, None) | Finished | Stopped None -> Ok ()
      in
      let* () =
        Result.all
          (List.map summaries ~f:(fun summary ->
             Summary.of_json ~limits (Summary.to_json summary)))
        |> Result.map ~f:ignore
      in
      Result.map (C.validate ~limits:(Limits.result limits) (to_json t)) ~f:(fun () -> t)
  ;;

  let epoch t = t.epoch
  let revision t = t.revision
  let status t = t.status
  let details t = t.details
  let completed_statements t = t.completed_statements
  let statement_count t = t.statement_count
  let summaries t = t.summaries
  let task_reference_count t = t.task_reference_count
  let task_references t = t.task_references

  let status_of_json ~limits json =
    let* fields = C.fields ~allowed:[ "kind"; "reason"; "location"; "diagnostic" ] json in
    let* kind = get fields "kind" (C.string ~max_bytes:32) in
    match kind with
    | "need_more" ->
      let* () =
        C.fields ~allowed:[ "kind"; "reason"; "location" ] json |> Result.map ~f:ignore
      in
      let* reason = get fields "reason" (C.string ~max_bytes:32) in
      let* reason =
        match reason with
        | "parser_eof" -> Ok Parser_eof
        | "comment_eof" -> Ok Comment_eof
        | "string_eof" -> Ok String_eof
        | _ -> C.invalid "unknown REPL incomplete reason"
      in
      let* location = optional fields "location" (Location.of_json ~limits) in
      Ok (Need_more (reason, location))
    | "rejected" ->
      let* () = C.fields ~allowed:[ "kind"; "diagnostic" ] json |> Result.map ~f:ignore in
      let* diagnostic = get fields "diagnostic" (Diagnostic.of_json ~limits) in
      Ok (Rejected diagnostic)
    | "finished" ->
      let* () = C.fields ~allowed:[ "kind" ] json |> Result.map ~f:ignore in
      Ok Finished
    | "stopped" ->
      let* () = C.fields ~allowed:[ "kind"; "diagnostic" ] json |> Result.map ~f:ignore in
      let* diagnostic = optional fields "diagnostic" (Diagnostic.of_json ~limits) in
      Ok (Stopped diagnostic)
    | _ -> C.invalid "unknown REPL submission status"
  ;;

  let of_json ~limits json =
    let* () = C.validate ~limits:(Limits.result limits) json in
    let* fields =
      C.fields
        ~allowed:
          [ "status"
          ; "details"
          ; "completed_statements"
          ; "statement_count"
          ; "epoch"
          ; "revision"
          ; "summaries"
          ; "task_reference_count"
          ; "task_references"
          ]
        json
    in
    let* status = get fields "status" (status_of_json ~limits) in
    let* details =
      get fields "details" (fun json ->
        let* value = C.string ~max_bytes:32 json in
        match value with
        | "available" -> Ok Available
        | "output_limited" -> Ok Output_limited
        | _ -> C.invalid "unknown REPL detail status")
    in
    let* completed_statements = get fields "completed_statements" C.nonnegative in
    let* statement_count = get fields "statement_count" C.nonnegative in
    let* epoch =
      get fields "epoch" (fun json ->
        let* value = C.string ~max_bytes:96 json in
        Wire_id.Epoch.of_string value)
    in
    let* revision =
      get fields "revision" (fun json ->
        let* value = C.string ~max_bytes:96 json in
        Wire_id.Revision.of_string value)
    in
    let* summaries =
      get
        fields
        "summaries"
        (C.array ~max_items:(max_summaries limits) (Summary.of_json ~limits))
    in
    let* task_reference_count = get fields "task_reference_count" C.nonnegative in
    let* task_references =
      get
        fields
        "task_references"
        (C.array
           ~max_items:(Limits.max_page_items limits)
           (C.string ~max_bytes:(Limits.max_reference_bytes limits)))
    in
    create
      ~limits
      ~status
      ~details
      ~completed_statements
      ~statement_count
      ~epoch
      ~revision
      ~summaries
      ~task_reference_count
      ~task_references
  ;;
end

module Type_inspection = struct
  type t =
    { revision : Wire_id.Revision.t
    ; binding_name : string option
    ; rendered : Submission.Rendered_type.t
    }

  let to_json t =
    `Object
      ([ "revision", `String (Wire_id.Revision.to_string t.revision)
       ; "rendered", Submission.Rendered_type.to_json t.rendered
       ]
       @ C.optional_field "binding_name" t.binding_name (fun x -> `String x))
  ;;

  let create ~limits ~revision ~binding_name ~rendered =
    let t = { revision; binding_name; rendered } in
    let* () =
      Submission.Rendered_type.of_json ~limits (Submission.Rendered_type.to_json rendered)
      |> Result.map ~f:ignore
    in
    let* () =
      match binding_name with
      | None -> Ok ()
      | Some name when String.length name <= Limits.max_output_bytes limits -> Ok ()
      | Some _ -> C.limit "REPL binding name exceeds output bound"
    in
    Result.map (C.validate ~limits:(Limits.result limits) (to_json t)) ~f:(fun () -> t)
  ;;

  let revision t = t.revision
  let binding_name t = t.binding_name
  let rendered t = t.rendered

  let of_json ~limits json =
    let* () = C.validate ~limits:(Limits.result limits) json in
    let* fields = C.fields ~allowed:[ "revision"; "binding_name"; "rendered" ] json in
    let* revision =
      get fields "revision" (fun json ->
        let* value = C.string ~max_bytes:96 json in
        Wire_id.Revision.of_string value)
    in
    let* binding_name =
      optional
        fields
        "binding_name"
        (C.string ~max_bytes:(Limits.max_output_bytes limits))
    in
    let* rendered = get fields "rendered" (Submission.Rendered_type.of_json ~limits) in
    create ~limits ~revision ~binding_name ~rendered
  ;;
end

module Reference_page = struct
  type t =
    { items : string list
    ; next : string option
    ; total : int
    }

  let to_json t =
    `Object
      ([ "items", `Array (List.map t.items ~f:(fun x -> `String x))
       ; "total", C.integer_json t.total
       ]
       @ C.optional_field "next" t.next (fun x -> `String x))
  ;;

  let create ~limits ~items ~next ~total =
    let t = { items; next; total } in
    if
      total < List.length items
      || total > Authoring_codec.maximum_wire_integer
      || List.length items > Limits.max_page_items limits
      || List.exists items ~f:(fun x ->
        String.is_empty x || String.length x > Limits.max_reference_bytes limits)
      ||
      match next with
      | None -> false
      | Some cursor ->
        not (Option.value_map (List.last items) ~default:false ~f:(String.equal cursor))
    then C.invalid "invalid REPL reference page"
    else
      let* () = C.validate ~limits:(Limits.page limits) (to_json t) in
      Result.map
        (C.validate ~limits:(Limits.response limits) (to_json t))
        ~f:(fun () -> t)
  ;;

  let items t = t.items
  let next t = t.next
  let total t = t.total

  let of_json ~limits json =
    let* () = C.validate ~limits:(Limits.page limits) json in
    let* fields = C.fields ~allowed:[ "items"; "next"; "total" ] json in
    let* items =
      get
        fields
        "items"
        (C.array
           ~max_items:(Limits.max_page_items limits)
           (C.string ~max_bytes:(Limits.max_reference_bytes limits)))
    in
    let* next =
      optional fields "next" (C.string ~max_bytes:(Limits.max_reference_bytes limits))
    in
    let* total = get fields "total" C.nonnegative in
    create ~limits ~items ~next ~total
  ;;
end
