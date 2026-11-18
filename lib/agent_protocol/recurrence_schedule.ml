open! Core
module J = Json_codec
module D = Document_schema
module W = Recurrence_wire

type t =
  | Interval of
      { rule : Recurrence_rules.Rule.Interval.t
      ; first_due : Time_ns.Alternate_sexp.t
      }
  | Calendar of Recurrence_rules.Rule.Calendar.t
[@@deriving sexp_of]

let rule = function
  | Interval { rule; _ } -> Recurrence_rules.Rule.Interval rule
  | Calendar rule -> Recurrence_rules.Rule.Calendar rule
;;

let to_json t =
  match t with
  | Interval { first_due; _ } ->
    `Object
      [ "kind", `String "interval"
      ; "rule", Recurrence_rules.Rule.to_json (rule t)
      ; "first_due", Timestamp.to_json (Timestamp.of_time_ns first_due)
      ]
  | Calendar _ ->
    `Object [ "kind", `String "calendar"; "rule", Recurrence_rules.Rule.to_json (rule t) ]
;;

let limits =
  D.Limits.create ~max_bytes:65536 ~max_depth:24 ~max_fields:256 ~max_nodes:512
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let decode json ~public =
  let open Result.Let_syntax in
  let%bind () =
    D.Json.validate ~limits json
    |> Result.map_error ~f:(fun error ->
      Protocol_error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error)))
  in
  let%bind f = J.fields json in
  let%bind kind = J.required_as f "kind" J.string in
  let%bind rule =
    J.required_as f "rule" (fun json ->
      Recurrence_rules.Rule.of_json json
      |> Result.map_error ~f:(fun error ->
        Protocol_error.invalid_request
          (Sexp.to_string_hum (Recurrence_rules.Timing_error.sexp_of_t error))))
  in
  let%bind () =
    if not public
    then Ok ()
    else
      Extension_codec.closed
        f
        (match kind with
         | "interval" -> [ "kind"; "rule"; "first_due" ]
         | "calendar" | _ -> [ "kind"; "rule" ])
  in
  match kind, rule with
  | "interval", Recurrence_rules.Rule.Interval rule ->
    let%map first_due = J.required_as f "first_due" Timestamp.of_json in
    Interval { rule; first_due = Timestamp.to_time_ns first_due }
  | "calendar", Recurrence_rules.Rule.Calendar rule -> Ok (Calendar rule)
  | _, (Recurrence_rules.Rule.Interval _ | Recurrence_rules.Rule.Calendar _) ->
    Error (Protocol_error.invalid_request "recurrence schedule and rule kind differ")
;;

let of_json json = decode json ~public:false
let of_public_json json = decode json ~public:true

let shape =
  let v = D.Shape.value in
  W.tagged_shape_exn
    ~discriminator:"kind"
    [ "interval", W.fields_shape [ "kind"; "rule"; "first_due" ]
    ; "calendar", W.shape_exn [ "kind", v; "rule", v ]
    ]
;;
