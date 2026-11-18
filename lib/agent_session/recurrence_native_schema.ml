open! Core
module Effect = Recurrence_native_admission.Effect

let text = `Object [ "type", `String "string" ]

let integer ~min ~max =
  `Object [ "type", `String "integer"; "minimum", `Number min; "maximum", `Number max ]
;;

let enum values =
  `Object
    [ "type", `String "string"
    ; "enum", `Array (List.map values ~f:(fun value -> `String value))
    ]
;;

let object_ properties =
  `Object
    [ "type", `String "object"
    ; "properties", `Object properties
    ; "required", `Array (List.map properties ~f:(fun (name, _) -> `String name))
    ; "additionalProperties", `False
    ]
;;

let any_of schemas = `Object [ "anyOf", `Array schemas ]
let nullable schema = any_of [ schema; `Object [ "type", `String "null" ] ]

let array schema ~max =
  `Object [ "type", `String "array"; "items", schema; "maxItems", `Number max ]
;;

let revision =
  `Object
    [ "type", `String "string"
    ; "description", `String "Canonical nonnegative decimal string; no leading zeroes"
    ; "minLength", `Number "1"
    ; "maxLength", `Number "19"
    ]
;;

let generation = integer ~min:"0" ~max:(Int.to_string Int.max_value)
let session = object_ [ "server_id", text; "session_id", text ]

let source =
  object_
    [ "script_id", text
    ; "source_sha256", text
    ; "generation", generation
    ; "installation_epoch", revision
    ]
;;

let attachment =
  let common =
    [ "kind", enum [ "file"; "image"; "audio"; "binary" ]
    ; "media_type", text
    ; "byte_length", integer ~min:"0" ~max:(Int64.to_string Int64.max_value)
    ; "digest", text
    ]
  in
  (* Omission and an explicitly supplied display_name remain distinct wire cases;
     each branch is strict. No null-to-absence normalization. *)
  any_of
    (List.concat_map [ false; true ] ~f:(fun named ->
       let common = if named then common @ [ "display_name", text ] else common in
       [ object_ (common @ [ "source", enum [ "stored" ]; "blob_id", text ])
       ; object_ (common @ [ "source", enum [ "inline_base64" ]; "inline_base64", text ])
       ]))
;;

let input =
  object_
    [ "kind", enum [ "plain_text"; "chatmd" ]
    ; "text", text
    ; "attachments", array attachment ~max:"1024"
    ]
;;

let target =
  any_of
    [ object_ [ "kind", enum [ "existing_session" ]; "session", session; "input", input ]
    ; object_
        [ "kind", enum [ "authored_child" ]
        ; "name", text
        ; "mode", enum [ "single_turn"; "workflow" ]
        ]
    ]
;;

let interval_rule =
  object_
    [ "version", integer ~min:"1" ~max:"1"
    ; "kind", enum [ "interval" ]
    ; "mode", enum [ "fixed_rate"; "fixed_delay" ]
    ; "period_ms", integer ~min:"1" ~max:(Int64.to_string Int64.(max_value / 1_000_000L))
    ]
;;

let pattern =
  any_of
    [ object_ [ "kind", enum [ "daily" ] ]
    ; object_
        [ "kind", enum [ "weekdays" ]
        ; "days", array (integer ~min:"0" ~max:"6") ~max:"7"
        ]
    ; object_
        [ "kind", enum [ "month_days" ]
        ; "days", array (integer ~min:"1" ~max:"31") ~max:"31"
        ]
    ]
;;

let calendar_rule =
  object_
    [ "version", integer ~min:"1" ~max:"1"
    ; "kind", enum [ "calendar" ]
    ; "pattern", pattern
    ; "ofday", text
    ; "zone", text
    ; "start_date", text
    ; "end_date", nullable text
    ; "gap", enum [ "skip"; "at_gap_end" ]
    ; "repeat", enum [ "earlier"; "later"; "both" ]
    ]
;;

let schedule =
  any_of
    [ object_ [ "kind", enum [ "interval" ]; "rule", interval_rule; "first_due", text ]
    ; object_ [ "kind", enum [ "calendar" ]; "rule", calendar_rule ]
    ]
;;

let policy =
  object_
    [ ( "misfire"
      , any_of
          [ object_ [ "kind", enum [ "skip" ] ]
          ; object_ [ "kind", enum [ "coalesce" ] ]
          ; object_ [ "kind", enum [ "catch_up" ]; "limit", integer ~min:"1" ~max:"32" ]
          ] )
    ; "grace_ms", revision
    ; "max_pending", integer ~min:"1" ~max:"32"
    ; "max_history", integer ~min:"1" ~max:"256"
    ; "max_calendar_steps", integer ~min:"1" ~max:"4096"
    ]
;;

let basis ~create ~needs_source =
  object_
    [ "generation", generation
    ; "expected_catalog_revision", revision
    ; ( "expected_control_revision"
      , if create then `Object [ "type", `String "null" ] else revision )
    ; ("source", if needs_source then source else `Object [ "type", `String "null" ])
    ; ( "expected_lifecycle_revision"
      , if needs_source then revision else `Object [ "type", `String "null" ] )
    ; "key", text
    ]
;;

let capture = object_ [ "schedule", schedule; "target", target; "policy", policy ]

let parameters = function
  | Effect.List -> object_ []
  | Get | Status | History -> object_ [ "id", text ]
  | Pause | Cancel ->
    object_ [ "id", text; "basis", basis ~create:false ~needs_source:false ]
  | Resume -> object_ [ "id", text; "basis", basis ~create:false ~needs_source:true ]
  | Create ->
    object_
      [ "id", text; "basis", basis ~create:true ~needs_source:true; "capture", capture ]
  | Edit ->
    object_
      [ "id", text; "basis", basis ~create:false ~needs_source:true; "capture", capture ]
;;
