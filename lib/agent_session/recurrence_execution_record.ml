open! Core
module P = Agent_protocol
module D = Document_schema
module X = Persistence_codec
module Request = Recurrence_execution_attempt
module Resolution = Recurrence_retry_resolution
module Origin = Recurrence_run_origin
module Reference = Recurrence_run_reference

type phase =
  | Unresolved
  | Resolved of Resolution.t
  | Admitted of
      { origin : Origin.t
      ; terminal : Reference.t option
      }

type t =
  { request : Request.t
  ; rejection : P.Error.code option
  ; uncertainty : P.Error.code option
  ; phase : phase
  ; json : Jsonaf.t
  }

let request t = t.request
let rejection t = t.rejection
let uncertainty t = t.uncertainty
let to_json t = t.json
let invalid message = Error (P.Error.invalid_request message)

let retry_resolution t =
  match t.phase with
  | Resolved r -> Some r
  | Unresolved | Admitted _ -> None
;;

let origin t =
  match t.phase with
  | Admitted a -> Some a.origin
  | Unresolved | Resolved _ -> None
;;

let terminal t =
  match t.phase with
  | Admitted a -> a.terminal
  | Unresolved | Resolved _ -> None
;;

let is_resolved t =
  match t.phase with
  | Resolved _ -> true
  | Unresolved | Admitted _ -> false
;;

let known =
  [ "request"
  ; "rejection"
  ; "uncertainty"
  ; "phase"
  ; "retry_resolution"
  ; "origin"
  ; "terminal"
  ]
;;

let set json name value =
  match json with
  | `Object fields ->
    if List.Assoc.mem fields name ~equal:String.equal
    then
      `Object
        (List.map fields ~f:(fun (key, old) ->
           key, if String.equal key name then value else old))
    else `Object (fields @ [ name, value ])
  | _ -> assert false
;;

let unknown json =
  match json with
  | `Object fields ->
    `Object
      (List.filter fields ~f:(fun (name, _) ->
         not (List.mem known name ~equal:String.equal)))
  | _ -> assert false
;;

let code_json code = `String (P.Error.code_to_string code)

let retains_unknown previous incoming =
  match unknown previous, unknown incoming with
  | `Object old, `Object next ->
    List.for_all old ~f:(fun (name, value) ->
      Option.value_map
        (List.Assoc.find next name ~equal:String.equal)
        ~default:false
        ~f:(D.Json.equal value))
  | _ -> assert false
;;

let validate_code code =
  let encoded = P.Error.code_to_string code in
  if
    String.length encoded <= 128
    && String.for_all encoded ~f:(function
      | 'a' .. 'z' | '_' -> true
      | _ -> false)
  then Ok ()
  else invalid "execution audit code exceeds its ASCII wire profile"
;;

let code_of_json json =
  let open Result.Let_syntax in
  let%bind text = P.Json_codec.string json in
  let%bind code = P.Error.code_of_string text in
  let%map () = validate_code code in
  code
;;

let of_json json ~intent =
  let open Result.Let_syntax in
  let%bind fields = X.object_ json in
  let%bind request = X.required fields "request" Request.of_json in
  let%bind () = Request.validate request ~intent in
  let%bind rejection = X.required fields "rejection" (X.nullable code_of_json) in
  let%bind uncertainty = X.required fields "uncertainty" (X.nullable code_of_json) in
  let%bind phase = X.required fields "phase" P.Json_codec.string in
  let%bind resolution =
    X.required fields "retry_resolution" (X.nullable Resolution.of_json)
  in
  let%bind origin = X.required fields "origin" (X.nullable Origin.of_json) in
  let%bind terminal = X.required fields "terminal" (X.nullable Reference.of_json) in
  let%map phase =
    match phase, resolution, origin, terminal with
    | "unresolved", None, None, None -> Ok Unresolved
    | "resolved", Some resolution, None, None ->
      let%map () = Resolution.validate resolution ~intent ~attempt:request in
      Resolved resolution
    | "admitted", None, Some origin, terminal ->
      let%bind () = Origin.validate origin ~intent ~attempt:request in
      let%map () =
        match terminal with
        | None -> Ok ()
        | Some reference -> Reference.validate_origin reference ~origin
      in
      Admitted { origin; terminal }
    | _ -> invalid "execution attempt phase contradicts its retained evidence"
  in
  { request; rejection; uncertainty; phase; json }
;;

let create ~intent request =
  of_json
    (`Object
        [ "request", Request.to_json request
        ; "rejection", `Null
        ; "uncertainty", `Null
        ; "phase", `String "unresolved"
        ; "retry_resolution", `Null
        ; "origin", `Null
        ; "terminal", `Null
        ])
    ~intent
;;

let record_code t ~name ~existing ~code ~update =
  let open Result.Let_syntax in
  let%bind () = validate_code code in
  match existing with
  | Some old when P.Error.equal_code old code -> Ok t
  | Some _ -> invalid "immutable execution audit code changed"
  | None ->
    (match t.phase with
     | Resolved _ | Admitted _ -> invalid "execution attempt is no longer unresolved"
     | Unresolved -> Ok (update { t with json = set t.json name (code_json code) }))
;;

let record_rejection t ~code =
  record_code t ~name:"rejection" ~existing:t.rejection ~code ~update:(fun t ->
    { t with rejection = Some code })
;;

let mark_uncertain t ~code =
  record_code t ~name:"uncertainty" ~existing:t.uncertainty ~code ~update:(fun t ->
    { t with uncertainty = Some code })
;;

let resolve_retry t ~intent resolution =
  let open Result.Let_syntax in
  let%bind () = Resolution.validate resolution ~intent ~attempt:t.request in
  match t.phase with
  | Resolved old when Resolution.equal old resolution -> Ok t
  | Resolved _ | Admitted _ ->
    invalid "attempt already has contradictory resolution or admission"
  | Unresolved ->
    Ok
      { t with
        phase = Resolved resolution
      ; json =
          set
            (set t.json "phase" (`String "resolved"))
            "retry_resolution"
            (Resolution.to_json resolution)
      }
;;

let associate t ~intent origin =
  let open Result.Let_syntax in
  let%bind () = Origin.validate origin ~intent ~attempt:t.request in
  match t.phase with
  | Admitted old when Origin.equal old.origin origin -> Ok t
  | Resolved _ | Admitted _ ->
    invalid "attempt already has contradictory resolution or admission"
  | Unresolved ->
    Ok
      { t with
        phase = Admitted { origin; terminal = None }
      ; json =
          set (set t.json "phase" (`String "admitted")) "origin" (Origin.to_json origin)
      }
;;

let settle t reference =
  let open Result.Let_syntax in
  match t.phase with
  | Unresolved | Resolved _ ->
    invalid "terminal reference requires the current admitted attempt"
  | Admitted admitted ->
    let%bind () = Reference.validate_origin reference ~origin:admitted.origin in
    (match admitted.terminal with
     | Some old when Reference.equal old reference -> Ok t
     | Some _ -> invalid "immutable execution terminal reference changed"
     | None ->
       Ok
         { t with
           phase = Admitted { admitted with terminal = Some reference }
         ; json = set t.json "terminal" (Reference.to_json reference)
         })
;;

let validate_transition t ~incoming =
  let retained equal old next =
    match old, next with
    | None, _ -> true
    | Some old, Some next -> equal old next
    | Some _, None -> false
  in
  let audit_added =
    (Option.is_none t.rejection && Option.is_some incoming.rejection)
    || (Option.is_none t.uncertainty && Option.is_some incoming.uncertainty)
  in
  let phase_valid =
    match t.phase, incoming.phase with
    | Unresolved, _ -> true
    | Resolved old, Resolved next -> Resolution.equal old next && not audit_added
    | Admitted old, Admitted next ->
      Origin.equal old.origin next.origin
      && retained Reference.equal old.terminal next.terminal
      && not audit_added
    | Resolved _, (Unresolved | Admitted _) | Admitted _, (Unresolved | Resolved _) ->
      false
  in
  if
    Request.equal t.request incoming.request
    && retains_unknown t.json incoming.json
    && retained P.Error.equal_code t.rejection incoming.rejection
    && retained P.Error.equal_code t.uncertainty incoming.uncertainty
    && phase_valid
  then Ok ()
  else invalid "execution attempt immutable evidence or compatible metadata changed"
;;

let quota limits =
  Capacity_growth.create
    ~bytes:(D.Limits.max_bytes limits)
    ~fields:(D.Limits.max_fields limits)
    ~nodes:(D.Limits.max_nodes limits)
;;

let reserved_growth t =
  let open Result.Let_syntax in
  match t.phase with
  | Resolved _ | Admitted { terminal = Some _; _ } -> Ok Capacity_growth.zero
  | Admitted { terminal = None; _ } -> quota Reference.limits
  | Unresolved ->
    let future_audit =
      List.fold
        [ "rejection", t.rejection; "uncertainty", t.uncertainty ]
        ~init:t.json
        ~f:(fun json (name, code) ->
          if Option.is_some code
          then json
          else set json name (`String (String.make 128 'x')))
    in
    let%bind before = Capacity_growth.measure t.json ~limits:D.Limits.default in
    let%bind after = Capacity_growth.measure future_audit ~limits:D.Limits.default in
    let audit = Capacity_growth.positive_difference after ~previous:before in
    let%bind origin = quota Origin.limits in
    let%bind terminal = quota Reference.limits in
    let%bind admitted = Capacity_growth.add origin terminal in
    let%bind resolved = quota Resolution.limits in
    (* Null slots and phase shrink are never credited. Complete quotas include
       child envelopes and unknowns; the larger mutually exclusive path wins. *)
    Capacity_growth.add audit (Capacity_growth.maximum admitted resolved)
;;

let capacity_projection t =
  match t.phase with
  | Resolved _ | Admitted { terminal = Some _; _ } -> t.json
  | Admitted { terminal = None; _ } ->
    set t.json "terminal" (Recurrence_execution_capacity.profile Reference.limits)
  | Unresolved ->
    List.fold
      [ "rejection", t.rejection; "uncertainty", t.uncertainty ]
      ~init:t.json
      ~f:(fun json (name, code) ->
        if Option.is_some code
        then json
        else set json name (`String (String.make 128 'x')))
    |> fun json ->
    (* Origin plus Terminal dominates the mutually exclusive Resolution profile
       in bytes, fields, nodes and depth. No absent-slot shrink is spent. *)
    set json "origin" (Recurrence_execution_capacity.profile Origin.limits)
    |> fun json ->
    set json "terminal" (Recurrence_execution_capacity.profile Reference.limits)
;;
