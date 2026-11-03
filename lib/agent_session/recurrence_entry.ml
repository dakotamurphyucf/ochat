open! Core
module P = Agent_protocol
module D = Document_schema
module X = Persistence_codec
module C = D.Extension_carrier
module I = Recurrence_identity
module F = Recurrence_definition

module Pause = struct
  type t =
    | Operator_pause
    | Controller_stop
    | Source_review
    | Recovery_review
  [@@deriving equal, sexp_of]

  let to_string = function
    | Operator_pause -> "operator_pause"
    | Controller_stop -> "controller_stop"
    | Source_review -> "source_review"
    | Recovery_review -> "recovery_review"
  ;;

  let of_json =
    P.Json_codec.enum
      ~name:"recurrence pause"
      [ "operator_pause", Operator_pause
      ; "controller_stop", Controller_stop
      ; "source_review", Source_review
      ; "recovery_review", Recovery_review
      ]
  ;;
end

module Retirement = struct
  type t =
    | Operator_retire
    | Lifecycle_changed
  [@@deriving equal, sexp_of]

  let to_string = function
    | Operator_retire -> "operator_retire"
    | Lifecycle_changed -> "lifecycle_changed"
  ;;

  let of_json =
    P.Json_codec.enum
      ~name:"recurrence retirement"
      [ "operator_retire", Operator_retire; "lifecycle_changed", Lifecycle_changed ]
  ;;
end

type control =
  | Paused of Pause.t
  | Enabled
  | Retired of Retirement.t
[@@deriving equal, sexp_of]

module Stored_frontier = struct
  type t =
    { value : Recurrence_frontier.t
    ; encoded : Jsonaf.t
    }

  let create value =
    Recurrence_frontier.to_document value
    |> Result.map_error ~f:(fun e ->
      P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t e)))
    |> Result.map ~f:(fun document -> { value; encoded = D.Document.json document })
  ;;

  let of_json encoded =
    let convert result =
      Result.map_error result ~f:(fun e ->
        P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t e)))
    in
    let open Result.Let_syntax in
    let%bind document =
      D.Document.inspect ~limits:Recurrence_frontier.limits encoded |> convert
    in
    let%map value = Recurrence_frontier.of_document document |> convert in
    { value; encoded }
  ;;
end

type projection =
  { definitions : F.t list
  ; control : control
  ; control_revision : int64
  ; cursor : Recurrence_cursor.t
  ; frontier : Stored_frontier.t option
  }

type t = projection C.t

let definitions t = (C.value t).definitions
let current_projection p = List.last_exn p.definitions
let current t = current_projection (C.value t)
let id t = I.Definition.recurrence_id (F.key (current t))
let controller t = I.Definition.controller (F.key (current t))
let control t = (C.value t).control
let control_revision t = (C.value t).control_revision
let cursor t = (C.value t).cursor

let frontier t =
  match (C.value t).frontier with
  | Some frontier -> Ok frontier.value
  | None ->
    Error
      (P.Error.invalid_request
         "historical recurrence lacks planning frontier; create a new identity")
;;

let invalid message = Error (P.Error.invalid_request message)

let protocol result =
  Result.map_error result ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error)))
;;

let steps = function
  | Enabled -> 2
  | Paused _ -> 1
  | Retired _ -> 0
;;

let required_transition_steps t = steps (control t)

let limits =
  D.Limits.create
    ~max_bytes:(28 * 1024 * 1024)
    ~max_depth:192
    ~max_fields:131072
    ~max_nodes:262144
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let control_json = function
  | Enabled -> `Object [ "kind", `String "enabled"; "reason", `Null ]
  | Paused reason ->
    `Object [ "kind", `String "paused"; "reason", `String (Pause.to_string reason) ]
  | Retired reason ->
    `Object [ "kind", `String "retired"; "reason", `String (Retirement.to_string reason) ]
;;

let control_of_json json =
  let open Result.Let_syntax in
  let%bind f = X.object_ json in
  let%bind kind = X.required f "kind" P.Json_codec.string in
  match kind with
  | "enabled" ->
    let%bind reason = P.Json_codec.required f "reason" in
    (match reason with
     | `Null -> Ok Enabled
     | _ -> invalid "enabled recurrence has a pause reason")
  | "paused" ->
    let%map reason = X.required f "reason" Pause.of_json in
    Paused reason
  | "retired" ->
    let%map reason = X.required f "reason" Retirement.of_json in
    Retired reason
  | _ -> invalid "unsupported recurrence control variant"
;;

let raw_json p =
  `Object
    [ ( "definitions"
      , `Array
          (List.map p.definitions ~f:(fun definition ->
             `Object
               [ "revision", X.int64_json (I.Definition.revision (F.key definition))
               ; "value", D.Document.json (F.to_document definition)
               ])) )
    ; "control", control_json p.control
    ; "control_revision", X.int64_json p.control_revision
    ; "cursor", Recurrence_cursor.to_json p.cursor
    ; "frontier", X.option_json (fun f -> f.Stored_frontier.encoded) p.frontier
    ]
;;

let validate_projection p =
  let open Result.Let_syntax in
  let%bind first =
    match p.definitions with
    | [] -> invalid "recurrence has no captured definition"
    | first :: _ -> Ok first
  in
  let key = F.key first in
  let%bind _ =
    List.fold_result p.definitions ~init:(0, None) ~f:(fun (count, previous) definition ->
      let candidate = F.key definition in
      let source = F.source definition in
      let source_valid =
        Option.for_all previous ~f:(fun previous ->
          let old = F.source previous in
          source.generation >= old.generation
          && Int64.(source.installation_epoch >= old.installation_epoch)
          && ((not (Int64.equal source.installation_epoch old.installation_epoch))
              || P.Run_source.equal source old))
      in
      if
        count >= 8
        || (not (Int64.equal (I.Definition.revision candidate) (Int64.of_int count)))
        || (not
              (P.Id.Recurrence.equal
                 (I.Definition.recurrence_id key)
                 (I.Definition.recurrence_id candidate)))
        || (not
              (P.Session_ref.equal
                 (I.Definition.controller key)
                 (I.Definition.controller candidate)))
        || (not (P.Id.Principal.equal (F.creator first) (F.creator definition)))
        || (not
              (P.Session_lifecycle.Revision.equal
                 (F.birth_lifecycle_revision first)
                 (F.birth_lifecycle_revision definition)))
        || not source_valid
      then invalid "recurrence immutable revision history is inconsistent"
      else Ok (count + 1, Some definition))
  in
  let current = current_projection p in
  let%bind () =
    if
      Int64.(
        p.control_revision < 0L
        || p.control_revision > max_value - of_int (steps p.control))
      || Int64.(p.control_revision < I.Definition.revision (F.key current))
    then invalid "recurrence control has no reserved disposition frontier"
    else Ok ()
  in
  let%bind () =
    Recurrence_cursor.validate p.cursor ~rule:(F.Schedule.rule (F.schedule current))
  in
  let%bind () =
    match p.frontier with
    | None -> Ok ()
    | Some frontier ->
      let%bind () =
        Recurrence_frontier.validate
          frontier.value
          ~definitions:p.definitions
          ~cursor:p.cursor
      in
      if
        Int64.(
          Recurrence_frontier.revision frontier.value
          > max_value - of_int (steps p.control))
      then invalid "planning frontier lacks reserved control steps"
      else Ok ()
  in
  if not (I.Definition.equal (Recurrence_cursor.definition p.cursor) (F.key current))
  then invalid "recurrence cursor belongs to another definition"
  else if
    Int64.equal p.control_revision 0L
    && ((not (equal_control p.control (Paused Operator_pause)))
        || not (Recurrence_cursor.equal p.cursor (F.initial_cursor first)))
  then invalid "recurrence initial control or cursor has no creation provenance"
  else Ok ()
;;

let decode json =
  let open Result.Let_syntax in
  let%bind f = X.object_ json in
  let%bind raw =
    X.required f "definitions" (function
      | `Array values -> Ok values
      | _ -> invalid "expected recurrence definition history")
  in
  let%bind definitions =
    List.fold_result raw ~init:(0, []) ~f:(fun (count, acc) json ->
      if count >= 8
      then invalid "recurrence definition retention is saturated"
      else (
        let%bind f = X.object_ json in
        let%bind revision =
          X.required f "revision" (function
            | `String text when String.length text <= 19 ->
              X.nonnegative_int64 (`String text)
            | _ -> invalid "invalid retained recurrence revision")
        in
        let%bind definition =
          X.required f "value" (fun json ->
            let%bind document = D.Document.inspect ~limits json |> protocol in
            F.of_document document |> protocol)
        in
        if not (Int64.equal revision (I.Definition.revision (F.key definition)))
        then invalid "recurrence history identity differs from captured revision"
        else Ok (count + 1, definition :: acc)))
    |> Result.map ~f:(fun (_, values) -> List.rev values)
  in
  let%bind current =
    match List.last definitions with
    | None -> invalid "empty recurrence definition history"
    | Some current -> Ok current
  in
  let%bind control = X.required f "control" control_of_json in
  let%bind control_revision =
    X.required f "control_revision" (function
      | `String text when String.length text <= 19 -> X.nonnegative_int64 (`String text)
      | _ -> invalid "invalid recurrence control revision")
  in
  let%bind cursor =
    X.required f "cursor" (fun json ->
      Recurrence_cursor.of_json json ~rule:(F.Schedule.rule (F.schedule current)))
  in
  let%bind frontier =
    match P.Json_codec.optional f "frontier" with
    | None | Some `Null -> Ok None
    | Some json -> Result.map (Stored_frontier.of_json json) ~f:Option.some
  in
  let p = { definitions; control; control_revision; cursor; frontier } in
  let%map () = validate_projection p in
  p
;;

let shape =
  let v = D.Shape.value in
  X.shape_exn
    [ ( "definitions"
      , X.array_shape_exn
          ~identity_field:"revision"
          (X.shape_exn [ "revision", v; "value", v ]) )
    ; ( "control"
      , X.tagged_shape_exn
          ~discriminator:"kind"
          [ "enabled", X.fields_shape [ "kind"; "reason" ]
          ; "paused", X.fields_shape [ "kind"; "reason" ]
          ; "retired", X.fields_shape [ "kind"; "reason" ]
          ] )
    ; "control_revision", v
    ; "cursor", Recurrence_cursor.shape
    ; "frontier", v
    ]
;;

let codec =
  D.Domain_codec.create_validated
    ~limits
    ~kind:"ochat.recurrence_entry"
    ~version:1
    ~shape
    ~supported_semantics:[]
    ~validate:(fun p -> X.document_result (validate_projection p))
    ~decode:(fun json -> X.document_result (decode json))
    ~encode:(fun p -> Ok (raw_json p))
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

(* A private preservation/size probe is JSON accounting only: no domain value,
   receipt, currentness or executable state is constructed from it or persisted. *)
let probe_codec =
  D.Domain_codec.create
    ~limits
    ~kind:"ochat.recurrence_entry"
    ~version:1
    ~shape
    ~supported_semantics:[]
    ~decode:(fun json -> Ok json)
    ~encode:(fun json -> Ok json)
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let future_controls =
  [ Paused Pause.Operator_pause
  ; Paused Controller_stop
  ; Paused Source_review
  ; Paused Recovery_review
  ; Retired Retirement.Operator_retire
  ; Retired Lifecycle_changed
  ]
;;

let owned_control control revision =
  `Object [ "control", control_json control; "control_revision", X.int64_json revision ]
;;

let control_growth t =
  if required_transition_steps t = 0
  then Ok Capacity_growth.zero
  else
    let open Result.Let_syntax in
    let p = C.value t in
    let%bind current =
      Capacity_growth.measure (owned_control p.control p.control_revision) ~limits
    in
    let%map maximum =
      List.fold_result
        future_controls
        ~init:Capacity_growth.zero
        ~f:(fun maximum control ->
          let%map candidate =
            Capacity_growth.measure (owned_control control Int64.max_value) ~limits
          in
          Capacity_growth.maximum maximum candidate)
    in
    Capacity_growth.positive_difference maximum ~previous:current
;;

let reserved_growth t =
  let open Result.Let_syntax in
  let%bind control = control_growth t in
  let%bind frontier =
    match (C.value t).frontier with
    | None -> Ok Capacity_growth.zero
    | Some frontier -> Recurrence_frontier.reserved_growth frontier.value
  in
  Capacity_growth.add control frontier
;;

let to_document t =
  let open Result.Let_syntax in
  let%bind document = D.Domain_codec.encode codec t in
  let%bind growth = reserved_growth t |> X.document_result in
  let%bind measured =
    Capacity_growth.measure (D.Document.json document) ~limits |> X.document_result
  in
  let%bind reserved = Capacity_growth.add measured growth |> X.document_result in
  let%bind () =
    if
      Capacity_growth.encoded_bytes reserved > D.Limits.max_bytes limits
      || Capacity_growth.fields reserved > D.Limits.max_fields limits
      || Capacity_growth.nodes reserved > D.Limits.max_nodes limits
    then Error (D.Error.Limit_exceeded "recurrence control disposition reserve")
    else Ok ()
  in
  let%map () =
    if required_transition_steps t = 0
    then Ok ()
    else
      List.fold_result future_controls ~init:() ~f:(fun () control ->
        let p = C.value t in
        let json = raw_json { p with control; control_revision = Int64.max_value } in
        D.Domain_codec.encode probe_codec (C.with_value t json)
        |> Result.map ~f:(fun _ -> ()))
  in
  document
;;

let of_document document =
  let open Result.Let_syntax in
  let%bind t = D.Domain_codec.decode codec document in
  let%map _ = to_document t in
  t
;;

let admit p =
  let t = C.of_authored_value p in
  let%map.Result _ = to_document t |> protocol in
  t
;;

let create definition =
  let open Result.Let_syntax in
  let%bind frontier = Recurrence_frontier.initial definition in
  let%bind frontier = Stored_frontier.create frontier in
  admit
    { definitions = [ definition ]
    ; control = Paused Operator_pause
    ; control_revision = 0L
    ; cursor = F.initial_cursor definition
    ; frontier = Some frontier
    }
;;

let same_definition a b = String.equal (F.digest a) (F.digest b)

let same_frontier a b =
  Option.equal
    (fun a b -> D.Json.equal a.Stored_frontier.encoded b.Stored_frontier.encoded)
    a
    b
;;

let control_frontier p ~definitions ~control =
  let open Result.Let_syntax in
  match p.frontier with
  | None -> Ok None
  | Some old ->
    let current = current_projection p in
    let%bind next =
      if List.length definitions = List.length p.definitions + 1
      then
        Recurrence_frontier.adopt_definition
          old.value
          ~previous:current
          ~definition:(List.last_exn definitions)
      else (
        match control with
        | Enabled -> Ok old.value
        | Paused _ | Retired _ ->
          Recurrence_frontier.withdraw old.value ~definition:current)
    in
    Result.map (Stored_frontier.create next) ~f:Option.some
;;

let validate_transition previous ~incoming =
  let open Result.Let_syntax in
  let%bind () = validate_projection (C.value incoming) in
  let p = C.value previous
  and n = C.value incoming in
  let rec prefix old retained =
    match old, retained with
    | [], remaining -> Ok remaining
    | first :: rest, next :: remaining when same_definition first next ->
      prefix rest remaining
    | _ -> invalid "immutable recurrence definition evidence disappeared or changed"
  in
  let%bind added = prefix p.definitions n.definitions in
  let unchanged =
    List.is_empty added
    && equal_control p.control n.control
    && Recurrence_cursor.equal p.cursor n.cursor
    && same_frontier p.frontier n.frontier
  in
  let due =
    List.is_empty added
    && equal_control p.control Enabled
    && equal_control n.control Enabled
    && Int64.equal p.control_revision n.control_revision
    && not unchanged
  in
  let%bind () =
    if unchanged
    then
      if Int64.equal p.control_revision n.control_revision
      then Ok ()
      else invalid "unchanged recurrence advanced control"
    else if due
    then (
      match p.frontier, n.frontier with
      | Some old, Some next ->
        let%bind () =
          Recurrence_frontier.validate_transition
            old.value
            ~incoming:next.value
            ~definitions:n.definitions
            ~cursor:n.cursor
        in
        if
          Int64.(Recurrence_frontier.revision old.value < max_value)
          && Int64.equal
               (Recurrence_frontier.revision next.value)
               (Int64.succ (Recurrence_frontier.revision old.value))
        then Ok ()
        else invalid "due transition must advance planning frontier once"
      | None, (None | Some _) | Some _, None ->
        invalid "due transition lacks prior planning custody")
    else if
      Int64.equal p.control_revision Int64.max_value
      || not (Int64.equal n.control_revision (Int64.succ p.control_revision))
    then invalid "recurrence control revision did not advance exactly once"
    else (
      let%bind () =
        match p.control, added with
        | Retired _, _ -> invalid "retired recurrence cannot be changed"
        | (Enabled | Paused _), [] ->
          if Recurrence_cursor.equal p.cursor n.cursor
          then Ok ()
          else invalid "metadata control cannot advance recurrence cursor"
        | (Enabled | Paused _), [ definition ] ->
          if
            equal_control n.control (Paused Operator_pause)
            && Recurrence_cursor.equal n.cursor (F.initial_cursor definition)
          then Ok ()
          else invalid "edited recurrence must start paused with initial cursor"
        | (Enabled | Paused _), _ -> invalid "recurrence edit added multiple revisions"
      in
      let%bind expected =
        control_frontier p ~definitions:n.definitions ~control:n.control
      in
      if same_frontier expected n.frontier
      then Ok ()
      else invalid "control transition changed or discarded planning custody")
  in
  let%bind adopted = D.Domain_codec.adopt codec ~previous ~incoming |> protocol in
  let%bind adopted = to_document adopted |> protocol in
  let%bind incoming = to_document incoming |> protocol in
  if D.Json.equal (D.Document.json adopted) (D.Document.json incoming)
  then Ok ()
  else invalid "recurrence update discarded preserved extension fields"
;;

let change t ~definitions ~control ~cursor =
  let p = C.value t in
  if
    equal_control p.control control
    && List.equal same_definition p.definitions definitions
    && Recurrence_cursor.equal p.cursor cursor
  then Ok t
  else if Int64.equal p.control_revision Int64.max_value
  then invalid "recurrence control frontier exhausted"
  else
    let open Result.Let_syntax in
    let%bind frontier = control_frontier p ~definitions ~control in
    let candidate =
      C.with_value
        t
        { definitions
        ; control
        ; control_revision = Int64.succ p.control_revision
        ; cursor
        ; frontier
        }
    in
    let%bind () = validate_transition t ~incoming:candidate in
    let%map _ = to_document candidate |> protocol in
    candidate
;;

let apply_due t plan =
  let open Result.Let_syntax in
  let%bind frontier = frontier t in
  let%bind () =
    if
      equal_control (control t) Enabled
      && Recurrence_due_plan.matches
           plan
           ~definition:(current t)
           ~cursor:(cursor t)
           ~frontier
    then Ok ()
    else invalid "due plan is stale or recurrence is not enabled"
  in
  let%bind frontier = Stored_frontier.create (Recurrence_due_plan.frontier plan) in
  let p = C.value t in
  let candidate =
    C.with_value
      t
      { p with cursor = Recurrence_due_plan.cursor plan; frontier = Some frontier }
  in
  let%bind () = validate_transition t ~incoming:candidate in
  let%map _ = to_document candidate |> protocol in
  candidate
;;

let edit t definition =
  change
    t
    ~definitions:(definitions t @ [ definition ])
    ~control:(Paused Operator_pause)
    ~cursor:(F.initial_cursor definition)
;;

let enable t =
  match control t with
  | Retired _ -> invalid "retired recurrence cannot be enabled"
  | Enabled | Paused _ ->
    change t ~definitions:(definitions t) ~control:Enabled ~cursor:(cursor t)
;;

let pause t ~reason =
  match control t with
  | Retired _ -> invalid "retired recurrence cannot be paused"
  | Enabled | Paused _ ->
    change t ~definitions:(definitions t) ~control:(Paused reason) ~cursor:(cursor t)
;;

let retire t ~reason =
  match control t with
  | Retired _ -> Ok t
  | Enabled | Paused _ ->
    change t ~definitions:(definitions t) ~control:(Retired reason) ~cursor:(cursor t)
;;

let hold_for_review t =
  match control t with
  | Enabled ->
    let%map.Result held = pause t ~reason:Source_review in
    Some held
  | Paused _ | Retired _ -> Ok None
;;

let retire_lifecycle t =
  match control t with
  | Retired _ -> Ok None
  | Enabled | Paused _ ->
    let%map.Result retired = retire t ~reason:Lifecycle_changed in
    Some retired
;;

let effective_control t ~current_revision ~archived =
  let birth = F.birth_lifecycle_revision (current t) in
  let comparison = P.Session_lifecycle.Revision.compare birth current_revision in
  if comparison > 0
  then invalid "recurrence birth is newer than current lifecycle authority"
  else (
    match control t with
    | Retired _ as control -> Ok control
    | Enabled | Paused _ ->
      if archived || comparison < 0
      then Ok (Retired Lifecycle_changed)
      else Ok (control t))
;;

let capacity_projection t =
  let open Result.Let_syntax in
  let p = C.value t in
  let%bind frontier =
    match p.frontier with
    | None -> Ok `Null
    | Some frontier -> Recurrence_frontier.capacity_projection frontier.value
  in
  let json =
    raw_json
      { p with control = Retired Lifecycle_changed; control_revision = Int64.max_value }
  in
  let json =
    match json with
    | `Object fields ->
      `Object
        (List.map fields ~f:(fun (name, value) ->
           name, if String.equal name "frontier" then frontier else value))
    | _ -> assert false
  in
  let%map document =
    if required_transition_steps t = 0
    then D.Domain_codec.encode codec t
    else D.Domain_codec.encode probe_codec (C.with_value t json)
  in
  D.Document.json document
;;
