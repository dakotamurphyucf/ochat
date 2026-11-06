open! Core
module J = Json_codec
module D = Document_schema

(* Input envelope12288 plus64 bytes for the closed canonical action wrapper. *)
let max_bytes = Run_limits.max_document_bytes + 12288 + 64
let max_depth = Run_limits.max_depth + 3

type t =
  | Start of Goal_native_start_intent.t
  | Resume of Goal_native_resume_intent.t
[@@deriving equal]

let to_json = function
  | Start intent ->
    `Object [ "kind", `String "start"; "intent", Goal_native_start_intent.to_json intent ]
  | Resume intent ->
    `Object
      [ "kind", `String "resume"; "intent", Goal_native_resume_intent.to_json intent ]
;;

let of_json json =
  let open Result.Let_syntax in
  (* Resume retains its old8192-byte payload ceiling inside the new wrapper.
     Larger Start bounds do not enlarge Resume's own validation domain. *)
  let%bind () = Extension_codec.validate_json ~max_bytes ~max_depth json in
  let%bind fields = J.fields json in
  let%bind () = Extension_codec.closed fields [ "kind"; "intent" ] in
  match%bind J.required_as fields "kind" J.string with
  | "start" ->
    let%map intent = J.required_as fields "intent" Goal_native_start_intent.of_json in
    Start intent
  | "resume" ->
    let%map intent = J.required_as fields "intent" Goal_native_resume_intent.of_json in
    Resume intent
  | _ -> Error (Protocol_error.invalid_request "unsupported native Goal action kind")
;;

let validate_update t ~incoming =
  match t, incoming with
  | Start previous, Start incoming ->
    Goal_native_start_intent.validate_update previous ~incoming
  | Resume previous, Resume incoming ->
    Goal_native_resume_intent.validate_update previous ~incoming
  | Start _, Resume _ | Resume _, Start _ ->
    Error (Protocol_error.invalid_request "native Goal action family changed")
;;

let persistence_semantics = function
  | Start _ -> Goal_native_start_intent.persistence_semantics
  | Resume _ -> Goal_native_resume_intent.persistence_semantics
;;

let shape =
  let shape_exn = function
    | Ok shape -> shape
    | Error error -> raise_s [%sexp "invalid native Goal shape", (error : D.Error.t)]
  in
  let object_ fields = D.Shape.object_ fields |> shape_exn in
  let phase ~start =
    let case fields = object_ (("kind", D.Shape.value) :: fields) in
    D.Shape.tagged_object
      ~discriminator:"kind"
      ([ "staged", case []
       ; "accepted", case [ "accepted_at", D.Shape.value ]
       ; ( "dispatch_claimed"
         , case [ "accepted_at", D.Shape.value; "claimed_at", D.Shape.value ] )
       ; "discarded", case [ "reason", D.Shape.value ]
       ; "rejected", case [ "error", D.Shape.value ]
       ]
       @
       if start
       then
         [ ( "admitted"
           , case
               [ "accepted_at", D.Shape.value
               ; "claimed_at", D.Shape.value
               ; "admission", D.Shape.value
               ] )
         ]
       else [])
    |> shape_exn
  in
  let case ~start =
    object_
      [ "kind", D.Shape.value
      ; ( "intent"
        , object_
            [ "request", D.Shape.value
            ; "principal_id", D.Shape.value
            ; "phase", phase ~start
            ] )
      ]
  in
  D.Shape.tagged_object
    ~discriminator:"kind"
    [ "start", case ~start:true; "resume", case ~start:false ]
  |> shape_exn
;;

let with_phase action phase =
  let kind, request, principal_id =
    match action with
    | Start intent ->
      ( "start"
      , Goal_native_start_request.to_json (Goal_native_start_intent.request intent)
      , Goal_native_start_intent.principal_id intent )
    | Resume intent ->
      ( "resume"
      , Goal_resume_request.to_json (Goal_native_resume_intent.request intent)
      , Goal_native_resume_intent.principal_id intent )
  in
  `Object
    [ "kind", `String kind
    ; ( "intent"
      , `Object
          [ "request", request
          ; "principal_id", Id.Principal.to_json principal_id
          ; "phase", phase
          ] )
    ]
;;

let capacity_projections action =
  let terminal, accepted_at, claimed_at, admission =
    let time = `String (String.make 32 '9') in
    match action with
    | Start intent ->
      let terminal, accepted_at, claimed_at =
        match Goal_native_start_intent.phase intent with
        | Staged -> false, time, time
        | Accepted at -> false, Timestamp.to_json at, time
        | Dispatch_claimed { accepted_at; claimed_at } ->
          false, Timestamp.to_json accepted_at, Timestamp.to_json claimed_at
        | Admitted _ | Discarded _ | Rejected _ -> true, time, time
      in
      let request = Goal_native_start_intent.request intent in
      let receipt =
        `Object
          [ "run_id", `String ("run_" ^ String.make 92 'x')
          ; ( "principal_id"
            , Id.Principal.to_json (Goal_native_start_intent.principal_id intent) )
          ; "source", Run_source.to_json (Goal_native_start_request.source request)
          ; "key", Idempotency_key.to_json (Goal_native_start_request.key request)
          ; "request_sha256", `String (String.make 64 'f')
          ; "kind", `String "admission"
          ; "run_revision", `String "0"
          ; "session_revision", `String (Int64.to_string Int64.max_value)
          ; "committed_at", time
          ]
      in
      terminal, accepted_at, claimed_at, Some receipt
    | Resume intent ->
      let terminal, accepted_at, claimed_at =
        match Goal_native_resume_intent.phase intent with
        | Staged -> false, time, time
        | Accepted at -> false, Timestamp.to_json at, time
        | Dispatch_claimed { accepted_at; claimed_at } ->
          false, Timestamp.to_json accepted_at, Timestamp.to_json claimed_at
        | Discarded _ | Rejected _ -> true, time, time
      in
      terminal, accepted_at, claimed_at, None
  in
  if terminal
  then []
  else (
    let start_phase_reserve_bytes = 6144 in
    let resume_phase_reserve_bytes = 3072 in
    let reserve =
      if Option.is_some admission
      then start_phase_reserve_bytes
      else resume_phase_reserve_bytes
    in
    (* A retained Error is <=1800 encoded bytes/depth8. Even allowing empty
       keys, each member costs at least5 bytes and each array node at least2.
       These separate conservative witnesses cover bytes, fields, nodes and
       depth under componentwise maximum. They deliberately are NOT intents. *)
    let rec nested depth json =
      if depth = 0 then json else nested (depth - 1) (`Object [ "x", json ])
    in
    let rejection error = `Object [ "kind", `String "rejected"; "error", error ] in
    let diagnostic_nodes, diagnostic_fields, diagnostic_depth =
      match action with
      | Start _ -> 900, 360, 7
      (* Current Resume's decoder permits any error within its existing
         whole-phase2048-byte/depth12 ceiling. Do not narrow that codec. *)
      | Resume _ -> 1024, 410, 11
    in
    let future_accepted, future_claimed =
      match action with
      | Start intent ->
        (match Goal_native_start_intent.phase intent with
         | Staged -> true, true
         | Accepted _ -> false, true
         | Dispatch_claimed _ -> false, false
         | Admitted _ | Discarded _ | Rejected _ -> false, false)
      | Resume intent ->
        (match Goal_native_resume_intent.phase intent with
         | Staged -> true, true
         | Accepted _ -> false, true
         | Dispatch_claimed _ -> false, false
         | Discarded _ | Rejected _ -> false, false)
    in
    let probes =
      (if future_accepted
       then [ `Object [ "kind", `String "accepted"; "accepted_at", accepted_at ] ]
       else [])
      @ (if future_claimed
         then
           [ `Object
               [ "kind", `String "dispatch_claimed"
               ; "accepted_at", accepted_at
               ; "claimed_at", claimed_at
               ]
           ]
         else [])
      @ [ rejection (`String (String.make reserve 'x'))
        ; rejection
            (nested
               diagnostic_depth
               (`Array (List.init diagnostic_nodes ~f:(fun _ -> `Number "0"))))
        ; rejection
            (nested
               diagnostic_depth
               (`Object
                   (List.init diagnostic_fields ~f:(fun i -> Int.to_string i, `Number "0"))))
        ]
      @ Option.to_list
          (Option.map admission ~f:(fun admission ->
             `Object
               [ "kind", `String "admitted"
               ; "accepted_at", accepted_at
               ; "claimed_at", claimed_at
               ; "admission", admission
               ]))
    in
    List.map probes ~f:(with_phase action))
;;

let sexp_of_t t = Jsonaf.sexp_of_t (to_json t)

let t_of_sexp sexp =
  match of_json (Jsonaf.t_of_sexp sexp) with
  | Ok t -> t
  | Error error -> Sexplib.Conv.of_sexp_error error.message sexp
;;
