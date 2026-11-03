open! Core
module Objective = Goal_text.Objective
module Progress = Goal_text.Progress
module Reason = Goal_text.Reason
module Budget = Goal_budget
module Episode = Goal_episode
module Achievement_request = Goal_achievement.Request
module Achievement = Goal_achievement
module J = Json_codec

let invalid text = Error (Protocol_error.invalid_request text)

module Hold = Goal_state.Hold
module Closure = Goal_state.Closure
module State = Goal_state
module Capacity = Goal_capacity

type t =
  { id : Id.Goal.t
  ; session : Session_ref.t
  ; creator : Id.Principal.t
  ; source : Run_source.t
  ; objective : Objective.t
  ; progress : Progress.t
  ; budget : Budget.t
  ; state : State.t
  ; episode : Episode.t option
  ; achievement_request : Achievement_request.t option
  ; request_settlement : Episode.Settlement.t option
  ; previous_episode_settlement : Episode.Settlement.t option
  ; revision : int64
  }
[@@deriving equal, sexp_of]

let id t = t.id
let session t = t.session
let creator t = t.creator
let source t = t.source
let objective t = t.objective
let progress t = t.progress
let budget t = t.budget
let state t = t.state
let episode t = t.episode
let achievement_request t = t.achievement_request
let request_terminal t = Option.map t.request_settlement ~f:Episode.Settlement.receipt
let request_settlement t = t.request_settlement
let previous_episode_settlement t = t.previous_episode_settlement
let revision t = t.revision

let optional encode = function
  | None -> `Null
  | Some value -> encode value
;;

let nullable decode = function
  | `Null -> Ok None
  | value -> Result.map (decode value) ~f:Option.some
;;

let to_json t =
  `Object
    [ "id", Id.Goal.to_json t.id
    ; "session", Session_ref.to_json t.session
    ; "creator", Id.Principal.to_json t.creator
    ; "source", Run_source.to_json t.source
    ; "objective", Objective.to_json t.objective
    ; "progress", Progress.to_json t.progress
    ; "budget", Budget.to_json t.budget
    ; ( "state"
      , match t.state with
        | Closed (Achieved _) -> `Object [ "kind", `String "achieved" ]
        | Open _ | Closed (Abandoned _) -> State.to_json t.state )
    ; "episode", optional Episode.to_json t.episode
    ; "achievement_request", optional Achievement_request.to_json t.achievement_request
    ; "request_settlement", optional Episode.Settlement.to_json t.request_settlement
    ; ( "previous_episode_settlement"
      , optional Episode.Settlement.to_json t.previous_episode_settlement )
    ; "revision", `String (Int64.to_string t.revision)
    ]
;;

let reserved_capacity t =
  match t.state, t.achievement_request with
  | Open _, Some request -> Capacity.for_request request
  | Open _, None | Closed _, _ -> Capacity.measure (to_json t)
;;

let validate_projection t =
  let open Result.Let_syntax in
  let%bind () =
    Extension_codec.validate_json
      ~max_bytes:Capacity.max_projection_bytes
      ~max_depth:64
      (to_json t)
  in
  Result.map (reserved_capacity t) ~f:(fun _ -> ())
;;

let create ~id ~session ~creator ~source ~objective ~budget =
  let open Result.Let_syntax in
  let%bind () = Extension_codec.validate_id Id.Goal.to_json Id.Goal.of_json id in
  let%bind () =
    Extension_codec.validate_id Id.Principal.to_json Id.Principal.of_json creator
  in
  let%bind () =
    Extension_codec.validate_id
      Id.Server.to_json
      Id.Server.of_json
      (Session_ref.server_id session)
  in
  let%bind () =
    Extension_codec.validate_id
      Id.Session.to_json
      Id.Session.of_json
      (Session_ref.session_id session)
  in
  let%bind () = Run_source.validate source in
  let%bind progress = Progress.of_string "" in
  let t =
    { id
    ; session
    ; creator
    ; source
    ; objective
    ; progress
    ; budget
    ; state = Open (Held Source_review)
    ; episode = None
    ; achievement_request = None
    ; request_settlement = None
    ; previous_episode_settlement = None
    ; revision = 0L
    }
  in
  let%map () = validate_projection t in
  t
;;

let validate_associations t =
  let open Result.Let_syntax in
  let source_not_future source =
    if
      source.Run_source.generation <= t.source.generation
      && Int64.(source.installation_epoch <= t.source.installation_epoch)
      && ((not (Int64.equal source.installation_epoch t.source.installation_epoch))
          || Run_source.equal source t.source)
    then Ok ()
    else invalid "goal evidence refers to an unadopted future source"
  in
  let%bind () =
    match t.request_settlement, t.previous_episode_settlement with
    | Some request_proof, Some predecessor_proof
      when Id.Run.equal
             (Episode.Settlement.receipt request_proof).run_id
             (Episode.Settlement.receipt predecessor_proof).run_id
           && not (Episode.Settlement.equal request_proof predecessor_proof) ->
      invalid "one Run must reuse the exact retained goal terminal settlement"
    | None, _ | Some _, _ -> Ok ()
  in
  let%bind () =
    match t.previous_episode_settlement with
    | None -> Ok ()
    | Some proof ->
      let old = Episode.Settlement.episode proof in
      let%bind () = source_not_future (Episode.admission old).source in
      let%bind () =
        if Session_ref.equal t.session (Episode.session old)
        then Ok ()
        else invalid "foreign goal predecessor proof"
      in
      (match t.episode with
       | Some current
         when (not
                 (Id.Run.equal
                    (Episode.admission current).run_id
                    (Episode.admission old).run_id))
              && Int64.(
                   (Episode.admission current).session_revision
                   >= (Episode.Settlement.receipt proof).session_revision) -> Ok ()
       | None | Some _ ->
         invalid "goal episode admission precedes or aliases its predecessor terminal")
  in
  let%bind () =
    match t.episode with
    | None -> Ok ()
    | Some episode ->
      let%bind () = source_not_future (Episode.admission episode).source in
      if Session_ref.equal t.session (Episode.session episode)
      then Ok ()
      else invalid "foreign goal episode session"
  in
  let%bind () =
    match t.achievement_request, t.request_settlement with
    | None, None -> Ok ()
    | None, Some _ -> invalid "goal terminal association has no achievement request"
    | Some request, terminal ->
      let requested = Achievement_request.receipt request in
      let%bind () = source_not_future requested.source in
      let%bind () =
        if
          Session_ref.equal
            t.session
            (Episode.session (Achievement_request.episode request))
        then Ok ()
        else invalid "foreign goal request session"
      in
      (match terminal with
       | None ->
         (match t.episode with
          | Some episode when Episode.equal episode (Achievement_request.episode request)
            -> Ok ()
          | None | Some _ -> invalid "unresolved goal request belongs to another episode")
       | Some settlement ->
         let receipt = Episode.Settlement.receipt settlement in
         if
           Episode.equal
             (Episode.Settlement.episode settlement)
             (Achievement_request.episode request)
           && Run_receipt.Kind.equal receipt.kind Terminal
           && Id.Run.equal receipt.run_id requested.run_id
           && Id.Principal.equal receipt.principal_id requested.principal_id
           && Run_source.equal receipt.source requested.source
           && Int64.(receipt.run_revision >= requested.run_revision)
           && Int64.(receipt.session_revision >= requested.session_revision)
           && Timestamp.compare receipt.committed_at requested.committed_at >= 0
         then Ok ()
         else invalid "goal request terminal association conflicts")
  in
  let%bind () =
    match t.request_settlement, t.state with
    | Some settlement, (Open _ | Closed (Abandoned _)) ->
      (match t.achievement_request with
       | Some request when Result.is_ok (Achievement.of_evidence request settlement) ->
         invalid "matching Completed achievement settlement requires achieved closure"
       | None | Some _ -> Ok ())
    | None, _ | Some _, Closed (Achieved _) -> Ok ()
  in
  match t.state with
  | Open _ | Closed (Abandoned _) -> Ok ()
  | Closed (Achieved achievement) ->
    (match t.achievement_request, t.request_settlement, t.episode with
     | Some request, Some receipt, Some episode
       when Achievement_request.equal request (Achievement.request achievement)
            && Episode.Settlement.equal receipt (Achievement.settlement achievement)
            && Episode.equal episode (Achievement_request.episode request) -> Ok ()
     | None, _, _ | Some _, _, _ ->
       invalid "goal achievement lacks matching request and terminal associations")
;;

let validate_update t ~incoming =
  let open Result.Let_syntax in
  let%bind () = validate_projection incoming in
  let%bind () = validate_associations incoming in
  let%bind () =
    if
      Option.equal
        Achievement_request.equal
        t.achievement_request
        incoming.achievement_request
    then Ok ()
    else (
      match incoming.achievement_request, incoming.episode with
      | Some request, Some episode
        when Episode.equal (Achievement_request.episode request) episode -> Ok ()
      | Some _, _ ->
        invalid "introduced goal request must belong to the incoming current episode"
      | None, _ -> Ok ())
  in
  let%bind () =
    match t.achievement_request, incoming.achievement_request with
    | None, _ -> Ok ()
    | Some _, None -> invalid "goal achievement provenance cannot be removed"
    | Some previous, Some next ->
      if Achievement_request.equal previous next
      then (
        match t.request_settlement, incoming.request_settlement with
        | None, _ -> Ok ()
        | Some before, Some after when Episode.Settlement.equal before after -> Ok ()
        | Some _, None | Some _, Some _ ->
          invalid "settled goal receipt association is immutable")
      else if Option.is_none t.request_settlement
      then invalid "unresolved goal achievement request cannot be replaced"
      else if
        Episode.equal
          (Achievement_request.episode previous)
          (Achievement_request.episode next)
      then invalid "a settled episode cannot request achievement again"
      else Ok ()
  in
  let%bind () =
    match incoming.request_settlement with
    | Some settlement
      when not
             (Option.equal
                Episode.Settlement.equal
                t.request_settlement
                incoming.request_settlement) ->
      (match incoming.achievement_request with
       | Some request when Result.is_ok (Achievement.of_evidence request settlement) ->
         Ok ()
       | None | Some _ ->
         (match t.state, incoming.state with
          | Open Enabled, Open (Held Source_review) -> Ok ()
          | Open (Held before), Open (Held after) when Hold.equal before after -> Ok ()
          | Open _, _ | Closed _, _ ->
            invalid
              "terminal retirement must preserve its hold or hold enabled work for review"))
    | None | Some _ -> Ok ()
  in
  let source_changed = not (Run_source.equal t.source incoming.source) in
  let%bind () =
    if source_changed
    then (
      match incoming.state with
      | Open (Held Source_review) -> Ok ()
      | Open Enabled | Open (Held _) | Closed _ ->
        invalid "source adoption must leave the goal held for source review")
    else Ok ()
  in
  let%bind () =
    if Option.equal Episode.equal t.episode incoming.episode
    then
      if
        Option.equal
          Episode.Settlement.equal
          t.previous_episode_settlement
          incoming.previous_episode_settlement
      then Ok ()
      else invalid "goal predecessor proof changed without episode replacement"
    else (
      match t.episode, incoming.episode, incoming.previous_episode_settlement with
      | None, Some episode, None ->
        if Run_source.equal (Episode.admission episode).source incoming.source
        then Ok ()
        else invalid "new episode does not match the adopted source"
      | Some old, Some next, Some proof ->
        if
          Episode.equal old (Episode.Settlement.episode proof)
          && (not
                (Id.Run.equal
                   (Episode.admission old).run_id
                   (Episode.admission next).run_id))
          && Run_source.equal (Episode.admission next).source incoming.source
          && (Option.is_none t.achievement_request || Option.is_some t.request_settlement)
        then Ok ()
        else invalid "episode replacement lacks exact predecessor terminal proof"
      | None, None, _ | None, Some _, Some _ | Some _, None, _ | Some _, Some _, None ->
        invalid "goal episode history cannot be erased or replaced without terminal proof")
  in
  if equal t incoming
  then Ok ()
  else if
    not
      (Id.Goal.equal t.id incoming.id
       && Session_ref.equal t.session incoming.session
       && Id.Principal.equal t.creator incoming.creator)
  then invalid "goal identity and accounting scope are immutable"
  else (
    match t.state with
    | Closed _ -> invalid "closed goal metadata is immutable"
    | Open _ ->
      if
        Int64.equal t.revision Int64.max_value
        || not (Int64.equal incoming.revision (Int64.succ t.revision))
      then invalid "goal revision must advance exactly once without overflow"
      else if
        (not (Run_source.equal t.source incoming.source))
        && (incoming.source.generation < t.source.generation
            || Int64.(incoming.source.installation_epoch <= t.source.installation_epoch))
      then
        invalid
          "goal source adoption must advance installation without rewinding generation"
      else Ok ())
;;

let change t ~f =
  match t.state with
  | Closed _ -> invalid "closed goal cannot be edited or resumed"
  | Open _ ->
    if Int64.equal t.revision Int64.max_value
    then invalid "goal revision exhausted"
    else (
      let incoming = { (f t) with revision = Int64.succ t.revision } in
      Result.map (validate_update t ~incoming) ~f:(fun () -> incoming))
;;

let revise_objective t objective = change t ~f:(fun t -> { t with objective })
let record_progress t progress = change t ~f:(fun t -> { t with progress })
let change_budget t budget = change t ~f:(fun t -> { t with budget })
let hold t reason = change t ~f:(fun t -> { t with state = Open (Held reason) })
let resume t = change t ~f:(fun t -> { t with state = Open Enabled })

let bind_episode t episode ~previous =
  let open Result.Let_syntax in
  let admission = Episode.admission episode in
  let%bind () =
    if
      Session_ref.equal t.session (Episode.session episode)
      && Run_source.equal t.source admission.source
    then Ok ()
    else invalid "goal episode must match current session and adopted source"
  in
  let%bind previous_episode_settlement =
    match t.episode, previous with
    | None, None -> Ok None
    | Some old, Some (run, receipt) ->
      Result.map (Episode.Settlement.create old ~run ~receipt) ~f:Option.some
    | None, Some _ | Some _, None ->
      invalid "replacing a goal episode needs its actual prior terminal evidence"
  in
  let%bind () =
    match t.achievement_request, t.request_settlement with
    | Some _, None ->
      invalid "unresolved achievement request prevents episode replacement"
    | None, _ | Some _, Some _ -> Ok ()
  in
  let%bind () =
    match t.episode with
    | Some old when Id.Run.equal (Episode.admission old).run_id admission.run_id ->
      invalid "goal episode identity cannot be reused"
    | None | Some _ -> Ok ()
  in
  change t ~f:(fun t -> { t with episode = Some episode; previous_episode_settlement })
;;

let request_achievement t request =
  let open Result.Let_syntax in
  let%bind () =
    match t.episode with
    | Some episode when Episode.equal episode (Achievement_request.episode request) ->
      Ok ()
    | None | Some _ -> invalid "achievement request must belong to the current episode"
  in
  let%bind () =
    match t.achievement_request, t.request_settlement with
    | Some _, None -> invalid "goal already has an unresolved achievement request"
    | None, _ | Some _, Some _ -> Ok ()
  in
  change t ~f:(fun t ->
    { t with achievement_request = Some request; request_settlement = None })
;;

let retire_request t ~run ~receipt =
  let open Result.Let_syntax in
  let%bind request =
    Result.of_option
      t.achievement_request
      ~error:(Protocol_error.invalid_request "goal has no achievement request")
  in
  let%bind settlement =
    Episode.Settlement.create (Achievement_request.episode request) ~run ~receipt
  in
  let action = Achievement_request.receipt request in
  let%bind () =
    if
      Int64.(receipt.Run_receipt.run_revision >= action.run_revision)
      && Int64.(receipt.session_revision >= action.session_revision)
    then Ok ()
    else invalid "goal request terminal predates its action"
  in
  let%bind () =
    match run.Run.lifecycle with
    | Terminal (Failed _ | Cancelled | Limited | Interrupted) -> Ok ()
    | Terminal (Completed _) | Admitted | Active | Waiting _ ->
      invalid "only an actual adverse terminal can retire achievement"
  in
  let%bind () =
    match t.request_settlement with
    | None -> Ok ()
    | Some _ -> invalid "goal request already settled"
  in
  change t ~f:(fun t ->
    let state =
      match t.state with
      | Open Enabled -> State.Open (Held Source_review)
      | Open (Held _) | Closed _ -> t.state
    in
    { t with request_settlement = Some settlement; state })
;;

let achieve t achievement =
  let open Result.Let_syntax in
  let request = Achievement.request achievement in
  let%bind () =
    match t.episode with
    | Some episode when Episode.equal episode (Achievement_request.episode request) ->
      Ok ()
    | None | Some _ -> invalid "goal achievement must belong to its current episode"
  in
  let%bind () =
    match t.achievement_request, t.request_settlement with
    | None, None -> Ok ()
    | Some previous, None when Achievement_request.equal previous request -> Ok ()
    | Some previous, Some _
      when not
             (Episode.equal
                (Achievement_request.episode previous)
                (Achievement_request.episode request)) -> Ok ()
    | None, Some _ | Some _, _ ->
      invalid "goal achievement must settle its exact unresolved request"
  in
  change t ~f:(fun t ->
    { t with
      state = Closed (Achieved achievement)
    ; achievement_request = Some request
    ; request_settlement = Some (Achievement.settlement achievement)
    })
;;

let abandon t reason = change t ~f:(fun t -> { t with state = Closed (Abandoned reason) })

let adopt_source t source =
  let open Result.Let_syntax in
  let%bind () = Run_source.validate source in
  if Run_source.equal source t.source
  then invalid "source adoption requires a new installation"
  else change t ~f:(fun t -> { t with source; state = Open (Held Source_review) })
;;

let natural = function
  | `String text ->
    (match Int64.of_string_opt text with
     | Some value when Int64.(value >= 0L) && String.equal text (Int64.to_string value) ->
       Ok value
     | None | Some _ ->
       invalid "goal revision must be a canonical nonnegative decimal string")
  | _ -> invalid "goal revision must be a decimal string"
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind () =
    Extension_codec.validate_json
      ~max_bytes:Capacity.max_projection_bytes
      ~max_depth:64
      json
  in
  let%bind f = J.fields json in
  let%bind id = J.required_as f "id" Id.Goal.of_json in
  let%bind session = J.required_as f "session" Session_ref.of_json in
  let%bind creator = J.required_as f "creator" Id.Principal.of_json in
  let%bind source = J.required_as f "source" Run_source.of_json in
  let%bind objective = J.required_as f "objective" Objective.of_json in
  let%bind progress = J.required_as f "progress" Progress.of_json in
  let%bind budget = J.required_as f "budget" Budget.of_json in
  let%bind episode = J.required_as f "episode" (nullable Episode.of_json) in
  let%bind achievement_request =
    J.required_as f "achievement_request" (nullable Achievement_request.of_json)
  in
  let%bind request_settlement =
    J.required_as f "request_settlement" (nullable Episode.Settlement.of_json)
  in
  let%bind previous_episode_settlement =
    J.required_as f "previous_episode_settlement" (nullable Episode.Settlement.of_json)
  in
  let%bind state =
    J.required_as f "state" (fun json ->
      let%bind fields = J.fields json in
      let%bind kind = J.required_as fields "kind" J.string in
      if String.equal kind "achieved"
      then (
        match achievement_request, request_settlement with
        | Some request, Some settlement ->
          Result.map (Achievement.of_evidence request settlement) ~f:(fun proof ->
            State.Closed (Achieved proof))
        | None, _ | Some _, None ->
          invalid "achieved goal requires request and terminal evidence")
      else State.of_json json)
  in
  let%bind revision = J.required_as f "revision" natural in
  let t =
    { id
    ; session
    ; creator
    ; source
    ; objective
    ; progress
    ; budget
    ; state
    ; episode
    ; achievement_request
    ; request_settlement
    ; previous_episode_settlement
    ; revision
    }
  in
  let%bind () = validate_associations t in
  let%map () = validate_projection t in
  t
;;
