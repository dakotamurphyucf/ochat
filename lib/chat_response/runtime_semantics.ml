open! Core

type request = Moderation.Runtime_request.t

type policy =
  { honor_request_turn : bool
  ; honor_request_compaction : bool
  }

type continue_decision =
  [ `Stop
  | `Continue
  ]

type decision =
  { continue : continue_decision
  ; end_session_reason : string option
  ; compaction_requested : bool
  ; forward : request list
  }

let default_policy = { honor_request_turn = true; honor_request_compaction = false }

let should_end_session (requests : request list) : string option =
  List.find_map requests ~f:(function
    | Moderation.Runtime_request.End_session reason -> Some reason
    | Request_compaction | Request_turn -> None)
;;

let request_turn (requests : request list) : bool =
  List.exists requests ~f:(function
    | Moderation.Runtime_request.Request_turn -> true
    | Request_compaction | End_session _ -> false)
;;

let request_compaction (requests : request list) : bool =
  List.exists requests ~f:(function
    | Moderation.Runtime_request.Request_compaction -> true
    | Request_turn | End_session _ -> false)
;;

let collapse (requests : request list) : request list =
  let saw_turn = ref false in
  let saw_compaction = ref false in
  let saw_end_session = ref false in
  List.filter requests ~f:(function
    | Moderation.Runtime_request.Request_turn ->
      if !saw_turn
      then false
      else (
        saw_turn := true;
        true)
    | Moderation.Runtime_request.Request_compaction ->
      if !saw_compaction
      then false
      else (
        saw_compaction := true;
        true)
    | Moderation.Runtime_request.End_session _ ->
      if !saw_end_session
      then false
      else (
        saw_end_session := true;
        true))
;;

let decide_after_turn_end
      ~(policy : policy)
      ~(tool_followup : bool)
      (requests : request list)
  : decision
  =
  let requests = collapse requests in
  let end_session_reason = should_end_session requests in
  let compaction_requested =
    policy.honor_request_compaction && request_compaction requests
  in
  let continue =
    match end_session_reason with
    | Some _ -> `Stop
    | None ->
      if tool_followup
      then `Continue
      else if policy.honor_request_turn && request_turn requests
      then `Continue
      else `Stop
  in
  { continue; end_session_reason; compaction_requested; forward = requests }
;;
