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

val default_policy : policy
val collapse : request list -> request list
val should_end_session : request list -> string option
val request_turn : request list -> bool
val request_compaction : request list -> bool

val decide_after_turn_end
  :  policy:policy
  -> tool_followup:bool
  -> request list
  -> decision
