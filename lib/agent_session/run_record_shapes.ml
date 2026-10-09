open! Core
module X = Persistence_codec
module D = Document_schema

let v = D.Shape.value
let o = X.shape_exn
let f = X.fields_shape
let a = X.array_shape_exn
let n = D.Shape.nullable
let source = f [ "script_id"; "source_sha256"; "generation"; "installation_epoch" ]

let receipt =
  o
    [ "run_id", v
    ; "principal_id", v
    ; "source", source
    ; "key", v
    ; "request_sha256", v
    ; "kind", v
    ; "run_revision", v
    ; "session_revision", v
    ; "committed_at", v
    ]
;;

let work_key = o [ "kind", v; "id", v; "key", f [ "kind"; "id"; "attempt" ] ]
let work = o [ "key", work_key; "generation", v ]
let terminal_work = o [ "work", work; "outcome", v; "revision", v ]

let occurrence =
  o
    [ "kind", v
    ; "job_id", v
    ; "attempt", v
    ; "schedule_id", v
    ; "delivery_count", v
    ; "creator", f [ "kind"; "id" ]
    ; "subscription", n (f [ "id"; "epoch" ])
    ; "subscription_id", v
    ; "epoch", v
    ; "delivery_id", v
    ]
;;

let wake = o [ "run_id", v; "source", source; "occurrence", occurrence ]

let result_reference =
  o
    [ "kind", v
    ; "result", v
    ; "operation_id", v
    ; "generation", v
    ; "history_ids", a v
    ; "revision", v
    ]
;;

let terminal = o [ "kind", v; "result", n result_reference; "code", n v ]
let lifecycle = o [ "kind", v; "wake", wake; "terminal", terminal ]

let run =
  o
    [ "id", v
    ; "session", f [ "server_id"; "session_id" ]
    ; "principal_id", v
    ; "source", source
    ; "mode", v
    ; "lifecycle", lifecycle
    ; "revision", v
    ; "owned_work", a work
    ; "relinquished_work", a work
    ; "terminal_work", a terminal_work
    ; "created_at", v
    ; "updated_at", v
    ]
;;

let action = o [ "kind", v; "wake", wake; "terminal", terminal; "relinquish", a work ]

let intent =
  o
    [ "receipt", receipt
    ; "execution_id", v
    ; "action", action
    ; "disposition", o [ "kind", v; "operation_id", n v ]
    ]
;;
