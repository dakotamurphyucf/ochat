open! Core

type completion =
  { text : string
  ; model : string
  ; input_tokens : int option
  ; output_tokens : int option
  }

type complete = prompt:string -> (completion, string) result

type t =
  { id : string
  ; env : Eio_unix.Stdenv.base
  ; complete : complete
  ; wall_time_seconds : float
  ; max_prompt_bytes : int
  ; max_response_bytes : int
  }

let create
      ~env
      ?(wall_time_seconds = 30.)
      ?(max_prompt_bytes = 32_768)
      ?(max_response_bytes = 8_192)
      ~id
      ~complete
      ()
  =
  { id; env; complete; wall_time_seconds; max_prompt_bytes; max_response_bytes }
;;

let bounded_prompt t request =
  let prompt = Shell_access.Approval.prompt request in
  if String.length prompt <= t.max_prompt_bytes
  then Ok prompt
  else Error "model reviewer prompt exceeds its configured limit"
;;

let complete t prompt =
  let started = Eio.Time.now (Eio.Stdenv.clock t.env) in
  try
    let result =
      Eio.Time.with_timeout_exn
        (Eio.Stdenv.clock t.env)
        t.wall_time_seconds
        (fun () -> t.complete ~prompt)
    in
    result, started
  with
  | Eio.Time.Timeout -> Error "model reviewer timed out", started
;;

let metadata t started (completion : completion) =
  let elapsed = Eio.Time.now (Eio.Stdenv.clock t.env) -. started in
  Shell_access.Approval.
    { reviewer_id = t.id
    ; reviewer_kind = "model"
    ; model = Some completion.model
    ; input_tokens = completion.input_tokens
    ; output_tokens = completion.output_tokens
    ; latency_ms = Some (Float.iround_nearest_exn (elapsed *. 1000.))
    }
;;

let review_result t request =
  match bounded_prompt t request with
  | Error message -> Error message
  | Ok prompt ->
    let completed, started = complete t prompt in
    (match completed with
     | Error message -> Error (t.id ^ ": " ^ message)
     | Ok completion when String.length completion.text > t.max_response_bytes ->
       Error (t.id ^ ": response exceeds its configured limit")
     | Ok completion ->
       (match Shell_access.Approval.response_of_json completion.text with
        | Error message -> Error (t.id ^ ": " ^ message)
        | Ok response ->
          Ok
            Shell_access.Approval.
              { response; metadata = Some (metadata t started completion) }))
;;

let review t request =
  match review_result t request with
  | Ok review -> review.Shell_access.Approval.response
  | Error message -> Shell_access.Approval.Deny message
;;
