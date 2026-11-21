open! Core
module R = Capacity_rollover

type t =
  { port : R.t
  ; on_retired : unit -> unit
  }

let create port ~on_retired = { port; on_retired }
let on_retired t = t.on_retired ()

module Request = struct
  type control = t

  type t =
    { control : control
    ; can_roll_over : bool
    ; mutable observed_output : bool
    }

  exception Retired

  let create control ~can_roll_over = { control; can_roll_over; observed_output = false }

  let observe t event =
    match Inference.Event.view event with
    | Terminal _ -> ()
    | Candidate_ready _ -> t.observed_output <- true
    | Live live ->
      (match Transcript.Stream.view live with
       | Source_started _ -> ()
       | _ -> t.observed_output <- true)
  ;;

  let require = function
    | Ok value -> value
    | Error error -> raise (Effect_rejection.Rejected error)
  ;;

  let withhold () =
    raise
      (Effect_rejection.Rejected
         (Agent_protocol.Error.create
            Resource_limit
            ~message:"ordinary inference withheld at the context input limit"
            ~retryable:false
            ()))
  ;;

  let before_dispatch t prepared =
    let stage = R.Unsent prepared in
    let request trigger =
      if not t.can_roll_over then withhold ();
      match t.control.port.request ~trigger ~stage |> require with
      | Retire -> raise Retired
      | Continue -> withhold ()
    in
    match t.control.port.inspect stage |> require with
    | No_transition -> ()
    | Hard_limit -> request Hard_input_limit
    | Checkpoint_opportunity -> request Soft_input_limit
    | Qualified_rejection -> withhold ()
  ;;

  let after_receipt t ~attempt ~receipt ~finish_source =
    if t.can_roll_over && not t.observed_output
    then
      Option.iter (R.rejection ~attempt ~receipt) ~f:(fun rejection ->
        finish_source ();
        let stage = R.Rejected rejection in
        match t.control.port.inspect stage |> require with
        | Qualified_rejection ->
          (match
             t.control.port.request ~trigger:Provider_context_limit ~stage |> require
           with
           | Retire -> raise Retired
           | Continue -> ())
        | No_transition | Hard_limit | Checkpoint_opportunity -> ())
  ;;
end
