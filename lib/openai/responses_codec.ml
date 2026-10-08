open Core
module Request = Responses_request
module Wire = Responses_wire

type failure =
  | Framing of string
  | Decode of
      { raw : Jsonaf.t
      ; error : Wire.Decode_error.t
      }
  | Protocol of
      { event : Wire.Event.t option
      ; error : Wire.Tracker.error
      }

let protocol_violation ~stage failure =
  let module P = Inference.Observation.Diagnostic.Protocol_violation in
  let kind =
    match failure with
    | Framing _ -> P.Framing
    | Decode { error; raw = _ } ->
      P.Decode
        (match error.reason with
         | Missing_field -> Missing
         | Wrong_type _ -> Wrong_type
         | Invalid_value _ -> Invalid
         | Duplicate_field _ -> Duplicate
         | Limit_exceeded -> Limit)
    | Protocol { error; event = _ } ->
      P.Tracker
        (match error with
         | Origin_mismatch -> Origin_mismatch
         | Sequence_conflict _ -> Sequence_conflict
         | Sequence_regression _ -> Sequence_regression
         | Item_conflict _ -> Item_conflict
         | Part_conflict _ -> Part_conflict
         | Response_conflict -> Response_conflict
         | Event_after_terminal -> Event_after_terminal
         | Terminal_mismatch -> Terminal_mismatch
         | Truncated -> Truncated)
  in
  { P.stage; kind }
;;

let decode_response raw ~origin =
  Result.map_error (Wire.Response.decode raw ~origin) ~f:(fun error ->
    Decode { raw; error })
;;

module Stream = struct
  type t =
    { origin : Wire.Origin.t
    ; framing : Responses_sse.t
    ; mutable tracker : Wire.Tracker.t
    ; mutable failure : failure option
    ; mutable item_conflict :
        Inference.Observation.Diagnostic.Protocol_violation.Item_conflict.t option
    ; mutable ended : bool
    }

  type update =
    { event : Wire.Event.t
    ; disposition : Wire.Tracker.disposition
    ; newly_finalized : (int * Wire.Item.t) list
    }

  let create ?max_frame_bytes origin =
    Or_error.map (Responses_sse.create ?max_frame_bytes ()) ~f:(fun framing ->
      { origin
      ; framing
      ; tracker = Wire.Tracker.create origin
      ; failure = None
      ; item_conflict = None
      ; ended = false
      })
  ;;

  let protocol_violation t ~stage failure =
    match failure, t.item_conflict with
    | Protocol { error = Wire.Tracker.Item_conflict _; _ }, Some detail ->
      { Inference.Observation.Diagnostic.Protocol_violation.stage
      ; kind = Item_conflict detail
      }
    | _ -> protocol_violation ~stage failure
  ;;

  let terminal t =
    Result.map_error (Wire.Tracker.finish t.tracker) ~f:(fun error ->
      Protocol { event = None; error })
  ;;

  let feed_line t line =
    match t.failure with
    | Some error -> Error error
    | None ->
      let result =
        if t.ended
        then Error (Framing "data after end of SSE stream")
        else
          let open Result.Let_syntax in
          let%bind frame =
            Result.map_error (Responses_sse.feed_line t.framing line) ~f:(fun error ->
              Framing (Error.to_string_hum error))
          in
          match frame with
          | None -> Ok None
          | Some Done ->
            let%map _ = terminal t in
            t.ended <- true;
            None
          | Some (Payload raw) ->
            let%bind event =
              Result.map_error (Wire.Event.decode raw ~origin:t.origin) ~f:(fun error ->
                Decode { raw; error })
            in
            let%map transition =
              Result.map_error (Wire.Tracker.add t.tracker event) ~f:(fun error ->
                (match error with
                 | Wire.Tracker.Item_conflict output_index ->
                   t.item_conflict
                   <- Wire.Tracker.item_conflict_detail t.tracker event ~output_index
                 | _ -> ());
                Protocol { event = Some event; error })
            in
            t.tracker <- transition.tracker;
            Some
              { event
              ; disposition = transition.disposition
              ; newly_finalized = transition.newly_finalized
              }
      in
      (match result with
       | Ok _ -> ()
       | Error error -> t.failure <- Some error);
      result
  ;;

  let finish t =
    match t.failure with
    | Some error -> Error error
    | None ->
      let pending = if t.ended then false else Responses_sse.finish t.framing in
      t.ended <- true;
      let result =
        if pending
        then Error (Protocol { event = None; error = Wire.Tracker.Truncated })
        else terminal t
      in
      (match result with
       | Ok _ -> ()
       | Error error -> t.failure <- Some error);
      result
  ;;
end
