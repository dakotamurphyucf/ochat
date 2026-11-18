open! Core
module P = Agent_protocol

let invalid message = Error (P.Error.invalid_request message)

module Source = struct
  type t =
    { observer : P.Invocation.observer option
    ; installation : P.Run_source.t option
    }

  let create ~observer ~installation =
    let open Result.Let_syntax in
    let%map () =
      match installation with
      | None -> Ok ()
      | Some installation ->
        let%bind () = P.Run_source.validate installation in
        if Option.exists observer ~f:(P.Invocation.equal_observer installation.observer)
        then Ok ()
        else invalid "standalone source installation differs from its captured observer"
    in
    { observer; installation }
  ;;

  let equal left right =
    Option.equal P.Invocation.equal_observer left.observer right.observer
    && Option.equal P.Run_source.equal left.installation right.installation
  ;;
end

module Subject = struct
  type t =
    | Root_operation of P.Id.Operation.t
    | Member of Policy_cause_data.Member.t
  [@@deriving equal]
end

type t =
  { session_id : P.Id.Session.t
  ; generation : int
  ; subject : Subject.t
  ; origin : P.Inference_origin.t
  ; source : Source.t
  }

let create ~session_id ~generation ~subject ~origin ~source =
  let open Result.Let_syntax in
  let%bind () =
    if generation >= 0 then Ok () else invalid "standalone source generation is negative"
  in
  let%bind () = P.Inference_origin.validate origin in
  let%bind () =
    match subject with
    | Subject.Root_operation _ -> Ok ()
    | Member member ->
      if Int.equal (Policy_cause_data.Member.generation member) generation
      then Ok ()
      else invalid "standalone source member generation differs"
  in
  let%map () =
    match source.Source.installation with
    | None -> Ok ()
    | Some installation ->
      if Int.equal installation.generation generation
      then Ok ()
      else invalid "standalone source installation generation differs"
  in
  { session_id; generation; subject; origin; source }
;;

let equal left right =
  P.Id.Session.equal left.session_id right.session_id
  && Int.equal left.generation right.generation
  && Subject.equal left.subject right.subject
  && P.Inference_origin.equal left.origin right.origin
  && Source.equal left.source right.source
;;

module Classification = struct
  type descriptor = t

  type t =
    | Unknown
    | Standalone_unrelated of descriptor
end
