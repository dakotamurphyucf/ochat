open! Core
module P = Agent_protocol

module Row = struct
  type t =
    { manager : Chat_response.Moderator_manager.t
    ; source : P.Run_source.t
    ; schedule : P.Schedule.t
    ; admission : Execution_custody.Admission.t
    }
end

type t = Row.t list

let empty = []
let is_empty = List.is_empty

let retire t =
  List.iter t ~f:(fun (row : Row.t) -> Execution_custody.Admission.retire row.admission)
;;

let stage ~manager ~source ~(schedule : P.Schedule.t) ~parent =
  let open Result.Let_syntax in
  let%bind () = P.Schedule.validate schedule in
  let%bind () =
    match schedule.inference_provenance, schedule.status, schedule.ownership with
    | Some provenance, Scheduled, None
      when P.Run_source.equal (P.Schedule_provenance.source provenance) source -> Ok ()
    | (Some _ | None), (Scheduled | Delivering | Delivered | Cancelled | Failed _), _ ->
      Error
        (P.Error.invalid_request "legacy timer custody requires its captured creation")
  in
  let%map admission = Execution_custody.Admission.transfer parent in
  [ ({ manager; source; schedule; admission } : Row.t) ]
;;

let candidate t ~staged =
  let candidate = t @ staged in
  let identities = Hash_set.create (module P.Id.Schedule) in
  if
    List.for_all candidate ~f:(fun (row : Row.t) ->
      if Hash_set.mem identities row.schedule.id
      then false
      else (
        Hash_set.add identities row.schedule.id;
        true))
  then Ok candidate
  else Error (P.Error.invalid_request "duplicate legacy timer custody")
;;

let same_creation (previous : P.Schedule.t) (current : P.Schedule.t) =
  P.Id.Schedule.equal previous.id current.id
  && P.Id.Session.equal previous.session_id current.session_id
  && Int.equal previous.generation current.generation
  && Jsonaf.exactly_equal previous.payload current.payload
  && P.Timestamp.equal previous.created_at current.created_at
  && P.Timestamp.equal previous.next_due_at current.next_due_at
  && P.Schedule.equal_misfire previous.misfire current.misfire
  && Option.equal P.Schedule.equal_ownership previous.ownership current.ownership
  && Option.equal
       P.Schedule_provenance.equal
       previous.inference_provenance
       current.inference_provenance
;;

let retain_facts t ~source ~schedules =
  match source with
  | Some source ->
    List.filter t ~f:(fun (row : Row.t) ->
      P.Run_source.equal row.source source
      && List.exists schedules ~f:(fun (schedule : P.Schedule.t) ->
        same_creation row.schedule schedule
        &&
        match schedule.status with
        | Scheduled | Delivering -> true
        | Delivered | Cancelled | Failed _ -> false))
  | None -> []
;;

let retain_runtime t ~manager =
  match manager with
  | None -> []
  | Some manager -> List.filter t ~f:(fun (row : Row.t) -> phys_equal row.manager manager)
;;

let retain t ~manager ~source ~schedules =
  retain_facts t ~source ~schedules |> retain_runtime ~manager
;;

let install previous ~incoming =
  List.iter previous ~f:(fun (row : Row.t) ->
    if not (List.mem incoming row ~equal:phys_equal)
    then Execution_custody.Admission.retire row.admission);
  incoming
;;

let transfer_delivery t ~manager ~source ~(schedule : P.Schedule.t) =
  match
    List.find t ~f:(fun (row : Row.t) -> P.Id.Schedule.equal row.schedule.id schedule.id)
  with
  | None -> Ok None
  | Some row ->
    if
      phys_equal row.manager manager
      && P.Run_source.equal row.source source
      && same_creation row.schedule schedule
    then
      Execution_custody.Continuation.transfer_completed row.admission
      |> Result.map ~f:Option.some
    else Error (P.Error.invalid_request "legacy timer custody changed before delivery")
;;

let validate_constructor_handoff t ~manager ~source ~ids =
  if
    List.for_all t ~f:(fun (row : Row.t) ->
      (not (List.mem ids row.schedule.id ~equal:P.Id.Schedule.equal))
      || (phys_equal row.manager manager && P.Run_source.equal row.source source))
  then Ok ()
  else Error (P.Error.invalid_request "constructor timer custody changed")
;;

let without_constructor t ~manager ~source ~ids =
  List.filter t ~f:(fun (row : Row.t) ->
    not
      (phys_equal row.manager manager
       && P.Run_source.equal row.source source
       && List.mem ids row.schedule.id ~equal:P.Id.Schedule.equal))
;;
