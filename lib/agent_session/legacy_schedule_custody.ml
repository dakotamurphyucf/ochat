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
    (* Selected same-source DATA may advance while retaining the SAME original.
       Retire by admission identity, not by its prebuilt updated row wrapper. *)
    if
      not
        (List.exists incoming ~f:(fun (next : Row.t) ->
           phys_equal row.admission next.admission))
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

let validate_fresh ~source (schedule : P.Schedule.t) =
  let open Result.Let_syntax in
  let%bind () = P.Schedule.validate schedule in
  match schedule.status, schedule.ownership, schedule.inference_provenance with
  | Scheduled, Some ownership, Some provenance
    when P.Invocation.equal_observer ownership.source source.P.Run_source.observer
         && Int.equal schedule.generation source.generation
         && P.Run_source.equal (P.Schedule_provenance.source provenance) source -> Ok ()
  | (Scheduled | Delivering | Delivered | Cancelled | Failed _), _, _ ->
    Error
      (P.Error.invalid_request "owned timer custody requires its actual fresh creation")
;;

module Fresh = struct
  type phase =
    | Staged
    | Installed
    | Discarded

  type row = Row.t

  type t =
    { row : row
    ; mutable phase : phase
    }

  let value t = t.row.schedule

  let stage ~manager ~source ~schedule ~parent =
    let open Result.Let_syntax in
    let%bind () = validate_fresh ~source schedule in
    let%map admission = Execution_custody.Admission.transfer parent in
    { row = { Row.manager; source; schedule; admission }; phase = Staged }
  ;;

  let discard t =
    match t.phase with
    | Installed | Discarded -> ()
    | Staged ->
      t.phase <- Discarded;
      Execution_custody.Admission.retire t.row.admission
  ;;
end

module Staged = struct
  type entry =
    { owner : P.Job.launch_owner
    ; receipt : int
    ; fresh : Fresh.t
    }

  type t = { mutable entries : entry list }

  let create () = { entries = [] }
  let owned entry owner = P.Job.equal_launch_owner entry.owner owner

  let attach t ~owner ~receipt ~fresh =
    if
      List.exists t.entries ~f:(fun entry ->
        (owned entry owner && Int.equal entry.receipt receipt)
        || phys_equal entry.fresh fresh)
    then (
      (* A duplicate must not revoke a child already owned by this stager. *)
      if not (List.exists t.entries ~f:(fun entry -> phys_equal entry.fresh fresh))
      then Fresh.discard fresh;
      Error (P.Error.invalid_request "duplicate creator custody receipt"))
    else (
      t.entries <- { owner; receipt; fresh } :: t.entries;
      Ok ())
  ;;

  let selected t ~owner ~receipts =
    List.filter_map receipts ~f:(fun receipt ->
      List.find_map t.entries ~f:(fun entry ->
        Option.some_if
          (owned entry owner && Int.equal entry.receipt receipt)
          (Fresh.value entry.fresh, entry.fresh)))
  ;;

  (* Called only after the actual DATA receipt abort succeeds. *)
  let abort t ~owner ~receipt =
    t.entries
    <- List.filter t.entries ~f:(fun entry ->
         if owned entry owner && Int.equal entry.receipt receipt
         then (
           Fresh.discard entry.fresh;
           false)
         else true)
  ;;

  let release_owner t ~owner =
    t.entries
    <- List.filter t.entries ~f:(fun entry ->
         if owned entry owner
         then (
           Fresh.discard entry.fresh;
           false)
         else true)
  ;;

  let abort_all t =
    List.iter t.entries ~f:(fun entry -> Fresh.discard entry.fresh);
    t.entries <- []
  ;;
end

module Update = struct
  type inventory = t

  type t =
    { basis : inventory
    ; incoming : inventory
    ; selected : Fresh.t list
    ; mutable installed : bool
    }

  let prepare_selected current ~selected ~changes =
    let fresh = List.map selected ~f:snd in
    let result =
      let open Result.Let_syntax in
      let%bind rows =
        List.map selected ~f:(fun (chain, (fresh : Fresh.t)) ->
          let%bind first, rest =
            match fresh.phase, chain with
            | Staged, first :: rest
              when Jsonaf.exactly_equal
                     (P.Schedule.to_json first)
                     (P.Schedule.to_json fresh.row.schedule) -> Ok (first, rest)
            | (Staged | Installed | Discarded), _ ->
              Error
                (P.Error.invalid_request
                   "selected creator differs from its actual creation receipt")
          in
          let%map final =
            List.fold_result rest ~init:first ~f:(fun previous next ->
              let%map () =
                P.Schedule.validate_transition ~previous:(Some previous) next
              in
              next)
          in
          { fresh.row with schedule = final })
        |> Result.all
      in
      let%bind retained =
        List.map current ~f:(fun (row : Row.t) ->
          let chain =
            List.filter changes ~f:(fun value ->
              P.Id.Schedule.equal row.schedule.id value.P.Schedule.id)
          in
          match chain with
          | [] -> Ok row
          | _ :: _ ->
            let%map schedule =
              List.fold_result chain ~init:row.schedule ~f:(fun previous next ->
                let%map () =
                  P.Schedule.validate_transition ~previous:(Some previous) next
                in
                next)
            in
            { row with schedule })
        |> Result.all
      in
      let%map incoming = candidate retained ~staged:rows in
      { basis = current; incoming; selected = fresh; installed = false }
    in
    match result with
    | Ok _ -> result
    | Error _ ->
      List.iter fresh ~f:Fresh.discard;
      result
  ;;

  let check_before_commit t ~current ~manager ~source =
    let open Result.Let_syntax in
    let%bind () =
      if (not t.installed) && phys_equal current t.basis
      then Ok ()
      else Error (P.Error.invalid_request "creator custody inventory changed before ACK")
    in
    List.fold_result t.selected ~init:() ~f:(fun () (fresh : Fresh.t) ->
      match fresh.phase with
      | Staged
        when Option.exists manager ~f:(phys_equal fresh.row.manager)
             && P.Run_source.equal source fresh.row.source ->
        Execution_custody.Admission.check fresh.row.admission
      | Staged | Installed | Discarded ->
        Error (P.Error.invalid_request "creator custody lost its source or Manager"))
  ;;

  let discard t = if not t.installed then List.iter t.selected ~f:Fresh.discard

  let install t =
    t.installed <- true;
    List.iter t.selected ~f:(fun (fresh : Fresh.t) -> fresh.phase <- Installed);
    install t.basis ~incoming:t.incoming
  ;;
end
