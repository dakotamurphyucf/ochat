open! Core
module P = Agent_protocol
module I = External_ingress

module Row = struct
  type t =
    { manager : Chat_response.Moderator_manager.t
    ; source : P.Run_source.t
    ; registration : I.t
    ; admission : Execution_custody.Admission.t
    }
end

type t = Row.t list

let empty = []
let is_empty = List.is_empty

let retire t =
  List.iter t ~f:(fun (row : Row.t) -> Execution_custody.Admission.retire row.admission)
;;

let same_creation (before : I.t) (after : I.t) =
  I.equal_context before.context after.context
  && Option.equal
       I.Provenance.equal
       before.inference_provenance
       after.inference_provenance
;;

let retain_runtime t ~manager =
  List.filter t ~f:(fun (row : Row.t) ->
    Option.exists manager ~f:(phys_equal row.manager))
;;

let retain t ~manager ~source ~registrations ~subscriptions ~now =
  List.filter t ~f:(fun (row : Row.t) ->
    Option.exists manager ~f:(phys_equal row.manager)
    && Option.exists source ~f:(P.Run_source.equal row.source)
    && List.exists registrations ~f:(fun (registration : I.t) ->
      same_creation row.registration registration
      && Option.is_none registration.revoked
      && P.Timestamp.compare now registration.context.expires_at < 0
      && List.exists subscriptions ~f:(fun (subscription : P.Subscription.t) ->
        P.Id.Subscription.equal
          subscription.context.id
          registration.context.subscription_id
        && Int.equal subscription.epoch registration.context.epoch
        && P.Id.Session.equal
             subscription.context.session_id
             registration.context.session_id
        && Int.equal subscription.context.generation registration.context.generation
        && Option.equal
             P.Invocation.equal_observer
             subscription.context.source
             (Some row.source.observer)
        && Option.is_none subscription.result)))
;;

let install previous ~incoming =
  List.iter previous ~f:(fun (row : Row.t) ->
    if not (List.mem incoming row ~equal:phys_equal)
    then Execution_custody.Admission.retire row.admission);
  incoming
;;

let candidate current ~staged =
  let incoming = current @ staged in
  let seen = Hash_set.create (module P.Id.Capability) in
  if
    List.for_all incoming ~f:(fun (row : Row.t) ->
      let id = row.registration.context.id in
      if Hash_set.mem seen id
      then false
      else (
        Hash_set.add seen id;
        true))
  then Ok incoming
  else Error (P.Error.invalid_request "duplicate ingress creator custody")
;;

let transfer_delivery t ~manager ~source ~registration =
  match
    List.find t ~f:(fun (row : Row.t) ->
      P.Id.Capability.equal row.registration.context.id registration.I.context.id)
  with
  | None -> Ok None
  | Some row
    when phys_equal row.manager manager
         && P.Run_source.equal row.source source
         && same_creation row.registration registration ->
    Execution_custody.Continuation.transfer_completed row.admission
    |> Result.map ~f:Option.some
  | Some _ ->
    Error (P.Error.invalid_request "ingress creator custody changed before delivery")
;;

let validate_fresh ~source (registration : I.t) =
  let open Result.Let_syntax in
  let%bind () = I.validate registration in
  if
    List.is_empty registration.receipts
    && Option.is_none registration.revoked
    && Int.equal registration.context.generation source.P.Run_source.generation
    && P.Invocation.equal_observer registration.context.source source.observer
    && Option.exists registration.inference_provenance ~f:(fun provenance ->
      P.Run_source.equal (I.Provenance.source provenance) source)
  then Ok ()
  else
    Error (P.Error.invalid_request "ingress custody requires its actual fresh creation")
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

  let value t = t.row.registration

  let stage ~manager ~source ~registration ~parent =
    let open Result.Let_syntax in
    let%bind () = validate_fresh ~source registration in
    let%map admission = Execution_custody.Admission.transfer parent in
    { row = { Row.manager; source; registration; admission }; phase = Staged }
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

  let prepare_selected current ~selected =
    let fresh = List.map selected ~f:snd in
    let result =
      let open Result.Let_syntax in
      let%bind rows =
        List.map selected ~f:(fun (chain, (fresh : Fresh.t)) ->
          let%bind first, rest =
            match fresh.phase, chain with
            | Staged, first :: rest when I.equal first fresh.row.registration ->
              Ok (first, rest)
            | (Staged | Installed | Discarded), _ ->
              Error
                (P.Error.invalid_request
                   "selected creator differs from its actual creation receipt")
          in
          let%map final =
            (* Dependency-aware validation against each actual Subscription already
             ran in Staged_ingress.selected. Check this same ordered carrier;
             no receipt/context here creates or replaces a creator original. *)
            List.fold_result rest ~init:first ~f:(fun _ next ->
              let%bind () = I.validate next in
              if same_creation fresh.row.registration next
              then Ok next
              else Error (P.Error.invalid_request "selected ingress changed its creator"))
          in
          { fresh.row with registration = final })
        |> Result.all
      in
      let%map incoming = candidate current ~staged:rows in
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
