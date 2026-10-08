open! Core
module P = Agent_protocol
module DTO = P.Provider_operator
module M = Credential_registry_model
module S = Private_storage

module Error = struct
  type t =
    | Corrupt
    | Full
    | Conflict
    | Missing
    | Busy
    | Storage of S.Error.t
  [@@deriving sexp_of]
end

module Record = struct
  type t =
    { incarnation : M.Id.t
    ; owner : P.Id.Principal.t
    ; operation : M.Id.t
    ; binding : M.Id.t
    ; key : P.Idempotency_key.t
    ; mode : DTO.Login_mode.t
    ; result : DTO.Flow_result.t
    }

  let create ~incarnation ~owner ~operation ~binding ~key ~mode ~flow =
    { incarnation
    ; owner
    ; operation
    ; binding
    ; key
    ; mode
    ; result = { flow; phase = Pending }
    }
  ;;

  let owner t = t.owner
  let operation t = t.operation
  let binding t = t.binding
  let key t = t.key
  let mode t = t.mode
  let result t = t.result

  let to_json t =
    `Object
      [ "incarnation", `String (M.Id.to_string t.incarnation)
      ; "owner", P.Id.Principal.to_json t.owner
      ; "operation", `String (M.Id.to_string t.operation)
      ; "binding", `String (M.Id.to_string t.binding)
      ; "key", P.Idempotency_key.to_json t.key
      ; "mode", DTO.Login_mode.to_json t.mode
      ; "result", DTO.Flow_result.to_json t.result
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let fields =
      match json with
      | `Object fields -> Some fields
      | _ -> None
    in
    let%bind fields = Result.of_option fields ~error:Error.Corrupt in
    let expected =
      [ "incarnation"; "owner"; "operation"; "binding"; "key"; "mode"; "result" ]
    in
    let%bind () =
      if
        List.equal
          String.equal
          (List.sort (List.map fields ~f:fst) ~compare:String.compare)
          (List.sort expected ~compare:String.compare)
      then Ok ()
      else Error Error.Corrupt
    in
    let get key decode =
      List.Assoc.find fields key ~equal:String.equal
      |> Result.of_option ~error:Error.Corrupt
      |> Result.bind ~f:(fun value ->
        decode value |> Result.map_error ~f:(fun _ -> Error.Corrupt))
    in
    let model_id = function
      | `String value -> M.Id.create value
      | _ -> M.Id.create ""
    in
    let%bind incarnation = get "incarnation" model_id in
    let%bind owner = get "owner" P.Id.Principal.of_json in
    let%bind operation = get "operation" model_id in
    let%bind binding = get "binding" model_id in
    let%bind key = get "key" P.Idempotency_key.of_json in
    let%bind mode = get "mode" DTO.Login_mode.of_json in
    let%map result = get "result" DTO.Flow_result.of_json in
    { incarnation; owner; operation; binding; key; mode; result }
  ;;
end

type t =
  { directory : S.Directory.t
  ; incarnation : M.Id.t
  ; maximum_records : int
  }

let name value =
  match S.Name.create value with
  | Ok name -> name
  | Error _ -> failwith "invalid static owner-record filename"
;;

let metadata_name = name "operator-owner-records.json"
let lock_name = name "operator-owner-records.lock"

let storage error =
  if S.Error.code error |> S.Error.equal_code S.Error.Busy
  then Error.Busy
  else Error.Storage error
;;

let create directory ~incarnation ~maximum_records =
  if maximum_records <= 0 || maximum_records > 128
  then Error Error.Full
  else Ok { directory; incarnation; maximum_records }
;;

let equal_flow (left : DTO.Flow_ref.t) (right : DTO.Flow_ref.t) =
  P.Id.Server.equal left.server_id right.server_id
  && DTO.Profile_id.equal left.profile right.profile
  && DTO.Flow_id.equal left.flow_id right.flow_id
  && P.Timestamp.equal left.expires_at right.expires_at
;;

let read t =
  match S.Directory.read_bounded t.directory metadata_name ~max_bytes:(512 * 1024) with
  | Error error when S.Error.equal_code (S.Error.code error) S.Error.Missing -> Ok []
  | Error error -> Error (storage error)
  | Ok bytes ->
    let open Result.Let_syntax in
    let%bind json =
      Result.try_with (fun () -> Jsonaf.of_string (Bytes.to_string bytes))
      |> Result.map_error ~f:(fun _ -> Error.Corrupt)
    in
    let%bind values =
      match json with
      | `Object fields
        when List.equal
               String.equal
               (List.map fields ~f:fst |> List.sort ~compare:String.compare)
               [ "records"; "version" ] ->
        (match
           ( List.Assoc.find_exn fields "version" ~equal:String.equal
           , List.Assoc.find_exn fields "records" ~equal:String.equal )
         with
         | `Number "1", `Array values -> Ok values
         | _ -> Error Error.Corrupt)
      | _ -> Error Error.Corrupt
    in
    let%bind () =
      if List.length values <= t.maximum_records then Ok () else Error Error.Full
    in
    let%bind records = List.map values ~f:Record.of_json |> Result.all in
    let%bind () =
      if
        List.for_all records ~f:(fun record ->
          M.Id.equal record.Record.incarnation t.incarnation)
      then Ok ()
      else Error Error.Conflict
    in
    let%bind () =
      if
        List.existsi records ~f:(fun i r ->
          List.exists (List.take records i) ~f:(fun previous ->
            M.Id.equal previous.Record.operation r.Record.operation
            || (P.Id.Principal.equal previous.owner r.owner
                && P.Idempotency_key.equal previous.key r.key)))
      then Error Error.Corrupt
      else Ok ()
    in
    let%map () =
      if
        Option.is_some
          (List.find_a_dup
             (List.map records ~f:(fun r ->
                DTO.Flow_id.to_string r.Record.result.flow.flow_id))
             ~compare:String.compare)
      then Error Error.Corrupt
      else Ok ()
    in
    records
;;

let locked t f =
  Eio.Switch.run (fun sw ->
    match S.Lock.acquire t.directory lock_name ~sw ~mode:Exclusive with
    | Error error -> Error (storage error)
    | Ok lock -> Exn.protect ~finally:(fun () -> S.Lock.release lock) ~f)
;;

let locked_wait t ~clock ~maximum_wait f =
  let seconds = Time_ns.Span.to_sec maximum_wait in
  if (not (Float.is_finite seconds)) || Float.(seconds <= 0. || seconds > 60.)
  then Error Error.Busy
  else
    Eio.Switch.run (fun sw ->
      let started = Eio.Time.Mono.now clock in
      let remaining () =
        seconds
        -. (Mtime.Span.to_float_ns (Mtime.span started (Eio.Time.Mono.now clock)) /. 1e9)
      in
      let rec acquire ~initial =
        if (not initial) && Float.(remaining () <= 0.)
        then Error Error.Busy
        else (
          match S.Lock.acquire t.directory lock_name ~sw ~mode:Exclusive with
          | Error error when S.Error.equal_code (S.Error.code error) Busy ->
            let remaining = remaining () in
            if Float.(remaining <= 0.)
            then Error Error.Busy
            else (
              Eio.Time.Mono.sleep clock (Float.min remaining 0.01);
              acquire ~initial:false)
          | Error error -> Error (storage error)
          | Ok lease -> Exn.protect ~finally:(fun () -> S.Lock.release lease) ~f)
      in
      acquire ~initial:true)
;;

let list t = locked t (fun () -> read t)

let write t records =
  let encoded =
    Jsonaf.to_string
      (`Object
          [ "version", `Number "1"
          ; "records", `Array (List.map records ~f:Record.to_json)
          ])
  in
  if String.length encoded > 512 * 1024
  then Error Error.Full
  else
    S.Directory.replace_metadata t.directory metadata_name (Bytes.of_string encoded)
    |> Result.map_error ~f:storage
;;

let begin_ t candidate =
  locked t (fun () ->
    let open Result.Let_syntax in
    let%bind records = read t in
    match
      List.find records ~f:(fun r ->
        P.Id.Principal.equal r.Record.owner candidate.Record.owner
        && P.Idempotency_key.equal r.key candidate.key)
    with
    | Some record ->
      if
        DTO.Login_mode.equal record.mode candidate.mode
        && DTO.Profile_id.equal record.result.flow.profile candidate.result.flow.profile
        && M.Id.equal record.binding candidate.binding
      then Ok record
      else Error Error.Conflict
    | None ->
      let%bind () =
        if List.length records >= t.maximum_records then Error Error.Full else Ok ()
      in
      let%bind () =
        if
          List.exists records ~f:(fun r ->
            M.Id.equal r.Record.operation candidate.operation
            || DTO.Flow_id.equal r.result.flow.flow_id candidate.result.flow.flow_id)
        then Error Error.Conflict
        else Ok ()
      in
      let%bind () =
        if M.Id.equal candidate.incarnation t.incarnation
        then Ok ()
        else Error Error.Conflict
      in
      let%map () = write t (records @ [ candidate ]) in
      candidate)
;;

let find t flow =
  list t
  |> Result.bind ~f:(fun records ->
    List.find records ~f:(fun r -> equal_flow r.Record.result.flow flow)
    |> Result.of_option ~error:Error.Missing)
;;

let change_phase t flow ~phase ~reconcile ~with_lock =
  with_lock (fun () ->
    let open Result.Let_syntax in
    let%bind records = read t in
    let%bind record =
      List.find records ~f:(fun r -> equal_flow r.Record.result.flow flow)
      |> Result.of_option ~error:Error.Missing
    in
    let%bind () =
      if DTO.Flow_result.equal_phase record.result.phase phase
      then Ok ()
      else (
        match record.result.phase, phase with
        | Pending, _ -> Ok ()
        | ( (Interrupted | Failed Submission_uncertain)
          , (Completed | Cancelled | Failed _ | Expired) )
          when reconcile -> Ok ()
        | _ -> Error Error.Conflict)
    in
    let updated = { record with result = { record.result with phase } } in
    let%map () =
      write
        t
        (List.map records ~f:(fun r ->
           if equal_flow r.Record.result.flow flow then updated else r))
    in
    updated)
;;

let set_phase t flow ~phase =
  change_phase t flow ~phase ~reconcile:false ~with_lock:(locked t)
;;

let set_phase_wait t flow ~phase ~clock ~maximum_wait =
  change_phase
    t
    flow
    ~phase
    ~reconcile:false
    ~with_lock:(locked_wait t ~clock ~maximum_wait)
;;

let reconcile_phase t flow ~phase =
  change_phase t flow ~phase ~reconcile:true ~with_lock:(locked t)
;;

let flow_lock_name (flow : DTO.Flow_ref.t) =
  S.Name.create ("operator-flow-" ^ DTO.Flow_id.to_string flow.flow_id ^ ".lock")
  |> Result.map_error ~f:storage
;;

let claim t flow ~sw =
  flow_lock_name flow
  |> Result.bind ~f:(fun name ->
    S.Lock.acquire t.directory name ~sw ~mode:Exclusive |> Result.map_error ~f:storage)
;;

let is_live t flow =
  Eio.Switch.run (fun sw ->
    match claim t flow ~sw with
    | Error Error.Busy -> Ok true
    | Error error -> Error error
    | Ok lease ->
      S.Lock.release lease;
      Ok false)
;;
