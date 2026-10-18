open! Core
module P = Agent_protocol
module Store = Agent_store.Developer_run_store
module Captured = Chatml_source
module Trace = Chatml_trace

let denied () =
  P.Error.create
    P.Error.Permission_denied
    ~message:"trace read denied"
    ~retryable:false
    ()
;;

let invalid message = P.Error.create P.Error.Invalid_request ~message ~retryable:false ()
let limited message = P.Error.create P.Error.Resource_limit ~message ~retryable:false ()

let add_bounded current amount ~limit =
  if amount < 0 || current < 0 || current > limit || amount > limit - current
  then None
  else Some (current + amount)
;;

(* Validated command IDs and keys are at most 256 bytes, typed owner IDs at
   most 96, digests exactly 64, and execution IDs at most 128. Reserve space
   for one maximal association even when every recorder limit is selected. *)
let association_fixed_charge = 128
let expired_fixed_charge = 128

let max_association_and_token_charge =
  association_fixed_charge
  + 256 (* receipt ID *)
  + 256 (* idempotency key *)
  + (3 * 96) (* host, run, principal IDs *)
  + (2 * 64) (* capture and request digests *)
  + 16 (* kind and epoch scalar *)
  + 128 (* execution ID token *)
  + expired_fixed_charge
;;

module Limits = struct
  type t =
    { max_traces : int
    ; max_retained_bytes : int
    ; max_source_refs_per_trace : int
    ; max_provenance_bytes_per_trace : int
    ; max_expired_tombstones : int
    ; max_age_seconds : float
    ; max_header_bytes : int
    ; recorder : Trace.Recorder.Limits.t
    }

  let create
        ~max_traces
        ~max_retained_bytes
        ~max_source_refs_per_trace
        ~max_provenance_bytes_per_trace
        ~max_expired_tombstones
        ~max_age_seconds
        ~max_header_bytes
        ~recorder
    =
    let minimum =
      Option.bind
        (add_bounded
           0
           (Trace.Recorder.Limits.max_normal_bytes recorder)
           ~limit:Int.max_value)
        ~f:(fun charge ->
          add_bounded
            charge
            (Trace.Recorder.Limits.max_terminal_bytes recorder)
            ~limit:Int.max_value)
      |> Option.bind ~f:(fun charge ->
        add_bounded charge max_header_bytes ~limit:Int.max_value)
      |> Option.bind ~f:(fun charge ->
        add_bounded charge max_provenance_bytes_per_trace ~limit:Int.max_value)
      |> Option.bind ~f:(fun charge ->
        add_bounded charge max_association_and_token_charge ~limit:Int.max_value)
    in
    if
      max_traces <= 0
      || max_retained_bytes <= 0
      || max_source_refs_per_trace <= 0
      || max_provenance_bytes_per_trace <= 0
      || max_expired_tombstones <= 0
      || max_header_bytes <= 0
      || (not (Float.is_finite max_age_seconds))
      || Float.(max_age_seconds <= 0.)
      || not
           (Option.value_map minimum ~default:false ~f:(fun charge ->
              charge <= max_retained_bytes))
    then Or_error.error_string "invalid developer trace retention limits"
    else
      Ok
        { max_traces
        ; max_retained_bytes
        ; max_source_refs_per_trace
        ; max_provenance_bytes_per_trace
        ; max_expired_tombstones
        ; max_age_seconds
        ; max_header_bytes
        ; recorder
        }
  ;;
end

module Provenance = struct
  type action =
    | Command of Store.Command.Receipt.Kind.t
    | Repl_statement

  type source_ref =
    { id : Captured.Id.t
    ; digest : Captured.Digest.t
    ; byte_length : int
    }

  type t =
    { action : action
    ; original_principal_id : P.Id.Principal.t
    ; original_authentication_kind : string
    ; route : Developer_command_original.Route.t
    ; capture_fingerprint : Captured.Digest.t option
    ; selection_fingerprint : Captured.Digest.t
    ; sources : source_ref list
    ; charge : int
    }

  let route t = t.route
  let original_principal_id t = t.original_principal_id
  let original_authentication_kind t = t.original_authentication_kind
  let selection_fingerprint t = t.selection_fingerprint
  let action t = t.action

  let charge_source source ~limit =
    add_bounded 96 (String.length (Captured.Id.package_namespace source.id)) ~limit
    |> Option.bind ~f:(fun charge ->
      add_bounded charge (String.length (Captured.Id.relative_path source.id)) ~limit)
  ;;

  let add_source t source ~limits =
    if List.length t.sources >= limits.Limits.max_source_refs_per_trace
    then Error (limited "trace source inventory limit reached")
    else if
      List.exists t.sources ~f:(fun existing -> Captured.Id.equal existing.id source.id)
    then Error (invalid "duplicate trace source identity")
    else (
      match
        Option.bind
          (charge_source source ~limit:limits.max_provenance_bytes_per_trace)
          ~f:(fun source_charge ->
            add_bounded
              t.charge
              source_charge
              ~limit:limits.max_provenance_bytes_per_trace)
      with
      | None -> Error (limited "trace source provenance limit reached")
      | Some charge -> Ok { t with sources = source :: t.sources; charge })
  ;;

  let add_captured_source t source ~limits =
    add_source
      t
      { id = Captured.id source
      ; digest = Captured.digest source
      ; byte_length = Captured.byte_length source
      }
      ~limits
  ;;

  let for_retained_task t ~capture_fingerprint ~limits =
    match t.action, t.capture_fingerprint with
    | Repl_statement, None ->
      add_bounded
        t.charge
        (String.length (Captured.Digest.to_hex capture_fingerprint))
        ~limit:limits.Limits.max_provenance_bytes_per_trace
      |> Option.map ~f:(fun charge ->
        { t with
          action = Command Store.Command.Receipt.Kind.Repl_task_run
        ; capture_fingerprint = Some capture_fingerprint
        ; charge
        })
    | Command _, _ | Repl_statement, Some _ -> None
  ;;

  let of_prepared
        ~action
        ~original_actor
        ~route
        ~capture_fingerprint
        ~selection_fingerprint
        ~prepared
        ~limits
    =
    let principal = Operator_authorization.principal original_actor in
    let route_bytes =
      Developer_command_original.Identity.route_json route
      |> Jsonaf.to_string
      |> String.length
    in
    let base =
      add_bounded
        192
        (String.length (P.Id.Principal.to_string principal.id))
        ~limit:limits.Limits.max_provenance_bytes_per_trace
      |> Option.bind ~f:(fun charge ->
        add_bounded
          charge
          (String.length principal.authentication_kind)
          ~limit:limits.max_provenance_bytes_per_trace)
      |> Option.bind ~f:(fun charge ->
        add_bounded charge route_bytes ~limit:limits.max_provenance_bytes_per_trace)
    in
    if
      Option.is_none base
      ||
      match action, capture_fingerprint with
      | Command _, None | Repl_statement, Some _ -> true
      | Command _, Some _ | Repl_statement, None -> false
    then Error (limited "trace provenance limit reached")
    else (
      let base = Option.value_exn base in
      List.fold_result
        (Chatml_prepared_program.sources prepared)
        ~init:
          { action
          ; original_principal_id = principal.id
          ; original_authentication_kind = principal.authentication_kind
          ; route
          ; capture_fingerprint
          ; selection_fingerprint
          ; sources = []
          ; charge = base
          }
        ~f:(fun t source -> add_captured_source t source ~limits))
  ;;

  let contains_site t location =
    let span = Captured.Location.span location in
    List.exists t.sources ~f:(fun source ->
      Captured.Id.equal source.id (Captured.Location.source location)
      && Captured.Digest.equal source.digest (Captured.Location.source_digest location)
      && span.left.offset >= 0
      && span.right.offset >= span.left.offset
      && span.right.offset <= source.byte_length)
  ;;
end

module Association = struct
  type t =
    | Command of
        { id : Store.Command.Receipt.Id.t
        ; key : P.Idempotency_key.t
        ; owner : P.Execution_owner.developer
        ; capture_fingerprint : Captured.Digest.t
        ; request_fingerprint : Captured.Digest.t
        ; kind : Store.Command.Receipt.Kind.t
        }
    | Repl_statement of
        { run_id : P.Id.Developer_run.t
        ; instance_nonce : P.Id.Transaction.t
        ; epoch_serial : int
        ; key : P.Idempotency_key.t
        ; selection_fingerprint : Captured.Digest.t
        }

  let charge t =
    let lengths =
      match t with
      | Command value ->
        [ String.length (Store.Command.Receipt.Id.to_string value.id)
        ; String.length (P.Idempotency_key.to_string value.key)
        ; String.length (P.Id.Server.to_string value.owner.host_id)
        ; String.length (P.Id.Developer_run.to_string value.owner.run_id)
        ; String.length (P.Id.Principal.to_string value.owner.principal_id)
        ; String.length (Captured.Digest.to_hex value.capture_fingerprint)
        ; String.length (Captured.Digest.to_hex value.request_fingerprint)
        ; 16 (* kind and epoch scalar *)
        ]
      | Repl_statement value ->
        [ String.length (P.Id.Developer_run.to_string value.run_id)
        ; String.length (P.Id.Transaction.to_string value.instance_nonce)
        ; String.length (P.Idempotency_key.to_string value.key)
        ; String.length (Captured.Digest.to_hex value.selection_fingerprint)
        ; 8 (* epoch scalar *)
        ]
    in
    List.fold lengths ~init:(Some association_fixed_charge) ~f:(fun charge length ->
      Option.bind charge ~f:(fun charge -> add_bounded charge length ~limit:Int.max_value))
  ;;

  let of_reserved_row row =
    match Store.Command.Row.phase row with
    | Reserved ->
      Ok
        (Command
           { id = Store.Command.Row.id row
           ; key = Store.Command.Row.key row
           ; owner = Store.Command.Row.owner row
           ; capture_fingerprint = Store.Command.Row.capture_fingerprint row
           ; request_fingerprint = Store.Command.Row.request_fingerprint row
           ; kind = Store.Command.Row.kind row
           })
    | Waiting_permission | Execution_started | Terminal _ ->
      Error (invalid "trace association requires a reserved command")
  ;;

  let repl_statement ~run_id ~instance_nonce ~epoch_serial ~key ~selection_fingerprint =
    if epoch_serial < 0
    then Error (invalid "invalid trace epoch")
    else
      Ok
        (Repl_statement
           { run_id; instance_nonce; epoch_serial; key; selection_fingerprint })
  ;;

  let matches_row t row =
    match t with
    | Repl_statement _ -> false
    | Command stored ->
      Store.Command.Receipt.Id.equal stored.id (Store.Command.Row.id row)
      && P.Idempotency_key.equal stored.key (Store.Command.Row.key row)
      && P.Execution_owner.equal_developer stored.owner (Store.Command.Row.owner row)
      && Captured.Digest.equal
           stored.capture_fingerprint
           (Store.Command.Row.capture_fingerprint row)
      && Captured.Digest.equal
           stored.request_fingerprint
           (Store.Command.Row.request_fingerprint row)
      && Store.Command.Receipt.Kind.equal stored.kind (Store.Command.Row.kind row)
  ;;
end

module Entry = struct
  type t =
    { owner : unit ref
    ; execution_id : Trace.Execution_id.t
    ; association : Association.t
    }

  let association t = t.association
  let execution_id t = t.execution_id
end

module Availability = struct
  type t =
    | Unavailable
    | Expired
    | Retained of Trace.Recorder.Snapshot.t
end

module Read_policy = struct
  type t = { authorize : Operator_authorization.t -> Provenance.t -> bool }

  let create ~authorize = { authorize }
  let deny_all = { authorize = (fun _ _ -> false) }
end

module Read_request = struct
  type t =
    { owner : unit ref
    ; actor : Operator_authorization.t
    ; entry : Entry.t
    }
end

module Configuration = struct
  type t =
    { mode : Trace.Recorder.Mode.t
    ; limits : Limits.t
    ; read_policy : Read_policy.t
    }

  let create ~mode ~limits ~read_policy = { mode; limits; read_policy }
end

type retained =
  { entry : Entry.t
  ; provenance : Provenance.t
  ; mutable recorder : Trace.Recorder.t option
  ; mutable terminal : Trace.Terminal.t option
  ; born : Mtime.t
  ; ordinal : int
  ; mutable charge : int
  ; expired_charge : int
  }

type t =
  { owner : unit ref
  ; clock : Eio.Time.Mono.ty Eio.Time.Mono.t
  ; configuration : Configuration.t
  ; entries : (string, retained) Hashtbl.t
  ; mutable retained_bytes : int
  ; mutable expired_count : int
  ; mutable next_ordinal : int
  ; mutable closed : bool
  ; mutable release_hook : Eio.Switch.hook
  }

let token entry = Trace.Execution_id.to_opaque_string entry.Entry.execution_id

let release_recording t retained =
  if Option.is_some retained.recorder
  then (
    let charge = retained.expired_charge in
    t.retained_bytes <- t.retained_bytes - retained.charge + charge;
    retained.charge <- charge;
    retained.recorder <- None;
    retained.terminal <- None;
    t.expired_count <- t.expired_count + 1)
;;

let remove_retained t retained =
  t.retained_bytes <- t.retained_bytes - retained.charge;
  if Option.is_none retained.recorder then t.expired_count <- t.expired_count - 1;
  Hashtbl.remove t.entries (token retained.entry)
;;

let age_seconds t retained =
  Mtime.span retained.born (Eio.Time.Mono.now t.clock)
  |> Mtime.Span.to_float_ns
  |> fun nanoseconds -> nanoseconds /. 1e9
;;

let oldest t ~filter =
  Hashtbl.data t.entries
  |> List.filter ~f:filter
  |> List.min_elt ~compare:(fun a b ->
    match Mtime.compare a.born b.born with
    | 0 -> Int.compare a.ordinal b.ordinal
    | order -> order)
;;

let rec trim_expired t =
  if t.expired_count > t.configuration.limits.max_expired_tombstones
  then (
    match oldest t ~filter:(fun value -> Option.is_none value.recorder) with
    | None -> ()
    | Some value ->
      remove_retained t value;
      trim_expired t)
;;

let sweep t =
  Hashtbl.data t.entries
  |> List.iter ~f:(fun retained ->
    if
      Float.(age_seconds t retained >= t.configuration.limits.max_age_seconds)
      && Option.is_some retained.terminal
    then release_recording t retained);
  trim_expired t
;;

let close t =
  if not t.closed
  then (
    t.closed <- true;
    ignore (Eio.Switch.try_remove_hook t.release_hook : bool);
    Hashtbl.clear t.entries;
    t.retained_bytes <- 0;
    t.expired_count <- 0)
;;

let create ~sw ~clock ~configuration =
  let owner =
    { owner = ref ()
    ; clock
    ; configuration
    ; entries = Hashtbl.create (module String)
    ; retained_bytes = 0
    ; expired_count = 0
    ; next_ordinal = 0
    ; closed = false
    ; release_hook = Eio.Switch.null_hook
    }
  in
  owner.release_hook <- Eio.Switch.on_release_cancellable sw (fun () -> close owner);
  owner
;;

let limits t = t.configuration.limits

let match_provenance association provenance =
  match association, Provenance.action provenance with
  | Association.Command command, Command kind ->
    Store.Command.Receipt.Kind.equal command.kind kind
    && P.Id.Principal.equal
         command.owner.principal_id
         (Provenance.original_principal_id provenance)
    && Option.value_map
         provenance.capture_fingerprint
         ~default:false
         ~f:(Captured.Digest.equal command.capture_fingerprint)
  | Repl_statement statement, Repl_statement ->
    Captured.Digest.equal
      statement.selection_fingerprint
      (Provenance.selection_fingerprint provenance)
  | Command _, Repl_statement | Repl_statement _, Command _ -> false
;;

let inventory_matches provenance header =
  List.for_all (Trace.Header.source_inventory header) ~f:(fun source ->
    List.exists provenance.Provenance.sources ~f:(fun retained ->
      Captured.Id.equal retained.id (Trace.Header.Source_ref.id source)
      && Captured.Digest.equal retained.digest (Trace.Header.Source_ref.digest source)
      && Int.equal retained.byte_length (Trace.Header.Source_ref.byte_length source)))
;;

let source_matches provenance source =
  List.exists provenance.Provenance.sources ~f:(fun retained ->
    Captured.Id.equal retained.id (Captured.id source)
    && Captured.Digest.equal retained.digest (Captured.digest source)
    && Int.equal retained.byte_length (Captured.byte_length source))
;;

let make_entry t association provenance ~submission_fingerprint ~header =
  if not (match_provenance association provenance && inventory_matches provenance header)
  then None
  else if
    not
      (Captured.Digest.equal
         (Trace.Header.submission_fingerprint header)
         submission_fingerprint)
  then None
  else if
    Hashtbl.mem
      t.entries
      (Trace.Execution_id.to_opaque_string (Trace.Header.execution_id header))
    || Int.equal t.next_ordinal Int.max_value
  then None
  else (
    let limits = t.configuration.limits in
    let expired_charge =
      Option.bind (Association.charge association) ~f:(fun value ->
        add_bounded value provenance.Provenance.charge ~limit:limits.max_retained_bytes)
      |> Option.bind ~f:(fun value ->
        add_bounded
          value
          (String.length
             (Trace.Execution_id.to_opaque_string (Trace.Header.execution_id header)))
          ~limit:limits.max_retained_bytes)
      |> Option.bind ~f:(fun value ->
        add_bounded value expired_fixed_charge ~limit:limits.max_retained_bytes)
    in
    let charge =
      Option.bind expired_charge ~f:(fun value ->
        add_bounded
          value
          (Trace.Recorder.Limits.max_normal_bytes limits.recorder)
          ~limit:limits.max_retained_bytes)
      |> Option.bind ~f:(fun value ->
        add_bounded
          value
          (Trace.Recorder.Limits.max_terminal_bytes limits.recorder)
          ~limit:limits.max_retained_bytes)
      |> Option.bind ~f:(fun value ->
        add_bounded value limits.max_header_bytes ~limit:limits.max_retained_bytes)
    in
    sweep t;
    let rec make_room charge =
      if
        Hashtbl.length t.entries < limits.max_traces
        && charge <= limits.max_retained_bytes - t.retained_bytes
      then true
      else (
        match oldest t ~filter:(fun value -> Option.is_none value.recorder) with
        | Some value ->
          remove_retained t value;
          make_room charge
        | None ->
          (match oldest t ~filter:(fun value -> Option.is_some value.terminal) with
           | None -> false
           | Some value ->
             release_recording t value;
             trim_expired t;
             make_room charge))
    in
    match charge, expired_charge with
    | None, _ | _, None -> None
    | Some charge, Some _ when not (make_room charge) -> None
    | Some charge, Some expired_charge ->
      let entry =
        { Entry.owner = t.owner
        ; execution_id = Trace.Header.execution_id header
        ; association
        }
      in
      let recorder =
        Trace.Recorder.create header ~mode:t.configuration.mode ~limits:limits.recorder
      in
      let retained =
        { entry
        ; provenance
        ; recorder = Some recorder
        ; terminal = None
        ; born = Eio.Time.Mono.now t.clock
        ; ordinal = t.next_ordinal
        ; charge
        ; expired_charge
        }
      in
      Hashtbl.set t.entries ~key:(token entry) ~data:retained;
      t.next_ordinal <- t.next_ordinal + 1;
      t.retained_bytes <- t.retained_bytes + charge;
      Some entry)
;;

let new_execution_id () =
  Trace.Execution_id.of_opaque_string
    ("developer-trace." ^ P.Id.Transaction.to_string (P.Id.Transaction.create ()))
;;

let begin_prepared
      t
      ~association
      ~provenance
      ~submission_fingerprint
      ~prepared
      ~checkpoint
  =
  if
    t.closed
    || (not (match_provenance association provenance))
    || not
         (List.for_all
            (Chatml_prepared_program.sources prepared)
            ~f:(source_matches provenance))
  then None
  else (
    match new_execution_id () with
    | Error _ -> None
    | Ok execution_id ->
      (match
         Trace.Header.of_prepared_graph
           ~execution_id
           ~submission_fingerprint
           ~prepared
           ~selection_fingerprint:(Provenance.selection_fingerprint provenance)
           ~max_inventory_entries:t.configuration.limits.max_source_refs_per_trace
           ~max_header_bytes:t.configuration.limits.max_header_bytes
           ~checkpoint
       with
       | Error _ -> None
       | Ok header -> make_entry t association provenance ~submission_fingerprint ~header))
;;

let begin_submitted t ~association ~provenance ~submission_fingerprint ~source ~checkpoint
  =
  if
    t.closed
    || (not (match_provenance association provenance))
    || not (source_matches provenance source)
  then None
  else (
    match new_execution_id () with
    | Error _ -> None
    | Ok execution_id ->
      (match
         Trace.Header.of_submitted_capture
           ~execution_id
           ~submission_fingerprint
           ~sources:[ source ]
           ~selection_fingerprint:(Provenance.selection_fingerprint provenance)
           ~max_inventory_entries:t.configuration.limits.max_source_refs_per_trace
           ~max_header_bytes:t.configuration.limits.max_header_bytes
           ~checkpoint
       with
       | Error _ -> None
       | Ok header -> make_entry t association provenance ~submission_fingerprint ~header))
;;

let find t entry =
  if t.closed || not (phys_equal t.owner entry.Entry.owner)
  then None
  else
    Hashtbl.find t.entries (token entry)
    |> Option.filter ~f:(fun retained -> phys_equal retained.entry entry)
;;

let find_command_entry t row =
  if t.closed
  then None
  else
    Hashtbl.data t.entries
    |> List.find_map ~f:(fun retained ->
      if Association.matches_row (Entry.association retained.entry) row
      then Some retained.entry
      else None)
;;

let within t entry f =
  let recorder =
    Option.bind entry ~f:(fun entry ->
      Option.bind (find t entry) ~f:(fun value -> value.recorder))
  in
  Trace.Scope.within recorder f
;;

let finish t entry terminal =
  Option.iter (find t entry) ~f:(fun retained ->
    match retained.recorder, retained.terminal with
    | Some recorder, None ->
      (match Trace.Recorder.finish recorder terminal with
       | Ok () -> retained.terminal <- Some terminal
       | Error _ -> ())
    | Some _, Some previous
      when Int.equal
             (Trace.Terminal.receipt_version previous)
             (Trace.Terminal.receipt_version terminal)
           && Trace.Terminal.Disposition.equal
                (Trace.Terminal.disposition previous)
                (Trace.Terminal.disposition terminal) -> ()
    | Some _, Some _ -> release_recording t retained
    | None, _ -> ());
  sweep t
;;

let begin_read t ~actor entry =
  if
    t.closed
    || (not (phys_equal t.owner entry.Entry.owner))
    || not (Operator_authorization.is_current actor)
  then Error (denied ())
  else Ok { Read_request.owner = t.owner; actor; entry }
;;

let same_actor principal actor =
  let current = Operator_authorization.principal actor in
  P.Id.Principal.equal current.id principal.P.Principal.id
  && String.equal current.authentication_kind principal.authentication_kind
;;

let snapshot t request ~current_actor ~exact_membership =
  if
    t.closed
    || (not (phys_equal t.owner request.Read_request.owner))
    || (not (Operator_authorization.is_current request.actor))
    || (not (Operator_authorization.is_current current_actor))
    || (not (same_actor (Operator_authorization.principal request.actor) current_actor))
    || not (exact_membership (Entry.association request.entry))
  then Error (denied ())
  else (
    sweep t;
    match find t request.entry with
    | None -> Ok Availability.Unavailable
    | Some retained ->
      let provenance = retained.provenance in
      if
        (not
           (P.Id.Principal.equal
              (Provenance.original_principal_id provenance)
              (Operator_authorization.principal current_actor).id))
        || (not
              (String.equal
                 (Provenance.original_authentication_kind provenance)
                 (Operator_authorization.principal current_actor).authentication_kind))
        || (not (t.configuration.read_policy.authorize current_actor provenance))
        || (not (Operator_authorization.is_current request.actor))
        || (not (Operator_authorization.is_current current_actor))
        || not (exact_membership (Entry.association request.entry))
      then Error (denied ())
      else (
        match retained.recorder with
        | None -> Ok Availability.Expired
        | Some recorder ->
          let snapshot = Trace.Recorder.snapshot recorder in
          Ok
            (Availability.Retained
               (Trace.Recorder.Snapshot.with_authorized_sites
                  snapshot
                  ~authorize:(Provenance.contains_site provenance)))))
;;
