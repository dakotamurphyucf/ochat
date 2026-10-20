open! Core
module P = Agent_protocol
module Store = Agent_store.Developer_run_store
module Captured = Chatml_source
module Trace = Chatml_trace
module Historical = Developer_historical_source

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
   most 96, digests exactly 64, and execution IDs at most 128. The fixed
   association charge includes its variant/record, owner record, string
   headers and option cells. Reserve one maximal association even when every
   recorder limit is selected; these are conservative retention charges, not
   exact heap-size measurements. *)
let association_fixed_charge = 384
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
    ; historical_origin : Historical.Origin.t
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
    (* Source-ref/list cells, ID record and string headers, and the retained
       64-character digest precede the variable namespace/path bytes. *)
    add_bounded 256 (String.length (Captured.Id.package_namespace source.id)) ~limit
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
      (* Charge the added option/digest header as well as its hex bytes. *)
      add_bounded
        t.charge
        (128 + String.length (Captured.Digest.to_hex capture_fingerprint))
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
        ~historical_origin
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
    (* Record, route object, option cells and both digest/string headers are
       charged here; variable actor/route/origin/source bytes follow. *)
    let base =
      add_bounded
        512
        (String.length (P.Id.Principal.to_string principal.id))
        ~limit:limits.Limits.max_provenance_bytes_per_trace
      |> Option.bind ~f:(fun charge ->
        add_bounded
          charge
          (String.length principal.authentication_kind)
          ~limit:limits.max_provenance_bytes_per_trace)
      |> Option.bind ~f:(fun charge ->
        add_bounded charge route_bytes ~limit:limits.max_provenance_bytes_per_trace)
      |> Option.bind ~f:(fun charge ->
        Historical.Origin.charge
          historical_origin
          ~max_bytes:(limits.max_provenance_bytes_per_trace - charge)
        |> Option.bind ~f:(fun origin_charge ->
          add_bounded charge origin_charge ~limit:limits.max_provenance_bytes_per_trace))
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
          ; historical_origin
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
  type t =
    | Authoring of Authoring_installation.t
    | Deny_all

  let of_authoring authoring = Authoring authoring
  let deny_all = Deny_all

  let check_full t ~actor provenance ~checkpoint =
    match t with
    | Deny_all -> Error (denied ())
    | Authoring owner ->
      Authoring_installation.check_historical_origin
        owner
        ~actor
        provenance.Provenance.historical_origin
        ~checkpoint
  ;;

  let check_now t ~actor provenance =
    match t with
    | Deny_all -> Error (denied ())
    | Authoring owner ->
      Authoring_installation.check_historical_origin_now
        owner
        ~actor
        provenance.Provenance.historical_origin
  ;;
end

module Read_request = struct
  type loan =
    { entry : Entry.t
    ; mutable provenance : Provenance.t option
    ; mutable readers : int
    ; mutable detached : bool
    }

  type active =
    { actor : Operator_authorization.t
    ; loan : loan option
    ; mutable checked_actor : Operator_authorization.t option
    ; mutable proof : unit ref option
    }

  type state =
    | Active of active
    | Released

  type t =
    { owner : unit ref
    ; entry : Entry.t
    ; mutable state : state
    }

  type checked =
    { request : t
    ; proof : unit ref
    }

  let provenance active = Option.bind active.loan ~f:(fun loan -> loan.provenance)
end

module Configuration = struct
  type t =
    { mode : Trace.Recorder.Mode.t
    ; limits : Limits.t
    ; read_policy : Read_policy.t
    ; historical_recording : Historical.Recording.t
    }

  let create ~mode ~limits ~read_policy =
    let historical_recording =
      Historical.Recording.create
        ~max_origin_bytes:limits.Limits.max_provenance_bytes_per_trace
      |> Or_error.ok_exn
    in
    { mode; limits; read_policy; historical_recording }
  ;;

  let historical_recording t = t.historical_recording
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
  ; mutable active_reads : int
  ; mutable borrowed_bytes : int
  ; mutable loans : Read_request.loan list
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
  List.iter t.loans ~f:(fun loan ->
    if phys_equal loan.Read_request.entry retained.entry && not loan.detached
    then (
      Option.iter loan.provenance ~f:(fun provenance ->
        t.borrowed_bytes <- t.borrowed_bytes + provenance.Provenance.charge);
      loan.detached <- true));
  t.retained_bytes <- t.retained_bytes - retained.charge;
  assert (
    t.borrowed_bytes >= 0
    && t.retained_bytes >= 0
    && t.borrowed_bytes <= t.configuration.limits.max_retained_bytes - t.retained_bytes);
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
    List.iter t.loans ~f:(fun loan -> loan.provenance <- None);
    t.loans <- [];
    t.retained_bytes <- 0;
    t.borrowed_bytes <- 0;
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
    ; active_reads = 0
    ; borrowed_bytes = 0
    ; loans = []
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
        && charge <= limits.max_retained_bytes - t.retained_bytes - t.borrowed_bytes
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

let same_actor principal actor =
  let current = Operator_authorization.principal actor in
  P.Id.Principal.equal current.id principal.P.Principal.id
  && String.equal current.authentication_kind principal.authentication_kind
;;

open Read_request

let request_current t request ~current_actor =
  (not t.closed)
  && phys_equal t.owner request.Read_request.owner
  && phys_equal t.owner request.entry.Entry.owner
  &&
  match request.state with
  | Released -> false
  | Active active ->
    Operator_authorization.is_current active.actor
    && Operator_authorization.is_current current_actor
    && same_actor (Operator_authorization.principal active.actor) current_actor
    && Option.value_map
         (Read_request.provenance active)
         ~default:true
         ~f:(fun provenance ->
           let principal = Operator_authorization.principal current_actor in
           P.Id.Principal.equal provenance.Provenance.original_principal_id principal.id
           && String.equal
                provenance.original_authentication_kind
                principal.authentication_kind)
;;

let with_read t ~actor entry ~f =
  let limits = t.configuration.limits in
  let max_reads =
    Int.min
      limits.max_traces
      (limits.max_retained_bytes / limits.max_provenance_bytes_per_trace)
  in
  if
    t.closed
    || (not (phys_equal t.owner entry.Entry.owner))
    || not (Operator_authorization.is_current actor)
  then Error (denied ())
  else if t.active_reads >= max_reads
  then Error (limited "trace read capacity reached")
  else (
    sweep t;
    let retained = find t entry in
    let loan =
      Option.map retained ~f:(fun value ->
        match
          List.find t.loans ~f:(fun loan -> phys_equal loan.Read_request.entry entry)
        with
        | Some loan ->
          loan.readers <- loan.readers + 1;
          loan
        | None ->
          let loan =
            { Read_request.entry
            ; provenance = Some value.provenance
            ; readers = 1
            ; detached = false
            }
          in
          t.loans <- loan :: t.loans;
          loan)
    in
    let request =
      { Read_request.owner = t.owner
      ; entry
      ; state = Active { actor; loan; checked_actor = None; proof = None }
      }
    in
    t.active_reads <- t.active_reads + 1;
    Exn.protect
      ~f:(fun () -> f request)
      ~finally:(fun () ->
        request.state <- Released;
        Option.iter loan ~f:(fun loan ->
          loan.readers <- loan.readers - 1;
          if Int.equal loan.readers 0
          then (
            if loan.detached && not t.closed
            then
              Option.iter loan.provenance ~f:(fun provenance ->
                t.borrowed_bytes <- t.borrowed_bytes - provenance.Provenance.charge);
            loan.provenance <- None;
            t.loans <- List.filter t.loans ~f:(fun other -> not (phys_equal other loan))));
        t.active_reads <- t.active_reads - 1))
;;

let check_read_current t request ~current_actor ~checkpoint =
  let open Result.Let_syntax in
  if not (request_current t request ~current_actor)
  then Error (denied ())
  else (
    let%bind () =
      match request.state with
      | Released -> Error (denied ())
      | Active active ->
        (match Read_request.provenance active with
         | None -> Ok ()
         | Some provenance ->
           Read_policy.check_full
             t.configuration.read_policy
             ~actor:current_actor
             provenance
             ~checkpoint)
    in
    if not (request_current t request ~current_actor)
    then Error (denied ())
    else (
      let%bind () =
        match request.state with
        | Released -> Error (denied ())
        | Active active ->
          (match Read_request.provenance active with
           | None -> Ok ()
           | Some provenance ->
             Read_policy.check_now
               t.configuration.read_policy
               ~actor:current_actor
               provenance)
      in
      match request.state with
      | Released -> Error (denied ())
      | Active active ->
        let proof = ref () in
        active.checked_actor <- Some current_actor;
        active.proof <- Some proof;
        Ok { Read_request.request; proof }))
;;

let snapshot_checked t request ~checked ~current_actor ~exact_membership =
  let proof_current =
    match request.Read_request.state with
    | Released -> false
    | Active active ->
      Option.value_map active.proof ~default:false ~f:(phys_equal checked.proof)
      && Option.value_map
           active.checked_actor
           ~default:false
           ~f:(phys_equal current_actor)
  in
  if
    (not (request_current t request ~current_actor))
    || (not (phys_equal checked.Read_request.request request))
    || (not proof_current)
    || not (exact_membership (Entry.association request.entry))
  then Error (denied ())
  else (
    let source_current =
      match request.state with
      | Released -> Error (denied ())
      | Active active ->
        (match Read_request.provenance active with
         | None -> Ok ()
         | Some provenance ->
           Read_policy.check_now
             t.configuration.read_policy
             ~actor:current_actor
             provenance)
    in
    match source_current with
    | Error _ as error -> error
    | Ok () ->
      if
        (not (request_current t request ~current_actor))
        || not (exact_membership (Entry.association request.entry))
      then Error (denied ())
      else (
        sweep t;
        match find t request.entry with
        | None -> Ok Availability.Unavailable
        | Some retained ->
          let provenance = retained.provenance in
          if
            not
              (match request.state with
               | Released -> false
               | Active active ->
                 Option.value_map
                   (Read_request.provenance active)
                   ~default:false
                   ~f:(phys_equal provenance))
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
                      ~authorize:(Provenance.contains_site provenance))))))
;;

let%expect_test "bounded historical read loan releases on cancellation and lexical exit" =
  let must = function
    | Ok value -> value
    | Error _ -> failwith "trace read fixture setup failed"
  in
  let recorder =
    Trace.Recorder.Limits.create
      ~max_events:4
      ~max_normal_bytes:256
      ~max_terminal_bytes:128
    |> Or_error.ok_exn
  in
  let limits =
    Limits.create
      ~max_traces:2
      ~max_retained_bytes:
        (Trace.Recorder.Limits.max_normal_bytes recorder
         + Trace.Recorder.Limits.max_terminal_bytes recorder
         + 512
         + 1024
         + max_association_and_token_charge)
      ~max_source_refs_per_trace:1
      ~max_provenance_bytes_per_trace:1024
      ~max_expired_tombstones:1
      ~max_age_seconds:60.
      ~max_header_bytes:512
      ~recorder
    |> Or_error.ok_exn
  in
  let actor =
    P.Principal.create
      ~id:(P.Id.Principal.of_string "pri_trace_loan" |> must)
      ~authentication_kind:"local.test"
      ~scopes:P.Scope.Set.empty
      ~attributes:[]
    |> must
    |> Operator_authorization.trusted_local
  in
  Eio_main.run (fun env ->
    Eio.Switch.run (fun sw ->
      let t =
        create
          ~sw
          ~clock:(Eio.Stdenv.mono_clock env)
          ~configuration:
            (Configuration.create
               ~mode:Trace.Recorder.Mode.Summary
               ~limits
               ~read_policy:Read_policy.deny_all)
      in
      (* The entry is deliberately absent: this case tests only the finite loan
       owner, not historical source admission or recorder publication. *)
      let entry =
        { Entry.owner = t.owner
        ; execution_id = Trace.Execution_id.of_opaque_string "trace-loan" |> must
        ; association =
            Association.repl_statement
              ~run_id:(P.Id.Developer_run.of_string "dvr_trace_loan" |> must)
              ~instance_nonce:(P.Id.Transaction.of_string "txn_trace_loan" |> must)
              ~epoch_serial:0
              ~key:(P.Idempotency_key.of_string "trace-loan" |> must)
              ~selection_fingerprint:
                (Captured.Digest.of_contents "selection" ~checkpoint:Fn.id)
            |> must
        }
      in
      let reason = Failure "cancel trace read" in
      let cancelled =
        match
          Eio.Cancel.sub (fun cancel ->
            with_read t ~actor entry ~f:(fun _ ->
              with_read t ~actor entry ~f:(fun _ ->
                (match with_read t ~actor entry ~f:(fun _ -> Ok ()) with
                 | Error { code = Resource_limit; _ } -> ()
                 | Ok () | Error _ -> failwith "read slot was not bounded");
                Ok ())
              |> must;
              Eio.Cancel.cancel cancel reason;
              Eio.Fiber.check ();
              Ok ()))
        with
        | exception Eio.Cancel.Cancelled caught -> phys_equal caught reason
        | Ok _ | Error _ -> false
      in
      assert cancelled;
      let escaped = ref None in
      let recovered =
        with_read t ~actor entry ~f:(fun request ->
          let checked =
            check_read_current t request ~current_actor:actor ~checkpoint:Fn.id |> must
          in
          escaped := Some (request, checked);
          Ok ())
      in
      assert (Result.is_ok recovered);
      let request, checked = Option.value_exn !escaped in
      assert (
        Result.is_error
          (snapshot_checked
             t
             request
             ~checked
             ~current_actor:actor
             ~exact_membership:(fun _ -> true)));
      let source_id =
        Captured.Id.create ~package_namespace:"trace-loan" ~relative_path:"entry.chatml"
        |> Or_error.ok_exn
      in
      let source =
        Captured.create
          source_id
          ~contents:"let main = 1\n"
          ~expected_digest:None
          ~max_bytes:None
          ~checkpoint:Fn.id
        |> Or_error.ok_exn
      in
      let selected = Captured.digest source in
      let policy =
        Authoring_context_service_core.Supplied_document_policy.create
          ~authorize:(fun _ -> true)
      in
      let provenance =
        { Provenance.action = Repl_statement
        ; original_principal_id = (Operator_authorization.principal actor).id
        ; original_authentication_kind = "local.test"
        ; route =
            Developer_command_original.Route.Local_source
              { root_index = 0
              ; member = Source.Portable_path.create "entry.chatml" |> Or_error.ok_exn
              }
        ; historical_origin =
            Historical.Origin.supplied_document
              (Authoring_context_service_core.Supplied_document_policy.identity policy)
        ; capture_fingerprint = None
        ; selection_fingerprint = selected
        ; sources =
            [ { Provenance.id = Captured.id source
              ; digest = Captured.digest source
              ; byte_length = Captured.byte_length source
              }
            ]
        ; charge = 1024
        }
      in
      let association key =
        Association.repl_statement
          ~run_id:(P.Id.Developer_run.of_string "dvr_trace_loan" |> must)
          ~instance_nonce:(P.Id.Transaction.of_string "txn_trace_loan" |> must)
          ~epoch_serial:0
          ~key:(P.Idempotency_key.of_string key |> must)
          ~selection_fingerprint:selected
        |> must
      in
      let admitted key execution =
        let header =
          Trace.Header.of_submitted_capture
            ~execution_id:(Trace.Execution_id.of_opaque_string execution |> must)
            ~submission_fingerprint:selected
            ~sources:[ source ]
            ~selection_fingerprint:selected
            ~max_inventory_entries:1
            ~max_header_bytes:512
            ~checkpoint:Fn.id
          |> must
        in
        make_entry t (association key) provenance ~submission_fingerprint:selected ~header
      in
      let retained_entry =
        admitted "trace-loan-first" "trace-loan-first" |> Option.value_exn
      in
      finish
        t
        retained_entry
        (Trace.Terminal.create ~receipt_version:1 ~disposition:Succeeded |> must);
      let escaped_retained = ref None in
      with_read t ~actor retained_entry ~f:(fun request ->
        escaped_retained := Some request;
        assert (Option.is_some (find t retained_entry));
        with_read t ~actor retained_entry ~f:(fun _ ->
          assert (Option.is_none (admitted "trace-loan-next" "trace-loan-next"));
          assert (Option.is_none (find t retained_entry));
          assert (Int.equal t.borrowed_bytes provenance.charge);
          Ok ())
        |> must;
        (* The second reader has exited, but the first still owns the one
           detached provenance charge. A table-only admission would fit. *)
        assert (Option.is_none (find t retained_entry));
        assert (Int.equal t.borrowed_bytes provenance.charge);
        (match request.state with
         | Active active -> assert (Option.is_some (Read_request.provenance active))
         | Released -> failwith "active read loan expired during eviction");
        Ok ())
      |> must;
      let escaped_retained = Option.value_exn !escaped_retained in
      (match escaped_retained.state with
       | Released -> ()
       | Active _ -> failwith "escaped retained loan kept provenance");
      assert (Int.equal t.borrowed_bytes 0);
      assert (Option.is_none (find t retained_entry));
      let next_entry = admitted "trace-loan-next" "trace-loan-next" |> Option.value_exn in
      with_read t ~actor next_entry ~f:(fun request ->
        close t;
        (match request.state with
         | Active active -> assert (Option.is_none (Read_request.provenance active))
         | Released -> failwith "close altered lexical read state");
        Ok ())
      |> must;
      close t;
      assert (Result.is_error (with_read t ~actor entry ~f:(fun _ -> Ok ())))));
  print_endline "finite read loan released and escaped proof expired";
  [%expect {| finite read loan released and escaped proof expired |}]
;;
