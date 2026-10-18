open Core
module Captured = Chatml_source
module Prepared = Chatml_prepared_program

(* Canonical accounting format v1 uses 64-bit words and raw length-prefixed
   strings. An ID namespace is arbitrary bytes, so no UTF-8 or escaping claim
   enters the budget. Digests are their validated 64 ASCII hex bytes. The
   OCH-273 wire adapter may use a different encoding under its own bound. *)
module Canonical_layout = struct
  let event_fixed = 6 * 8
  let site_fixed = (2 * 8) + 64 + (6 * 8)
  let source_ref_fixed = (2 * 8) + 64 + 8
  let header_fixed = (7 * 8) + (2 * 64)
  let prepared_extra = 3 * 64
  let correlation_fixed = 8
  let terminal_fixed = 8 * 8
end

module Execution_id = struct
  type t = string [@@deriving compare, equal, sexp_of]

  let valid_token value =
    let length = String.length value in
    length > 0
    && length <= 128
    && String.for_all value ~f:(function
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.' -> true
      | _ -> false)
  ;;

  let of_opaque_string value =
    if valid_token value then Ok value else Or_error.error_string "invalid execution ID"
  ;;

  let to_opaque_string t = t
end

module Header = struct
  module Source_ref = struct
    type t =
      { id : Captured.Id.t
      ; digest : Captured.Digest.t
      ; byte_length : int
      }

    let id t = t.id
    let digest t = t.digest
    let byte_length t = t.byte_length
  end

  module Admission = struct
    type t =
      | Submitted_capture
      | Prepared_graph
  end

  type t =
    { execution_id : Execution_id.t
    ; admission : Admission.t
    ; submission_fingerprint : Captured.Digest.t
    ; graph_fingerprint : Captured.Digest.t option
    ; contract_fingerprint : Captured.Digest.t option
    ; environment_fingerprint : Captured.Digest.t option
    ; selection_fingerprint : Captured.Digest.t
    ; source_inventory : Source_ref.t list
    }

  let execution_id t = t.execution_id
  let admission t = t.admission
  let submission_fingerprint t = t.submission_fingerprint
  let graph_fingerprint t = t.graph_fingerprint
  let contract_fingerprint t = t.contract_fingerprint
  let environment_fingerprint t = t.environment_fingerprint
  let selection_fingerprint t = t.selection_fingerprint
  let source_inventory t = t.source_inventory

  let freeze_sources
        sources
        ~base_bytes
        ~max_inventory_entries
        ~max_header_bytes
        ~checkpoint
    =
    if max_inventory_entries < 0 || max_header_bytes < 0
    then Or_error.error_string "invalid trace header limits"
    else (
      let count = ref 0 in
      let bytes = ref 0 in
      let add length =
        if length < 0 || length > max_header_bytes - !bytes
        then Or_error.error_string "trace header byte limit"
        else (
          bytes := !bytes + length;
          Ok ())
      in
      let open Result.Let_syntax in
      let%bind () = add base_bytes in
      let%bind rev, groups =
        List.fold_result
          sources
          ~init:([], String.Map.empty)
          ~f:(fun (rev, groups) source ->
            checkpoint ();
            if !count >= max_inventory_entries
            then Or_error.error_string "trace source inventory limit"
            else (
              count := !count + 1;
              let id = Captured.id source in
              let namespace = Captured.Id.package_namespace id in
              let path = Captured.Id.relative_path id in
              let%bind () =
                add Canonical_layout.source_ref_fixed
                >>= fun () ->
                add (String.length namespace) >>= fun () -> add (String.length path)
              in
              let%bind portable = Source.Portable_path.create path in
              let groups =
                Map.update groups namespace ~f:(fun paths ->
                  portable :: Option.value paths ~default:[])
              in
              Ok
                ( { Source_ref.id
                  ; digest = Captured.digest source
                  ; byte_length = Captured.byte_length source
                  }
                  :: rev
                , groups )))
      in
      let%map () =
        Map.fold groups ~init:(Ok ()) ~f:(fun ~key:_ ~data:paths result ->
          let%bind () = result in
          checkpoint ();
          match Source.Portable_path.first_collision paths ~checkpoint with
          | None -> Ok ()
          | Some _ -> Or_error.error_string "ambiguous trace source inventory")
      in
      List.rev rev, !bytes)
  ;;

  let of_submitted_capture
        ~execution_id
        ~submission_fingerprint
        ~sources
        ~selection_fingerprint
        ~max_inventory_entries
        ~max_header_bytes
        ~checkpoint
    =
    freeze_sources
      sources
      ~base_bytes:
        (Canonical_layout.header_fixed
         + String.length (Execution_id.to_opaque_string execution_id))
      ~max_inventory_entries
      ~max_header_bytes
      ~checkpoint
    |> Result.map ~f:(fun (source_inventory, _) ->
      { execution_id
      ; admission = Submitted_capture
      ; submission_fingerprint
      ; graph_fingerprint = None
      ; contract_fingerprint = None
      ; environment_fingerprint = None
      ; selection_fingerprint
      ; source_inventory
      })
  ;;

  let of_prepared_graph
        ~execution_id
        ~submission_fingerprint
        ~prepared
        ~selection_fingerprint
        ~max_inventory_entries
        ~max_header_bytes
        ~checkpoint
    =
    freeze_sources
      (Prepared.sources prepared)
      ~base_bytes:
        (Canonical_layout.header_fixed
         + Canonical_layout.prepared_extra
         + String.length (Execution_id.to_opaque_string execution_id))
      ~max_inventory_entries
      ~max_header_bytes
      ~checkpoint
    |> Result.bind ~f:(fun (source_inventory, _) ->
      let environment = Prepared.environment prepared in
      let environment_name = Prepared.Environment.name environment in
      if String.length environment_name > max_header_bytes
      then Or_error.error_string "trace header byte limit"
      else (
        checkpoint ();
        let environment_fingerprint =
          Captured.Digest.of_contents environment_name ~checkpoint
        in
        Ok
          { execution_id
          ; admission = Prepared_graph
          ; submission_fingerprint
          ; graph_fingerprint = Some (Prepared.fingerprint prepared)
          ; contract_fingerprint = Some (Prepared.compiler_contract prepared)
          ; environment_fingerprint = Some environment_fingerprint
          ; selection_fingerprint
          ; source_inventory
          }))
  ;;
end

module Event = struct
  module Phase = struct
    type t =
      | Preparation
      | Compilation
      | Dependency_initialization
      | Entry_initialization
      | Evaluation
      | Task
      | Host_wait
    [@@deriving equal, sexp_of]
  end

  module Operation = struct
    type t =
      | Tool_call
      | Tool_spawn
      | Job_start
      | Model_call
      | Model_spawn
      | Process_run
      | Schedule
      | Job_read
      | Local_transaction
      | Other
    [@@deriving equal, sexp_of]
  end

  module Failure = struct
    type t =
      | Parse
      | Typecheck
      | Link
      | Runtime
      | Host_operation
      | Permission
      | Resource_limit
      | Other
    [@@deriving equal, sexp_of]
  end

  module Correlation = struct
    type t = string [@@deriving equal, sexp_of]

    let of_host_token token =
      if Execution_id.valid_token token
      then Ok token
      else Or_error.error_string "invalid trace correlation"
    ;;

    let length = String.length
  end

  module Kind = struct
    type t =
      | Phase_started of Phase.t
      | Phase_finished of Phase.t
      | Statement_entered
      | Call_entered
      | Effect_attempted of Operation.t
      | Effect_host_returned of Operation.t
      | Effect_host_failed of Operation.t
      | Host_wait_entered
      | Host_wait_resumed
      | Job_admitted
      | Job_terminal
      | Recovered_failure of Failure.t
      | Unrecovered_failure of Failure.t
    [@@deriving equal, sexp_of]
  end

  type t =
    { sequence : int64
    ; kind : Kind.t
    ; site : Captured.Location.t option
    ; correlation : Correlation.t option
    }

  let sequence t = t.sequence
  let kind t = t.kind
  let site t = t.site
  let correlation t = t.correlation
end

module Terminal = struct
  module Disposition = struct
    type t =
      | Succeeded
      | Failed of Event.Failure.t
      | Cancelled
      | Denied
      | Interrupted
    [@@deriving equal, sexp_of]
  end

  type t =
    { receipt_version : int
    ; disposition : Disposition.t
    }

  let create ~receipt_version ~disposition =
    if receipt_version <= 0
    then Or_error.error_string "invalid trace receipt version"
    else Ok { receipt_version; disposition }
  ;;

  let disposition t = t.disposition
  let receipt_version t = t.receipt_version
end

module Recorder = struct
  module Mode = struct
    type t =
      | Summary
      | Full
    [@@deriving equal, sexp_of]
  end

  module Limits = struct
    type t =
      { max_events : int
      ; max_normal_bytes : int
      ; max_terminal_bytes : int
      }

    let create ~max_events ~max_normal_bytes ~max_terminal_bytes =
      if
        max_events < 0
        || max_normal_bytes < 0
        || max_terminal_bytes < Canonical_layout.terminal_fixed
      then Or_error.error_string "invalid trace recorder limits"
      else Ok { max_events; max_normal_bytes; max_terminal_bytes }
    ;;

    let max_normal_bytes t = t.max_normal_bytes
    let max_terminal_bytes t = t.max_terminal_bytes
  end

  module Drop_count = struct
    type t =
      { at_least : int64
      ; overflowed : bool
      }

    let at_least t = t.at_least
    let overflowed t = t.overflowed
  end

  module Snapshot = struct
    type state =
      | Incomplete
      | Complete of Terminal.t

    type t =
      { header : Header.t
      ; prefix : Event.t list
      ; last_sequence : int64
      ; omitted_after_prefix : Drop_count.t option
      ; last_retained_site_context : Captured.Location.t option
      ; omitted_site_count : int
      ; state : state
      }

    let header t = t.header
    let prefix t = t.prefix
    let last_sequence t = t.last_sequence
    let omitted_after_prefix t = t.omitted_after_prefix
    let last_retained_site_context t = t.last_retained_site_context
    let omitted_site_count t = t.omitted_site_count
    let state t = t.state

    let with_authorized_sites t ~authorize =
      let omitted = ref t.omitted_site_count in
      let last = ref None in
      let prefix =
        List.map t.prefix ~f:(fun event ->
          match event.Event.site with
          | None -> event
          | Some site when authorize site ->
            last := Some site;
            event
          | Some _ ->
            if !omitted < Int.max_value then incr omitted;
            { event with site = None })
      in
      { t with prefix; last_retained_site_context = !last; omitted_site_count = !omitted }
    ;;
  end

  type t =
    { header : Header.t
    ; mode : Mode.t
    ; limits : Limits.t
    ; owner_domain : unit ref
    ; mutable prefix_rev : Event.t list
    ; mutable retained_count : int
    ; mutable retained_bytes : int
    ; mutable last_retained_site : Captured.Location.t option
    ; mutable saturated : bool
    ; mutable dropped : int64
    ; mutable overflowed : bool
    ; mutable terminal : Terminal.t option
    }

  let domain_token : unit ref Domain.DLS.key = Domain.DLS.new_key (fun () -> ref ())
  let owns_domain t = phys_equal t.owner_domain (Domain.DLS.get domain_token)
  let require_owner t = if not (owns_domain t) then invalid_arg "foreign trace domain"

  let create header ~mode ~limits =
    { header
    ; mode
    ; limits
    ; owner_domain = Domain.DLS.get domain_token
    ; prefix_rev = []
    ; retained_count = 0
    ; retained_bytes = 0
    ; last_retained_site = None
    ; saturated = false
    ; dropped = 0L
    ; overflowed = false
    ; terminal = None
    }
  ;;

  let drop t =
    t.saturated <- true;
    if Int64.equal t.dropped Int64.max_value
    then t.overflowed <- true
    else t.dropped <- Int64.succ t.dropped
  ;;

  let enabled_for_kind t = function
    | Event.Kind.Statement_entered | Call_entered -> Mode.equal t.mode Full
    | Phase_started _
    | Phase_finished _
    | Effect_attempted _
    | Effect_host_returned _
    | Effect_host_failed _
    | Host_wait_entered
    | Host_wait_resumed
    | Job_admitted
    | Job_terminal
    | Recovered_failure _
    | Unrecovered_failure _ -> true
  ;;

  (* The documented canonical binary layout has six 64-bit event words. An
     optional site has two length words, exact namespace/path bytes, 64 hex
     digest bytes and six coordinate words. An optional correlation has one
     length word plus its bounded token bytes. No escaping is needed. *)
  let event_cost ~site_id ~correlation =
    let base = Canonical_layout.event_fixed in
    let correlation_bytes =
      Option.value_map correlation ~default:0 ~f:(fun token ->
        Canonical_layout.correlation_fixed + Event.Correlation.length token)
    in
    let site_bytes =
      Option.value_map site_id ~default:0 ~f:(fun id ->
        Canonical_layout.site_fixed
        + String.length (Captured.Id.package_namespace id)
        + String.length (Captured.Id.relative_path id))
    in
    base + correlation_bytes + site_bytes
  ;;

  let can_admit t ~site_id ~correlation =
    if t.saturated || t.retained_count >= t.limits.max_events
    then false
    else (
      let remaining = t.limits.max_normal_bytes - t.retained_bytes in
      (* Compare components by subtraction before their sum can overflow. *)
      let base =
        Canonical_layout.event_fixed
        + Option.value_map correlation ~default:0 ~f:(fun token ->
          Canonical_layout.correlation_fixed + Event.Correlation.length token)
      in
      if base > remaining
      then false
      else
        Option.value_map site_id ~default:true ~f:(fun id ->
          let namespace = String.length (Captured.Id.package_namespace id) in
          let path = String.length (Captured.Id.relative_path id) in
          let available = remaining - base in
          namespace <= available
          && path <= available - namespace
          && Canonical_layout.site_fixed <= available - namespace - path))
  ;;

  let admit t ~kind ~site ~correlation =
    let site_id = Option.map site ~f:Captured.Location.source in
    if not (can_admit t ~site_id ~correlation)
    then drop t
    else (
      let sequence = Int64.of_int (t.retained_count + 1) in
      let event = { Event.sequence; kind; site; correlation } in
      t.prefix_rev <- event :: t.prefix_rev;
      t.retained_count <- t.retained_count + 1;
      t.retained_bytes <- t.retained_bytes + event_cost ~site_id ~correlation;
      Option.iter site ~f:(fun site -> t.last_retained_site <- Some site))
  ;;

  let finish t terminal =
    require_owner t;
    match t.terminal with
    | Some _ -> Or_error.error_string "trace terminal already published"
    | None ->
      t.terminal <- Some terminal;
      Ok ()
  ;;

  let snapshot t =
    require_owner t;
    let omitted_after_prefix =
      if t.saturated
      then Some Drop_count.{ at_least = t.dropped; overflowed = t.overflowed }
      else None
    in
    { Snapshot.header = t.header
    ; prefix = List.rev t.prefix_rev
    ; last_sequence = Int64.of_int t.retained_count
    ; omitted_after_prefix
    ; last_retained_site_context = t.last_retained_site
    ; omitted_site_count = 0
    ; state =
        (match t.terminal with
         | None -> Incomplete
         | Some terminal -> Complete terminal)
    }
  ;;
end

module Scope = struct
  type binding =
    { recorder : Recorder.t
    ; active : bool ref
    }

  let key : binding Eio.Fiber.key = Eio.Fiber.create_key ()
  let active_scopes : int ref Domain.DLS.key = Domain.DLS.new_key (fun () -> ref 0)

  let within recorder f =
    match recorder with
    | None ->
      if Int.equal !(Domain.DLS.get active_scopes) 0
      then f ()
      else Eio.Fiber.without_binding key f
    | Some recorder ->
      Recorder.require_owner recorder;
      let counter = Domain.DLS.get active_scopes in
      let active = ref true in
      counter := !counter + 1;
      Exn.protect
        ~finally:(fun () ->
          active := false;
          counter := !counter - 1)
        ~f:(fun () -> Eio.Fiber.with_binding key { recorder; active } f)
  ;;

  let without_current f =
    if Int.equal !(Domain.DLS.get active_scopes) 0
    then f ()
    else Eio.Fiber.without_binding key f
  ;;

  let current () =
    if Int.equal !(Domain.DLS.get active_scopes) 0
    then None
    else (
      match Eio.Fiber.get key with
      | Some { recorder; active } when !active && Option.is_none recorder.terminal ->
        Some recorder
      | Some _ | None -> None)
  ;;

  let observe_unlocated ~kind ~correlation =
    Option.iter (current ()) ~f:(fun recorder ->
      if Recorder.enabled_for_kind recorder kind
      then Recorder.admit recorder ~kind ~site:None ~correlation)
  ;;

  let observe_site ~kind ~site ~correlation =
    Option.iter (current ()) ~f:(fun recorder ->
      if Recorder.enabled_for_kind recorder kind
      then
        if recorder.saturated || recorder.retained_count >= recorder.limits.max_events
        then Recorder.drop recorder
        else (
          let location = Option.map site ~f:Chatml_diagnostic.Site.location in
          Recorder.admit recorder ~kind ~site:location ~correlation))
  ;;

  let observe_span ~kind ~source ~span ~correlation =
    Option.iter (current ()) ~f:(fun recorder ->
      if Recorder.enabled_for_kind recorder kind
      then
        if recorder.saturated || recorder.retained_count >= recorder.limits.max_events
        then Recorder.drop recorder
        else (
          let site_id = Option.map source ~f:Captured.id in
          if not (Recorder.can_admit recorder ~site_id ~correlation)
          then Recorder.drop recorder
          else (
            match source with
            | None -> Recorder.admit recorder ~kind ~site:None ~correlation
            | Some source ->
              (match Captured.of_span source span ~checkpoint:(fun () -> ()) with
               | Ok site -> Recorder.admit recorder ~kind ~site:(Some site) ~correlation
               | Error _ -> Recorder.drop recorder))))
  ;;

  let with_host_wait ~site f =
    observe_site ~kind:Event.Kind.Host_wait_entered ~site ~correlation:None;
    let result = f () in
    observe_site ~kind:Event.Kind.Host_wait_resumed ~site ~correlation:None;
    result
  ;;
end
