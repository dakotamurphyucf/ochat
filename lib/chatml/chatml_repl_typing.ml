open Core
module Captured = Chatml_source
module Prepared = Chatml_prepared_program
module Lookup = Chatml_configured_lookup
module Parse = Chatml.Chatml_parse
module Session = Chatml_dependency_session
module Check = Chatml_configured_check
module Contract = Chatml.Chatml_compiler_contract
module Checker = Chatml_typechecker
module Scope = Checker.Value_scope
module Alias = Checker.Type_alias
module Public = Checker.Configured_resolution.Public_module
module Root_copy = Checker.Repl_type_roots
module Snapshot = Chatml_typechecker.Type_snapshot
module Diagnostic = Chatml_diagnostic

module Limits = struct
  type t =
    { parse : Parse.Submission.Limits.t
    ; display : Snapshot.Limits.t
    ; max_display_name_bytes : int
    ; max_statements : int
    ; root_copy : Root_copy.Limits.t
    }

  let create ~parse ~display ~max_display_name_bytes ~max_statements ~root_copy =
    if max_statements <= 0 || max_display_name_bytes < 0
    then Or_error.error_string "REPL statement/name limits are invalid"
    else Ok { parse; display; max_display_name_bytes; max_statements; root_copy }
  ;;
end

module Error = struct
  type t =
    | Lookup of Lookup.Repl_scope.Error.t
    | Closed
    | Prepared_mismatch
    | Contract_mismatch
    | Foreign_submission
    | Stale_submission
    | Aborted_submission
    | Statement_out_of_order
    | Statement_limit
    | Binding_limit
    | Root_copy of Root_copy.Error.t
    | Root_check of Session.Roots.Error.t
    | Display_limit
    | Type_diagnostic of Diagnostic.t
    | Foreign_candidate
    | Stale_candidate
    | Consumed_candidate
    | Busy_executing
    | Conflicting_finalization
    | Accounting_overflow
  [@@deriving sexp_of]
end

type ready0 =
  { owner : unit ref
  ; source : Captured.t
  ; parsed : Parse.Unit.t
  ; context : Lookup.t
  ; statement_count : int
  ; mutable next_index : int
  ; mutable aborted : bool
  }

type t =
  { witness : unit ref
  ; prepared : Prepared.t
  ; scope : Lookup.Repl_scope.t
  ; contract : Contract.t
  ; root_limits : Session.Roots.Limits.t
  ; limits : Limits.t
  ; mutable lexical : Scope.t
  ; mutable aliases : Alias.t
  ; mutable modules : Public.t Session.Roots.t
  ; mutable generation : int
  ; mutable current : ready0 option
  ; mutable candidate_witness : unit ref option
  ; mutable reservation_open : bool
  ; mutable busy : bool
  ; mutable closed : bool
  }

type typing_owner = t

let prepared t = t.prepared
let contract t = t.contract

module Submission = struct
  type ready = ready0

  type t =
    | Need_more of Parse.Submission.Need_more.t
    | Rejected of Parse.Submission.Rejection.t
    | Ready of ready

  let source t = t.source
  let statement_count t = t.statement_count
end

module Frozen_types = struct
  type t =
    { snapshot : Snapshot.t
    ; bindings : (string * Snapshot.Id.t) list
    }

  let snapshot t = t.snapshot
  let bindings t = t.bindings
end

module Candidate = struct
  module Checked_dependency = struct
    type t =
      { id : Prepared.Module_id.t
      ; implementation : Parse.Unit.t
      ; interface : Parse.Interface.t option
      ; checked : Check.Checked_unit.t
      }

    let id t = t.id
    let implementation t = t.implementation
    let interface t = t.interface
    let checked t = t.checked
  end

  type t =
    { owner : unit ref
    ; witness : unit ref
    ; generation : int
    ; ready : ready0
    ; statement : Chatml.Chatml_lang.stmt_node
    ; checked : Checker.checked_program
    ; frozen_types : Frozen_types.t
    ; frozen_before : Frozen_types.t
    ; before_values : Scope.t
    ; before_types : Alias.t
    ; after_values : Scope.t
    ; after_types : Alias.t
    ; modules : Public.t Session.Roots.t
    ; new_checked_dependencies : Checked_dependency.t list
    ; direct_references : Lookup.Reference.t list
    ; mutable consumed : bool
    }

  let source t = t.ready.source
  let statement t = t.statement
  let checked t = t.checked
  let frozen_types t = t.frozen_types
  let input_fingerprint t = Session.Roots.input_fingerprint t.modules
  let new_checked_dependencies t = t.new_checked_dependencies

  let checked_dependency_order t ~checkpoint =
    List.map (Session.Roots.checked_in_order t.modules) ~f:(fun (id, _) ->
      checkpoint ();
      id)
  ;;

  let dependency_edges t = Session.Roots.edges t.modules
  let direct_references t = t.direct_references
end

module Published = struct
  type outcome =
    | After_success
    | Constrained_before_runtime_failure
  [@@deriving equal, sexp_of]

  type t =
    { outcome : outcome
    ; frozen_types : Frozen_types.t
    }

  let outcome t = t.outcome
  let frozen_types t = t.frozen_types
end

module Reservation = struct
  type owner = t

  type pending =
    { owner : owner
    ; candidate : Candidate.t
    ; after : Published.t
    ; before : Published.t
    }

  type state =
    | Pending of pending
    | Finalized of Published.t

  type t = { mutable state : state }

  let pending_for t ~owner =
    match t.state with
    | Pending pending -> phys_equal pending.owner owner
    | Finalized _ -> false
  ;;
end

let with_activity t ~f =
  if t.busy || t.reservation_open
  then Error Error.Busy_executing
  else if t.closed
  then Error Error.Closed
  else (
    t.busy <- true;
    Exn.protect ~f ~finally:(fun () -> t.busy <- false))
;;

let create ~prepared ~scope ~contract ~root_limits ~limits ~checkpoint =
  checkpoint ();
  if
    not
      (Captured.Digest.equal
         (Lookup.Repl_scope.input_fingerprint scope)
         (Prepared.fingerprint prepared))
  then Error Error.Prepared_mismatch
  else if
    not
      (Captured.Digest.equal
         (Contract.fingerprint contract ~checkpoint)
         (Prepared.compiler_contract prepared))
  then Error Error.Contract_mismatch
  else (
    let surface = Contract.static_surface contract in
    let lexical = Checker.init_env_with_static surface ~checkpoint in
    let aliases = Checker.init_types_with_static surface ~checkpoint in
    checkpoint ();
    Ok
      { witness = ref ()
      ; prepared
      ; scope
      ; contract
      ; root_limits
      ; limits
      ; lexical
      ; aliases
      ; modules = Session.Roots.empty prepared ~limits:root_limits
      ; generation = 0
      ; current = None
      ; candidate_witness = None
      ; reservation_open = false
      ; busy = false
      ; closed = false
      })
;;

let count_statements parsed ~limit ~checkpoint =
  let rec loop count = function
    | [] -> Ok count
    | _ :: rest ->
      checkpoint ();
      if count >= limit then Error Error.Statement_limit else loop (count + 1) rest
  in
  loop 0 (Parse.Unit.program parsed).stmts
;;

let admit t source ~checkpoint =
  with_activity t ~f:(fun () ->
    let open Result.Let_syntax in
    match Parse.Submission.parse source ~limits:t.limits.parse ~checkpoint with
    | Need_more reason -> Ok (Submission.Need_more reason)
    | Rejected reason -> Ok (Submission.Rejected reason)
    | Complete parsed ->
      let%bind statement_count =
        count_statements parsed ~limit:t.limits.max_statements ~checkpoint
      in
      let%map context =
        Lookup.Repl_scope.for_submission t.scope source ~checkpoint
        |> Result.map_error ~f:(fun error -> Error.Lookup error)
      in
      Option.iter t.current ~f:(fun old -> old.aborted <- true);
      t.candidate_witness <- None;
      let ready =
        { owner = t.witness
        ; source
        ; parsed
        ; context
        ; statement_count
        ; next_index = 0
        ; aborted = false
        }
      in
      t.current <- Some ready;
      Submission.Ready ready)
;;

let check_ready t ready ~checkpoint =
  if not (phys_equal ready.owner t.witness)
  then Error Error.Foreign_submission
  else if ready.aborted
  then Error Error.Aborted_submission
  else if not (Option.exists t.current ~f:(phys_equal ready))
  then Error Error.Stale_submission
  else
    Lookup.Repl_scope.validate_context t.scope ready.context ~checkpoint
    |> Result.map_error ~f:(fun error -> Error.Lookup error)
;;

let freeze_values values ~(limits : Limits.t) ~checkpoint =
  let open Result.Let_syntax in
  let%bind roots =
    Scope.Roots.capture
      values
      ~max_bindings:(Root_copy.Limits.max_bindings limits.root_copy)
      ~checkpoint
    |> Result.map_error ~f:(function `Binding_limit -> Error.Binding_limit)
  in
  let names = Scope.Roots.names roots ~checkpoint in
  let name_bytes = ref 0 in
  let%bind () =
    List.fold_result names ~init:() ~f:(fun () name ->
      checkpoint ();
      let bytes = String.length name in
      if bytes > limits.max_display_name_bytes - !name_bytes
      then Error Error.Display_limit
      else (
        name_bytes := !name_bytes + bytes;
        Ok ()))
  in
  let groups =
    List.map (Scope.Roots.schemes roots) ~f:(fun scheme ->
      checkpoint ();
      Snapshot.Input_group.create [ scheme ])
  in
  let%bind snapshot =
    Snapshot.freeze groups ~limits:limits.display ~checkpoint
    |> Result.map_error ~f:(fun _ -> Error.Display_limit)
  in
  let rec pair names roots reversed =
    checkpoint ();
    match names, roots with
    | [], [] ->
      Ok
        (List.fold reversed ~init:[] ~f:(fun bindings binding ->
           checkpoint ();
           binding :: bindings))
    | name :: names, [ root ] :: roots -> pair names roots ((name, root) :: reversed)
    | _ -> Error Error.Display_limit
  in
  let%map bindings = pair names (Snapshot.roots snapshot) [] in
  { Frozen_types.snapshot; bindings }
;;

let reverse_with_checkpoint values ~checkpoint =
  List.fold values ~init:[] ~f:(fun reversed value ->
    checkpoint ();
    value :: reversed)
;;

let prepare_next t ready ~checkpoint =
  with_activity t ~f:(fun () ->
    let abort_if_current () =
      if phys_equal ready.owner t.witness && Option.exists t.current ~f:(phys_equal ready)
      then ready.aborted <- true
    in
    try
      let prepared =
        let open Result.Let_syntax in
        let%bind () = check_ready t ready ~checkpoint in
        if Option.is_some t.candidate_witness
        then Error Error.Busy_executing
        else if ready.next_index >= ready.statement_count
        then Error Error.Statement_out_of_order
        else (
          let generation = t.generation in
          let%bind copied =
            Root_copy.copy_detached
              ~lexical:t.lexical
              ~modules:t.modules
              ~limits:t.limits.root_copy
              ~checkpoint
            |> Result.map_error ~f:(fun error -> Error.Root_copy error)
          in
          let machine =
            Check.create_static_for_repl
              (Contract.static_surface t.contract)
              ~name_seed:(Root_copy.Copy.name_seed copied)
              ~checkpoint
          in
          let modules = ref (Root_copy.Copy.modules copied) in
          let checked_rev = ref [] in
          let direct_rev = ref [] in
          let root_error = ref None in
          let check node ~require =
            let source = Session.Unit.implementation node in
            let parse context =
              Parse.parse_captured (Lookup.source context) ~checkpoint
            in
            let parse_interface context =
              Parse.parse_captured_interface (Lookup.source context) ~checkpoint
            in
            let%bind implementation = parse source in
            let%bind interface =
              match Session.Unit.interface node with
              | None -> Ok None
              | Some context -> Result.map (parse_interface context) ~f:Option.some
            in
            let%map checked =
              Check.check_node machine node ~require ~implementation ~interface
            in
            checked_rev
            := { Candidate.Checked_dependency.id = Session.Unit.id node
               ; implementation
               ; interface
               ; checked
               }
               :: !checked_rev;
            Check.Checked_unit.exports checked
          in
          let require ~source reference =
            if phys_equal source ready.context then direct_rev := reference :: !direct_rev;
            match
              Session.Roots.require_selected
                !modules
                ~scope:t.scope
                ~source
                reference
                ~checkpoint
                ~check
            with
            | Ok (next, public) ->
              modules := next;
              Ok public
            | Error error ->
              if Option.is_none !root_error then root_error := Some error;
              Error
                (Diagnostic.create
                   ~site:(Lookup.Reference.site reference)
                   "Configured REPL dependency could not be checked")
          in
          let result =
            Check.check_repl_statement
              machine
              ~source:ready.context
              ~require
              ~parsed:ready.parsed
              ~statement_index:ready.next_index
              ~before_values:(Root_copy.Copy.lexical copied)
              ~before_types:t.aliases
          in
          match !root_error, result with
          | Some error, _ -> Error (Error.Root_check error)
          | None, Error diagnostic -> Error (Error.Type_diagnostic diagnostic)
          | None, Ok checked ->
            let before_values = Check.Repl_statement.before_values checked in
            let after_values = Check.Repl_statement.after_values checked in
            let%bind frozen_before =
              freeze_values before_values ~limits:t.limits ~checkpoint
            in
            let%bind frozen_types =
              freeze_values after_values ~limits:t.limits ~checkpoint
            in
            let new_checked_dependencies =
              reverse_with_checkpoint !checked_rev ~checkpoint
            in
            let direct_references = reverse_with_checkpoint !direct_rev ~checkpoint in
            let%bind () = check_ready t ready ~checkpoint in
            if generation <> t.generation || Option.is_some t.candidate_witness
            then Error Error.Stale_submission
            else (
              let witness = ref () in
              let candidate =
                { Candidate.owner = t.witness
                ; witness
                ; generation
                ; ready
                ; statement = Check.Repl_statement.statement checked
                ; checked = Check.Repl_statement.checked checked
                ; frozen_types
                ; frozen_before
                ; before_values
                ; before_types = Check.Repl_statement.before_types checked
                ; after_values
                ; after_types = Check.Repl_statement.after_types checked
                ; modules = !modules
                ; new_checked_dependencies
                ; direct_references
                ; consumed = false
                }
              in
              t.candidate_witness <- Some witness;
              Ok candidate))
      in
      (match prepared with
       | Error
           ( Root_copy _
           | Root_check _
           | Display_limit
           | Type_diagnostic _
           | Binding_limit
           | Lookup _
           | Stale_submission )
         when phys_equal ready.owner t.witness
              && Option.exists t.current ~f:(phys_equal ready) -> abort_if_current ()
       | Ok _ | Error _ -> ());
      prepared
    with
    | exn ->
      let backtrace = Stdlib.Printexc.get_raw_backtrace () in
      abort_if_current ();
      Exn.raise_with_original_backtrace exn backtrace)
;;

let check_candidate t candidate =
  if not (phys_equal candidate.Candidate.owner t.witness)
  then Error Error.Foreign_candidate
  else if candidate.consumed
  then Error Error.Consumed_candidate
  else if candidate.generation <> t.generation
  then Error Error.Stale_candidate
  else if not (Option.exists t.candidate_witness ~f:(phys_equal candidate.witness))
  then Error Error.Stale_candidate
  else if
    candidate.ready.aborted
    || not (Option.exists t.current ~f:(phys_equal candidate.ready))
  then Error Error.Stale_candidate
  else Ok ()
;;

let validate_candidate t candidate =
  if t.busy || t.reservation_open
  then Error Error.Busy_executing
  else if t.closed
  then Error Error.Closed
  else check_candidate t candidate
;;

let discard_candidate t candidate =
  if t.busy || t.reservation_open
  then Error Error.Busy_executing
  else if t.closed
  then Error Error.Closed
  else
    let open Result.Let_syntax in
    let%map () = check_candidate t candidate in
    candidate.consumed <- true;
    candidate.ready.aborted <- true;
    t.candidate_witness <- None
;;

let reserve_execution t candidate ~checkpoint =
  with_activity t ~f:(fun () ->
    let open Result.Let_syntax in
    let%bind () = check_candidate t candidate in
    if t.generation = Int.max_value
    then Error Error.Accounting_overflow
    else (
      let%bind () = check_ready t candidate.ready ~checkpoint in
      checkpoint ();
      let%bind () = check_ready t candidate.ready ~checkpoint in
      let%bind () = check_candidate t candidate in
      let after =
        { Published.outcome = After_success; frozen_types = candidate.frozen_types }
      in
      let before =
        { Published.outcome = Constrained_before_runtime_failure
        ; frozen_types = candidate.frozen_before
        }
      in
      candidate.consumed <- true;
      t.reservation_open <- true;
      Ok { Reservation.state = Pending { owner = t; candidate; after; before } }))
;;

let finalize_execution reservation ~outcome =
  match reservation.Reservation.state with
  | Finalized previous when Published.equal_outcome previous.outcome outcome ->
    Ok previous
  | Finalized _ -> Error Error.Conflicting_finalization
  | Pending { owner = t; candidate; after; before } ->
    let published =
      match outcome with
      | Published.After_success -> after
      | Constrained_before_runtime_failure -> before
    in
    t.lexical
    <- (match outcome with
        | Published.After_success -> candidate.after_values
        | Constrained_before_runtime_failure -> candidate.before_values);
    t.aliases
    <- (match outcome with
        | Published.After_success -> candidate.after_types
        | Constrained_before_runtime_failure -> candidate.before_types);
    t.modules <- candidate.modules;
    t.generation <- t.generation + 1;
    (match outcome with
     | Published.After_success ->
       candidate.ready.next_index <- candidate.ready.next_index + 1
     | Constrained_before_runtime_failure -> candidate.ready.aborted <- true);
    t.candidate_witness <- None;
    t.reservation_open <- false;
    reservation.state <- Finalized published;
    Ok published
;;

let close t =
  if t.busy || t.reservation_open
  then Error Error.Busy_executing
  else (
    t.closed <- true;
    Option.iter t.current ~f:(fun ready -> ready.aborted <- true);
    t.candidate_witness <- None;
    Ok ())
;;
