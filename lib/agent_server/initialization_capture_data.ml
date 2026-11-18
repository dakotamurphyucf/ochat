open! Core
module A = Agent_session.Session_actor
module C = Agent_session.Callback_cleanup

type end_outcome = C.Operation.terminal_outcome

type completion_origin =
  { completion : C.Completion.t
  ; slot : C.Completion_snapshot.Failure_slot.kind
  ; diagnostic : Original_cleanup_report.Segment.t
  }

type boundary =
  { domain : int
  ; scope : A.Initialization_scope.t
  ; callbacks : C.Initialization.t
  ; mutable original_end : end_outcome option
  ; mutable original_diagnostics : Original_cleanup_report.t option
  ; mutable completion_origins : completion_origin list
  ; mutable end_diagnostic : Original_cleanup_report.Segment.t option
  ; mutable late_diagnostics_rev : Original_cleanup_report.Segment.t list
  }

type t =
  { domain : int
  ; actor : A.t
  ; register : boundary -> unit
  }

let check_domain_exn domain =
  if not (Int.equal domain (Stdlib.Domain.self () :> int))
  then invalid_arg "initializer cleanup observation belongs to another domain"
;;

let create ~actor ~register = { domain = (Stdlib.Domain.self () :> int); actor; register }

let initialization t ~actor ~scope =
  check_domain_exn t.domain;
  if not (phys_equal t.actor actor)
  then invalid_arg "initializer cleanup belongs to another Actor";
  let boundary =
    { domain = t.domain
    ; scope
    ; callbacks = A.Initialization_scope.callback_cleanup scope
    ; original_end = None
    ; original_diagnostics = None
    ; completion_origins = []
    ; end_diagnostic = None
    ; late_diagnostics_rev = []
    }
  in
  t.register boundary;
  boundary
;;

let same_scope (first : boundary) (second : boundary) =
  check_domain_exn first.domain;
  check_domain_exn second.domain;
  phys_equal first.scope second.scope
;;

let remember_completion (t : boundary) completion slots ~late =
  let find slot =
    List.find t.completion_origins ~f:(fun original ->
      phys_equal original.completion completion
      && C.Completion_snapshot.Failure_slot.equal_kind original.slot slot)
  in
  let unknown =
    List.filter slots ~f:(fun slot ->
      Option.is_none (find (C.Completion_snapshot.Failure_slot.kind slot)))
  in
  (match unknown with
   | [] -> ()
   | _ ->
     let diagnostic =
       Original_cleanup_report.Segment.create
         (List.map unknown ~f:C.Completion_snapshot.Failure_slot.failure)
     in
     List.iter unknown ~f:(fun slot ->
       t.completion_origins
       <- { completion; slot = C.Completion_snapshot.Failure_slot.kind slot; diagnostic }
          :: t.completion_origins);
     if late then t.late_diagnostics_rev <- diagnostic :: t.late_diagnostics_rev);
  List.map slots ~f:(fun slot ->
    let kind = C.Completion_snapshot.Failure_slot.kind slot in
    match find kind with
    | Some original -> original.diagnostic
    | None -> invalid_arg "observed initialization slot was not retained")
;;

let observe_completion (t : boundary) observation =
  check_domain_exn t.domain;
  remember_completion
    t
    (C.Completion_observation.completion observation)
    (C.Completion_snapshot.failure_slots (C.Completion_observation.snapshot observation))
    ~late:true
;;

let record_original_end (t : boundary) outcome =
  check_domain_exn t.domain;
  match t.original_end with
  | None ->
    (* Keep the actual End result even if the read-only diagnostic observation
       itself encounters an invariant escape. Its absent view remains incomplete. *)
    t.original_end <- Some outcome;
    let diagnostic = function
      | [] -> None
      | failures -> Some (Original_cleanup_report.Segment.create failures)
    in
    let pieces diagnostic =
      List.map (Option.to_list diagnostic) ~f:(fun diagnostic ->
        Original_cleanup_report.Piece.Own diagnostic)
    in
    let own failures = pieces (diagnostic failures) in
    let ending =
      diagnostic
        (match outcome with
         | Ok (Ok ()) -> []
         | Ok (Error error) -> [ C.Failure.Protocol error ]
         | Error (exn, trace) -> [ Raised (exn, trace) ])
    in
    t.end_diagnostic <- ending;
    let callbacks, sealed =
      match C.Initialization.snapshot t.callbacks with
      | Error error -> own [ C.Failure.Protocol error ], false
      | Ok snapshot ->
        let _, reversed =
          List.fold
            (C.Cleanup_snapshot.claims snapshot)
            ~init:([], [])
            ~f:(fun (seen, reversed) observation ->
              let completion = C.Completion_observation.completion observation in
              if List.mem seen completion ~equal:phys_equal
              then seen, reversed
              else (
                let snapshot = C.Completion_observation.snapshot observation in
                let groups =
                  remember_completion
                    t
                    completion
                    (C.Completion_snapshot.failure_slots snapshot)
                    ~late:false
                in
                let original =
                  List.map groups ~f:(fun diagnostic ->
                    Original_cleanup_report.Piece.Own diagnostic)
                in
                completion :: seen, List.rev_append original reversed))
        in
        List.rev reversed, Callback_disposal.parent_diagnostics_sealed snapshot
    in
    (* Original End is observed only after the protected Init finish call. An
       unexplained await escape cannot make this original diagnostic complete
       merely because a later independent release settles current custody. *)
    t.original_diagnostics
    <- Some
         (Original_cleanup_report.create
            ~diagnostics_complete:sealed
            (callbacks @ pieces ending))
  | Some _ -> invalid_arg "original initialization End is already recorded"
;;

let snapshot (t : boundary) =
  check_domain_exn t.domain;
  C.Initialization.snapshot t.callbacks
;;

let original_end (t : boundary) =
  check_domain_exn t.domain;
  t.original_end
;;

let original_diagnostics (t : boundary) =
  check_domain_exn t.domain;
  let original =
    Option.value_or_thunk t.original_diagnostics ~default:(fun () ->
      Original_cleanup_report.create ~diagnostics_complete:false [])
  in
  match t.late_diagnostics_rev with
  | [] -> original
  | late ->
    Original_cleanup_report.create
      ~diagnostics_complete:(Original_cleanup_report.diagnostics_complete original)
      (Original_cleanup_report.Piece.Child original
       :: List.rev_map late ~f:(fun diagnostic ->
         Original_cleanup_report.Piece.Own diagnostic))
;;

let original_diagnostics_sealed (t : boundary) =
  Original_cleanup_report.diagnostics_complete (original_diagnostics t)
;;

(* Write-once failure slots belong to the SAME Completion, including a slot
   first observed after original End. Re-observation forwards that carrier; it
   neither re-mints the raw projection nor rewrites the frozen original view. *)
let completion_diagnostic (t : boundary) ~completion ~slot ~failure =
  check_domain_exn t.domain;
  match
    List.find t.completion_origins ~f:(fun original ->
      phys_equal original.completion completion
      && C.Completion_snapshot.Failure_slot.equal_kind original.slot slot)
  with
  | Some original -> original.diagnostic
  | None ->
    let diagnostic = Original_cleanup_report.Segment.create [ failure ] in
    t.completion_origins <- { completion; slot; diagnostic } :: t.completion_origins;
    t.late_diagnostics_rev <- diagnostic :: t.late_diagnostics_rev;
    diagnostic
;;

let end_diagnostic (t : boundary) =
  check_domain_exn t.domain;
  t.end_diagnostic
;;
