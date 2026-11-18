open! Core
module P = Agent_protocol

module Kind = struct
  type t =
    | Reconcile_foreground
    | Checkpoint
    | Suppress_automatic
    | Stop
    | Callback_failure
    | Start_compaction
    | Start_ordinary_turn
    | Retire_turn
    | Retire_compaction
    | Reconcile_terminal
  [@@deriving equal]
end

module Selected = struct
  type t =
    | Requested of Kind.t
    | Admission_hold of P.Id.Goal.t
end

module Diagnostic = struct
  type t =
    | Returned of P.Error.t
    | Raised of exn * Backtrace.t
end

module Acknowledgement = struct
  type t =
    { previous : Session_state.t
    ; candidate : Session_transition.t
    ; selected : Selected.t
    }

  let previous t = t.previous
  let candidate t = t.candidate
  let selected t = t.selected
end

type phase =
  | Not_attempted
  | Attempted
  | Installed of Acknowledgement.t

type selection =
  { acknowledgement : Acknowledgement.t
  ; installed_phase : phase
  ; mutable installed : bool
  }

type t =
  { owner : unit ref
  ; previous : Session_state.t
  ; requested : Session_transition.t
  ; kind : Kind.t
  ; attempted : bool ref
  ; mutable selected : selection option
  ; mutable diagnostic : Diagnostic.t option
  }

let conflict message = P.Error.create Conflict ~message ~retryable:true ()

let next_revision previous candidate =
  let revision = previous.Session_state.counters.revision in
  (not (Int64.equal revision Int64.max_value))
  && Int64.equal
       candidate.Session_transition.state.counters.revision
       Int64.(revision + 1L)
;;

let capture ~owner ~previous ~requested ~kind ~attempted =
  if !attempted || not (next_revision previous requested)
  then Error (conflict "legacy completion requires an actual unattempted next candidate")
  else
    Ok { owner; previous; requested; kind; attempted; selected = None; diagnostic = None }
;;

let previous t = t.previous
let requested t = t.requested
let kind t = t.kind
let attempted t = t.attempted
let diagnostic t = t.diagnostic

let phase t =
  match t.selected with
  | Some { installed = true; installed_phase; _ } -> installed_phase
  | Some { installed = false; _ } | None ->
    if !(t.attempted) then Attempted else Not_attempted
;;

let check_before_writer t ~owner ~state =
  if phys_equal owner t.owner && phys_equal state t.previous && not !(t.attempted)
  then Ok ()
  else Error (conflict "legacy completion writer lost its exact original basis")
;;

let select_actual t ~owner ~state ~candidate ~selected =
  let open Result.Let_syntax in
  let%bind () = check_before_writer t ~owner ~state in
  let%bind () =
    if Option.is_none t.selected && next_revision state candidate
    then Ok ()
    else Error (conflict "legacy completion writer was already selected")
  in
  let%bind () =
    match selected with
    | Selected.Requested kind
      when Kind.equal kind t.kind && phys_equal candidate t.requested -> Ok ()
    | Admission_hold _ when Kind.equal t.kind Start_ordinary_turn -> Ok ()
    | Requested _ | Admission_hold _ ->
      Error (conflict "legacy completion does not own this actual selected writer")
  in
  let acknowledgement = { Acknowledgement.previous = state; candidate; selected } in
  t.selected
  <- Some
       { acknowledgement; installed_phase = Installed acknowledgement; installed = false };
  Ok ()
;;

let install t ~owner ~candidate ~state =
  match t.selected with
  | Some ({ acknowledgement; _ } as selection)
    when phys_equal owner t.owner
         && !(t.attempted)
         && phys_equal acknowledgement.candidate candidate
         && phys_equal candidate.Session_transition.state state ->
    selection.installed <- true
  | None | Some _ -> invalid_arg "legacy completion actual State ACK mismatch"
;;

let retain_failure t error =
  if Option.is_none t.diagnostic then t.diagnostic <- Some (Diagnostic.Returned error)
;;

let retain_exception t exn backtrace =
  if Option.is_none t.diagnostic
  then t.diagnostic <- Some (Diagnostic.Raised (exn, backtrace))
;;
