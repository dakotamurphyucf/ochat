open! Core
module P = Agent_protocol
module X = Persistence_codec
module Window = Recovery_window_record
module Ledger = Recovery_window_ledger
module Member = Ledger.Member
module Record = Recovery_effective_record
module Projection = Recovery_effective_projection
module Version = P.Recovery_item_version
module Citation = P.Recovery_window.Citation
module Scope = P.Recovery_window.Scope
module Source = Recovery_source_reader
module Work = Recovery_source_work
module Digest = Chatml_source.Digest

type family = P.Recovery_history_request.family =
  | Canonical
  | Effective

type scope =
  | Current_generation
  | Window of Scope.t

module Highwater = struct
  type t =
    { windows : int
    ; last_window_members : int
    ; last_preparation_revision : int64 option
    }
end

type candidate =
  { selection : Recovery_item_text.selection
  ; ordinal : int
  ; attempt : Recovery_effective_attempt.t option
  ; is_selected : bool
  }

module Exact_version = struct
  module T = struct
    type t = P.History.Id.t * P.Recovery_item_version.t [@@deriving compare, sexp_of]
  end

  include T
  include Comparator.Make (T)

  let of_selection = function
    | Recovery_item_text.Canonical citation | Prepared { citation; _ } ->
      Citation.occurrence citation, Citation.version citation
  ;;
end

type t =
  { highwater : Highwater.t
  ; digest : Digest.t
  ; candidates : candidate array
  ; earliest : int Map.M(Exact_version).t
  ; has_unretained_sources : bool
  }

type error =
  | Source of Source.error
  | Refresh_required

let highwater t = t.highwater
let digest t = t.digest
let length t = Array.length t.candidates
let has_unretained_sources t = t.has_unretained_sources
let at t position = t.candidates.(position)

let is_earliest t candidate ~position =
  Map.find t.earliest (Exact_version.of_selection candidate.selection)
  |> Option.exists ~f:(Int.equal position)
;;

module Fingerprint = struct
  type t =
    { work : Work.t
    ; mutable digest : Digestif.SHA256.ctx
    }

  let create work = { work; digest = Digestif.SHA256.empty }

  (* Each value is an owner-admitted coordinate or a small fixed record of
     coordinates. No history body, provider payload or checkpoint is rendered. *)
  let limits = P.Recovery_wire.limits ~max_bytes:32_768 ~max_fields:64 ~max_nodes:128

  let add t json =
    let encoded_bytes =
      match Work.measure_json t.work ~limits json with
      | Ok bytes -> bytes
      | Error _ -> raise Work.Exhausted
    in
    Work.charge_source t.work (encoded_bytes + 1);
    let encoded = Jsonaf.to_string json in
    t.digest <- Digestif.SHA256.feed_string t.digest encoded;
    t.digest <- Digestif.SHA256.feed_string t.digest "\n"
  ;;

  let finish t =
    Digestif.SHA256.get t.digest
    |> Digestif.SHA256.to_hex
    |> Digest.of_hex
    |> Or_error.ok_exn
  ;;
end

type builder =
  { fingerprint : Fingerprint.t
  ; ordinals : (int * int) option
  ; mutable count : int
  ; mutable reversed : candidate list
  ; mutable earliest : int Map.M(Exact_version).t
  ; mutable has_unretained_sources : bool
  }

type disposition =
  | Excluded
  | Eligible
  | Unretained

let add t ~evidence ~disposition ~selection ~ordinal ~attempt =
  let disposition =
    if Option.for_all t.ordinals ~f:(fun (lo, hi) -> lo <= ordinal && ordinal < hi)
    then disposition
    else Excluded
  in
  (match disposition with
   | Excluded -> Fingerprint.add t.fingerprint `Null
   | Unretained ->
     Fingerprint.add t.fingerprint (`String "unretained");
     t.has_unretained_sources <- true
   | Eligible ->
     Fingerprint.add t.fingerprint evidence;
     let key = Exact_version.of_selection selection in
     if not (Map.mem t.earliest key)
     then t.earliest <- Map.set t.earliest ~key ~data:t.count);
  let is_selected =
    match disposition with
    | Eligible -> true
    | Excluded | Unretained -> false
  in
  t.count <- t.count + 1;
  t.reversed <- { selection; ordinal; attempt; is_selected } :: t.reversed
;;

let window_state = function
  | Window.Open -> `String "open"
  | Sealed -> `String "sealed"
  | Expired -> `String "expired"
;;

let canonical t ledger visibility window ~count =
  for ordinal = 0 to count - 1 do
    Work.charge_source t.fingerprint.work 1;
    let member = Window.at window ~ordinal |> Option.value_exn in
    let citation = Member.citation member in
    let visible =
      (not (Ledger.is_derived_summary ledger citation))
      &&
      match Recovery_visibility.select visibility citation with
      | Eligible _ -> true
      | Unavailable | Withdrawn | Expired -> false
    in
    add
      t
      ~evidence:(Member.to_jsonaf member)
      ~disposition:(if visible then Eligible else Excluded)
      ~selection:(Canonical citation)
      ~ordinal
      ~attempt:None
  done
;;

let archive_kind = function
  | Ledger.Source.Summary -> `String "summary"
  | Rollover -> `String "rollover"
;;

let custody = function
  | Record.Live -> `String "live"
  | Lost -> `String "lost"
  | Archived location ->
    let module A = Record.Archive in
    `Array
      [ archive_kind (A.kind location)
      ; P.Id.Operation.to_json (A.operation location)
      ; X.int64_json (A.revision location)
      ; `String (Digest.to_hex (A.digest location))
      ]
;;

let physical_position work record ~request_ordinal =
  let module A = Record.Archive in
  let module Position = A.Position in
  match Record.custody record with
  | Live | Lost -> None
  | Archived archive ->
    A.find ~work archive ~request_ordinal
    |> Option.map ~f:(fun position ->
      `Array
        [ X.integer_json (Position.effective_ordinal position)
        ; X.option_json X.integer_json (Position.canonical_ordinal position)
        ])
;;

let attempt_evidence attempt =
  let module A = Recovery_effective_attempt in
  let module E = P.Recovery_attempt_evidence in
  let phase : E.phase =
    match A.phase attempt with
    | Allocated -> Allocated
    | Running -> Running
    | Settled { origin; delivery } ->
      let delivery : E.delivery =
        match delivery with
        | Definitely_not_submitted -> Definitely_not_submitted
        | Possibly_submitted -> Possibly_submitted
        | Response_started -> Response_started
      in
      (match origin with Returned -> Returned delivery | Interrupted -> Interrupted delivery)
  in
  E.to_json { ordinal = A.ordinal attempt; tracked = A.tracked attempt; phase }
;;

let effective_incarnation (state : Session_state.t) =
  Option.bind state.recovery_effective ~f:Recovery_effective_ledger.current_incarnation
;;

let live_position (state : Session_state.t) ledger projection record member =
  let citation = Record.Member.citation member in
  Scope.equal (Record.scope record) (Window.scope (Ledger.active ledger))
  && Option.exists
       (effective_incarnation state)
       ~f:(Int64.equal (Record.overlay_incarnation record))
  && Int64.(state.counters.revision > Record.source_revision record)
  && Option.exists projection ~f:(fun projection ->
    Projection.at projection ~ordinal:(Record.Member.effective_ordinal member)
    |> Option.exists ~f:(fun row ->
      P.History.Id.equal (Projection.Row.occurrence row) (Citation.occurrence citation)
      && Option.equal
           Int.equal
           (Projection.Row.canonical_ordinal row)
           (Record.Member.canonical_ordinal member)
      &&
      match Citation.version citation, Projection.Row.origin row with
      | Effective { origin = Captured { session_revision }; _ }, Captured _ ->
        Int64.(state.counters.revision = succ session_revision)
      | Effective { origin; _ }, current -> Version.equal_origin origin current
      | Canonical _, _ -> false))
;;

let effective t state ledger visibility projection record =
  let source_revision = Record.source_revision record in
  let attempt = Record.attempt record in
  Fingerprint.add
    t.fingerprint
    (`Array
        [ X.int64_json source_revision
        ; X.int64_json (Record.overlay_incarnation record)
        ; custody (Record.custody record)
        ; X.option_json attempt_evidence attempt
        ]);
  List.iter (Record.members record) ~f:(fun member ->
    Work.charge_source t.fingerprint.work 1;
    let citation = Record.Member.citation member in
    let ordinal = Record.Member.request_ordinal member in
    let physical = physical_position t.fingerprint.work record ~request_ordinal:ordinal in
    let disposition =
      if Ledger.is_derived_summary ledger citation
      then Excluded
      else (
        match
          Recovery_visibility.check_effective ~work:t.fingerprint.work visibility citation
        with
        | Error (`Withdrawn | `Expired) -> Excluded
        | Error `Unavailable -> Unretained
        | Ok () ->
          (match Record.custody record with
           | Live when live_position state ledger projection record member -> Eligible
           | Archived _ when Option.is_some physical -> Eligible
           | Live | Lost | Archived _ -> Unretained))
    in
    let evidence =
      `Array
        [ Citation.to_json citation
        ; X.integer_json ordinal
        ; X.integer_json (Record.Member.effective_ordinal member)
        ; X.option_json X.integer_json (Record.Member.canonical_ordinal member)
        ; Option.value physical ~default:`Null
        ]
    in
    add
      t
      ~evidence
      ~disposition
      ~selection:(Prepared { citation; source_revision })
      ~ordinal
      ~attempt)
;;

let select_windows work ledger ~generation ~scope =
  List.filter (Ledger.windows ledger) ~f:(fun window ->
    Work.charge_source work 1;
    let candidate = Window.scope window in
    Int.equal generation (Scope.generation candidate)
    &&
    match scope with
    | Current_generation -> true
    | Window expected -> Scope.equal candidate expected)
;;

let select_preparations work state windows =
  let records =
    Option.value_map
      state.Session_state.recovery_effective
      ~default:[]
      ~f:Recovery_effective_ledger.records
  in
  List.filter records ~f:(fun record ->
    List.exists windows ~f:(fun window ->
      Work.charge_source work 1;
      Scope.equal (Record.scope record) (Window.scope window)))
;;

let capture_highwater windows preparations ~family =
  let last_window_members =
    match family with
    | Effective -> 0
    | Canonical -> Option.value_map (List.last windows) ~default:0 ~f:Window.length
  in
  { Highwater.windows = List.length windows
  ; last_window_members
  ; last_preparation_revision =
      Option.map (List.last preparations) ~f:Record.source_revision
  }
;;

let pin_windows windows (highwater : Highwater.t) ~family =
  let windows = List.take windows highwater.windows in
  if not (Int.equal (List.length windows) highwater.windows)
  then Error Refresh_required
  else (
    match family, List.last windows with
    | Canonical, Some last when Window.length last >= highwater.last_window_members ->
      Ok windows
    | Effective, Some _ when Int.equal highwater.last_window_members 0 -> Ok windows
    | _ -> Error Refresh_required)
;;

let pin_preparations candidates (highwater : Highwater.t) =
  match highwater.last_preparation_revision with
  | None -> Ok []
  | Some high ->
    let selected =
      List.take_while candidates ~f:(fun record ->
        Int64.(Record.source_revision record <= high))
    in
    (match List.last selected with
     | Some last when Int64.equal (Record.source_revision last) high -> Ok selected
     | _ -> Error Refresh_required)
;;

let pin_overlay work fingerprint (state : Session_state.t) preparations =
  let has_live =
    List.exists preparations ~f:(fun record ->
      match Record.custody record with
      | Live -> true
      | Lost | Archived _ -> false)
  in
  match effective_incarnation state with
  | Some overlay_incarnation when has_live ->
    let open Result.Let_syntax in
    let%bind moderator =
      Moderator_checkpoint.decode_for_recovery work state.moderator
      |> Result.map_error ~f:(fun _ -> Source Source.Corrupt)
    in
    Fingerprint.add
      fingerprint
      (Option.value_map moderator ~default:`Null ~f:(fun snapshot ->
         `Array
           [ X.integer_json snapshot.revision; X.integer_json snapshot.next_change_id ]));
    Projection.create_for_read
      work
      ~source_revision:state.counters.revision
      ~overlay_incarnation
      ~history:state.conversation.canonical_history
      ~moderator
    |> Result.map_error ~f:(fun _ -> Source Source.Corrupt)
  | Some _ | None -> Ok None
;;

let create work (state : Session_state.t) ~owner ~scope ~family ~ordinals ~highwater =
  let open Result.Let_syntax in
  let absent =
    if Option.is_some highwater then Refresh_required else Source Source.Unavailable
  in
  let%bind ledger = Result.of_option state.recovery_windows ~error:absent in
  let windows = select_windows work ledger ~generation:state.identity.generation ~scope in
  if List.is_empty windows
  then Error absent
  else (
    let preparations =
      match family with
      | Canonical -> []
      | Effective -> select_preparations work state windows
    in
    let highwater =
      Option.value highwater ~default:(capture_highwater windows preparations ~family)
    in
    let%bind windows = pin_windows windows highwater ~family in
    let%bind preparations = pin_preparations preparations highwater in
    let%bind visibility =
      Recovery_visibility.create ~work state ~owner
      |> Result.map_error ~f:(fun _ -> Source Source.Corrupt)
    in
    let fingerprint = Fingerprint.create work in
    let builder =
      { fingerprint
      ; ordinals
      ; count = 0
      ; reversed = []
      ; earliest = Map.empty (module Exact_version)
      ; has_unretained_sources = false
      }
    in
    let%map projection = pin_overlay work fingerprint state preparations in
    List.iteri windows ~f:(fun index window ->
      Fingerprint.add
        fingerprint
        (`Array
            [ Scope.to_json (Window.scope window); window_state (Window.state window) ]);
      match family with
      | Canonical ->
        let count =
          if Int.equal (index + 1) highwater.windows
          then highwater.last_window_members
          else Window.length window
        in
        canonical builder ledger visibility window ~count
      | Effective ->
        List.iter preparations ~f:(fun record ->
          Work.charge_source work 1;
          if Scope.equal (Record.scope record) (Window.scope window)
          then effective builder state ledger visibility projection record));
    { highwater
    ; digest = Fingerprint.finish fingerprint
    ; candidates = Array.of_list_rev builder.reversed
    ; earliest = builder.earliest
    ; has_unretained_sources = builder.has_unretained_sources
    })
;;
