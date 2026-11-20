open! Core
module P = Agent_protocol
module X = Persistence_codec
module D = Document_schema
module J = P.Json_codec
module Manifest = P.Recovery_window
module Scope = Manifest.Scope
module Citation = Manifest.Citation
module Window_id = P.Id.Context_window
module Operation = P.Id.Operation

let invalid message = Error (P.Error.invalid_request message)
let max_windows = 64
let max_members = Recovery_window_record.max_members

let limits =
  D.Limits.create
    ~max_bytes:(2 * 1024 * 1024)
    ~max_depth:24
    ~max_fields:(24 * max_members)
    ~max_nodes:(28 * max_members)
  |> Result.map_error ~f:(fun error -> Error.create_s [%sexp (error : D.Error.t)])
  |> Or_error.ok_exn
;;

let validate_encoded = D.Json.validate ~limits

let document_error error =
  P.Error.invalid_request (Sexp.to_string_hum ([%sexp_of: D.Error.t] error))
;;

let bounded_list json ~name ~max =
  match json with
  | `Array values when Stdlib.List.compare_length_with values max <= 0 -> Ok values
  | `Array _ -> invalid (name ^ " exceeds the record limit")
  | _ -> invalid (name ^ " must be an array")
;;

module Source = Recovery_window_location.Source
module Member = Recovery_window_location.Member
module Window = Recovery_window_record

module Summary_key = struct
  module T = struct
    type t = int * P.History.Id.t * P.History.Content_revision.t option
    [@@deriving compare, sexp_of]
  end

  include T
  include Comparator.Make (T)
end

let remember_summary versions member =
  let citation = Member.citation member in
  match Member.provenance member, Citation.version citation with
  | Derived_summary _, Canonical revision ->
    let generation = Scope.generation (Citation.scope citation) in
    let id = Citation.occurrence citation in
    Set.add (Set.add versions (generation, id, Some revision)) (generation, id, None)
  | _ -> versions
;;

type t =
  { owner : P.Session_ref.t
  ; windows : Window.t list
  ; active : Window.t
  ; has_withdrawals : bool
  ; summary_versions : Set.M(Summary_key).t
  }

let owner t = t.owner
let windows t = t.windows
let active t = t.active
let has_withdrawals t = t.has_withdrawals

let is_derived_summary t citation =
  let has revision =
    P.Session_ref.equal t.owner (Scope.session (Citation.scope citation))
    && Set.mem
         t.summary_versions
         ( Scope.generation (Citation.scope citation)
         , Citation.occurrence citation
         , revision )
  in
  match Citation.version citation with
  | Canonical revision
  | Effective { origin = Unchanged revision | Replaced { base = revision; _ }; _ } ->
    has (Some revision)
  | Effective { origin = Captured _; _ } -> has None
  | Effective { origin = Inserted _; _ } -> false
;;

(* Older readers must reject any ledger that contains a visibility fence. *)
let version t = if has_withdrawals t then 2 else 1

let to_jsonaf t =
  `Object
    [ "version", `Number (Int.to_string (version t))
    ; "owner", P.Session_ref.to_json t.owner
    ; "windows", `Array (List.map t.windows ~f:Window.to_jsonaf)
    ]
;;

let validate_origin previous (window : Window.t) operations =
  match previous, Window.origin window with
  | None, Root -> Ok ()
  | Some previous, Successor { previous = id; operation }
    when Window_id.equal (Window.id previous) id
         && Int.equal
              (Scope.generation (Window.scope previous))
              (Scope.generation (Window.scope window))
         && not (Hash_set.mem operations operation) ->
    Hash_set.add operations operation;
    Ok ()
  | Some previous, New_generation { previous = id }
    when Window_id.equal (Window.id previous) id
         && Scope.generation (Window.scope previous)
            < Scope.generation (Window.scope window) -> Ok ()
  | _ -> invalid "broken recovery window lineage"
;;

let validate_carry previous member =
  match Member.provenance member with
  | Observed | Derived_summary _ -> Ok ()
  | Carried_from window ->
    (match previous with
     | Some previous
       when Window_id.equal (Window.id previous) window
            && Int.equal
                 (Scope.generation (Window.scope previous))
                 (Scope.generation (Citation.scope (Member.citation member))) ->
       let open Result.Let_syntax in
       let citation = Member.citation member in
       let%bind previous_citation =
         Citation.create
           (Window.scope previous)
           ~occurrence:(Citation.occurrence citation)
           ~version:(Citation.version citation)
       in
       if Option.is_some (Manifest.ordinal (Window.manifest previous) previous_citation)
       then Ok ()
       else invalid "carried recovery member has no exact predecessor"
     | _ -> invalid "carried recovery member names a foreign predecessor")
;;

module Occurrence_version = struct
  module T = struct
    type t = int * P.History.Id.t * P.Recovery_item_version.t
    [@@deriving compare, sexp_of]
  end

  include T
  include Comparator.Make (T)
end

let check_withdrawals windows =
  let%map.Result _ =
    List.fold_result
      windows
      ~init:(Map.empty (module Occurrence_version))
      ~f:(fun statuses window ->
        let generation = Scope.generation (Window.scope window) in
        Window.fold_members window ~init:statuses ~f:(fun statuses member ->
          let citation = Member.citation member in
          let key = generation, Citation.occurrence citation, Citation.version citation in
          let withdrawn = Member.is_withdrawn member in
          match Map.find statuses key with
          | Some previous when Bool.equal previous withdrawn -> Ok statuses
          | Some _ -> invalid "conflicting recovery withdrawal across windows"
          | None -> Ok (Map.set statuses ~key ~data:withdrawn)))
  in
  ()
;;

let create ~owner windows =
  let open Result.Let_syntax in
  if Stdlib.List.compare_length_with windows max_windows > 0
  then invalid "recovery ledger exceeds the window limit"
  else (
    let%bind active =
      match List.last windows with
      | Some window when Window.equal_state (Window.state window) Open -> Ok window
      | Some _ | None -> invalid "recovery ledger requires a final open window"
    in
    let ids = Hash_set.create (module Window_id) in
    let operations = Hash_set.create (module Operation) in
    let%bind _, _, summary_versions =
      List.fold_result
        windows
        ~init:(None, 0, Set.empty (module Summary_key))
        ~f:(fun (previous, total, versions) window ->
          let scope = Window.scope window in
          let id = Window.id window in
          let total = total + Window.length window in
          if
            (not (P.Session_ref.equal owner (Scope.session scope)))
            || Hash_set.mem ids id
            || (Window.equal_state (Window.state window) Open
                && not (Window_id.equal id (Window.id active)))
          then invalid "foreign, duplicate or nonterminal open recovery window"
          else if total > max_members
          then invalid "recovery ledger exceeds the aggregate member limit"
          else (
            Hash_set.add ids id;
            let%bind () = validate_origin previous window operations in
            let%map versions =
              Window.fold_members window ~init:versions ~f:(fun versions member ->
                let%map () = validate_carry previous member in
                remember_summary versions member)
            in
            Some window, total, versions))
    in
    let has_withdrawals = List.exists windows ~f:Window.has_withdrawals in
    let t = { owner; windows; active; has_withdrawals; summary_versions } in
    let%bind () = if has_withdrawals then check_withdrawals windows else Ok () in
    let%map () = validate_encoded (to_jsonaf t) |> Result.map_error ~f:document_error in
    t)
;;

let expire t scope =
  let open Result.Let_syntax in
  if not (Int.equal (Scope.generation scope) (Scope.generation (Window.scope t.active)))
  then invalid "recovery window generation differs from the active generation"
  else (
    match List.find t.windows ~f:(fun w -> Scope.equal (Window.scope w) scope) with
    | None -> invalid "recovery window is unavailable"
    | Some window when Window.equal_state (Window.state window) Expired -> Ok t
    | Some window ->
      let%bind expired = Window.expire window in
      create
        ~owner:t.owner
        (List.map t.windows ~f:(fun w ->
           if Window_id.equal (Window.id w) (Window.id expired) then expired else w)))
;;

let validate_state
      t
      ~session_id
      ~generation
      ~revision
      ~history
      ~has_archive
      ~has_summary_operation
  =
  let open Result.Let_syntax in
  if
    not
      (P.Id.Session.equal session_id (P.Session_ref.session_id t.owner)
       && Int.equal generation (Scope.generation (Window.scope t.active)))
  then invalid "recovery ledger owner or generation differs from the committed state"
  else (
    let live = Array.of_list history in
    let archives = Hashtbl.create (module Operation) in
    let summaries = Hash_set.create (module Operation) in
    let bind_archive (archive : Source.archive) member =
      if Int64.(archive.basis.revision >= revision)
      then invalid "recovery archive is not a committed prestate"
      else (
        let%bind positions =
          match Hashtbl.find archives archive.operation with
          | Some (admitted, positions) when Source.equal_archive admitted archive ->
            Ok positions
          | Some _ -> invalid "recovery archive operation has conflicting roots"
          | None when has_archive archive ->
            let positions = Int.Table.create () in
            Hashtbl.set archives ~key:archive.operation ~data:(archive, positions);
            Ok positions
          | None -> invalid "recovery archive has no exact retained reference"
        in
        let ordinal = Member.source_ordinal member in
        match Hashtbl.find positions ordinal with
        | Some admitted when Member.same_version admitted member -> Ok ()
        | Some _ -> invalid "recovery archive row identifies conflicting member versions"
        | None ->
          Hashtbl.set positions ~key:ordinal ~data:member;
          Ok ())
    in
    let validate_summary member =
      match Member.provenance member with
      | Observed | Carried_from _ -> Ok ()
      | Derived_summary operation when Hash_set.mem summaries operation -> Ok ()
      | Derived_summary operation when has_summary_operation operation ->
        Hash_set.add summaries operation;
        Ok ()
      | Derived_summary _ -> invalid "derived recovery summary has no committed archive"
    in
    List.fold_result t.windows ~init:() ~f:(fun () window ->
      Window.fold_members window ~init:() ~f:(fun () member ->
        let%bind () = validate_summary member in
        match Member.source member with
        | Live basis ->
          let ordinal = Member.source_ordinal member in
          if
            Int64.(basis.revision <= revision)
            && (Member.is_withdrawn member
                || (ordinal < Array.length live && Member.matches member live.(ordinal)))
          then Ok ()
          else invalid "live recovery source no longer matches its exact occurrence"
        | Archive archive -> bind_archive archive member)))
;;

let of_jsonaf json =
  let open Result.Let_syntax in
  let%bind () = validate_encoded json |> Result.map_error ~f:document_error in
  let%bind fields = X.object_ json in
  let%bind encoded_version = X.required fields "version" (J.bounded_int ~min:1 ~max:2) in
  let%bind owner = X.required fields "owner" P.Session_ref.of_json in
  let%bind windows =
    X.required fields "windows" (bounded_list ~name:"recovery windows" ~max:max_windows)
  in
  let%bind windows = List.map windows ~f:(Window.of_jsonaf owner) |> Result.all in
  let%bind t = create ~owner windows in
  if Int.equal (version t) encoded_version
  then Ok t
  else invalid "recovery ledger version does not match its withdrawal semantics"
;;

let shape =
  X.shape_exn
    [ "version", D.Shape.value
    ; "owner", Source.owner_shape
    ; "windows", X.array_shape_exn ~identity_field:"id" Window.shape
    ]
;;
