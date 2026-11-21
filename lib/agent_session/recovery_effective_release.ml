open! Core
module P = Agent_protocol
module Citation = P.Recovery_window.Citation
module Scope = P.Recovery_window.Scope
module Source = Recovery_source_reader
module Effective = Recovery_effective_record
module Capture = Recovery_effective_capture
module Work = Recovery_source_work

let check ~visible ~checkpoint accepted ~source_revision citation =
  let open Result.Let_syntax in
  let owner = Scope.session (Citation.scope citation) in
  let same_member expected fresh =
    let module Member = Effective.Member in
    Int.equal (Member.request_ordinal expected) (Member.request_ordinal fresh)
    && Int.equal (Member.effective_ordinal expected) (Member.effective_ordinal fresh)
    && Option.equal
         Int.equal
         (Member.canonical_ordinal expected)
         (Member.canonical_ordinal fresh)
  in
  let inspect state ~f =
    try
      Work.with_metadata_scope ~checkpoint ~f:(fun work ->
        if not (visible state)
        then Error Source.Unavailable
        else (
          let%bind current =
            Recovery_visibility.create ~work state ~owner
            |> Result.map_error ~f:(fun _ -> Source.Corrupt)
          in
          let%bind () =
            Recovery_visibility.check_effective ~work current citation
            |> Result.map_error ~f:(function
              | `Unavailable -> Source.Unavailable
              | `Withdrawn -> Withdrawn
              | `Expired -> Expired)
          in
          let%bind record =
            Capture.resolve_record ~work state ~source_revision
            |> Result.of_option ~error:Source.Unavailable
          in
          let%bind member =
            Effective.find_member ~work record citation
            |> Result.of_option ~error:Source.Unavailable
          in
          f work (Effective.custody record) member))
    with
    | Work.Exhausted | Work.Inactive -> Error Source.Limit
  in
  let%map custody, selected =
    inspect accepted ~f:(fun _ custody member -> Ok (custody, member))
  in
  fun latest ->
    phys_equal accepted latest
    || Result.is_ok
         (inspect latest ~f:(fun work fresh member ->
            if not (same_member selected member)
            then Error Source.Stale
            else (
              match custody, fresh with
              | Effective.Archived first, Archived latest
                when Effective.Archive.equal first latest -> Ok ()
              | Live, Live ->
                Capture.resolve_live ~work latest ~source_revision citation
                |> Result.map_error ~f:(fun _ -> Source.Corrupt)
                |> Result.bind ~f:(function
                  | Some _ -> Ok ()
                  | None -> Error Source.Stale)
              | _ -> Error Source.Stale)))
;;
