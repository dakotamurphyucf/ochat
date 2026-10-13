open Core
module Captured = Chatml_source
module Prepared = Chatml_prepared_program
module Wire = Agent_protocol.Authoring_assistance
module Error = Fallback_model.Error
module Work = Fallback_model.Work
module Origin = Fallback_model.Origin

type t =
  { compiled : Captured.t
  ; authored : Captured.t
  ; compiled_cursor : int
  ; authored_cursor : int
  ; map : Fallback_model.Site_map.t option
  ; request_source : Wire.Source.t
  }

let equal_id work left right =
  Work.charge_string work (Captured.Id.package_namespace left);
  Work.charge_string work (Captured.Id.relative_path left);
  Work.charge_string work (Captured.Id.package_namespace right);
  Work.charge_string work (Captured.Id.relative_path right);
  Captured.Id.equal left right
;;

let equal_source work left right =
  Work.charge work 1;
  equal_id work (Captured.id left) (Captured.id right)
  && Captured.Digest.equal (Captured.digest left) (Captured.digest right)
  && Int.equal (Captured.byte_length left) (Captured.byte_length right)
;;

let matches_request work captured request_source =
  Work.charge work 1;
  equal_id work (Captured.id captured) (Wire.Source.id request_source)
  && Captured.Digest.equal (Captured.digest captured) (Wire.Source.digest request_source)
  && Int.equal (Captured.byte_length captured) (Wire.Source.byte_length request_source)
;;

let valid_location work source location ~empty =
  let span = Captured.Location.span location in
  let start_byte = span.left.offset in
  let end_byte = span.right.offset in
  Work.charge work 1;
  equal_id work (Captured.id source) (Captured.Location.source location)
  && Captured.Digest.equal
       (Captured.digest source)
       (Captured.Location.source_digest location)
  && start_byte >= 0
  && end_byte >= start_byte
  && end_byte <= Captured.byte_length source
  && ((not empty) || Int.equal start_byte end_byte)
;;

let create origin ~request ~work =
  let request_source = Wire.Assistance.Request.source request in
  let authored_cursor = Wire.Assistance.Request.cursor_byte request in
  match origin with
  | Origin.Chatml source ->
    if not (matches_request work source request_source)
    then Error Error.Stale_request_source
    else
      Ok
        (Some
           { compiled = source
           ; authored = source
           ; compiled_cursor = authored_cursor
           ; authored_cursor
           ; map = None
           ; request_source
           })
  | Origin.Mapped { compiled; authored; map } ->
    if not (matches_request work authored request_source)
    then Error Error.Stale_request_source
    else
      let open Result.Let_syntax in
      let%bind point =
        map.compiled_point authored_cursor ~checkpoint:(fun () -> Work.charge work 1)
      in
      (match point with
       | None -> Ok None
       | Some point ->
         if not (valid_location work compiled point ~empty:true)
         then Error Error.Invalid_projection
         else (
           let span = Captured.Location.span point in
           Ok
             (Some
                { compiled
                ; authored
                ; compiled_cursor = span.left.offset
                ; authored_cursor
                ; map = Some map
                ; request_source
                })))
;;

let selected_source_matches t selected ~work =
  let prepared = Fallback_selected.Selected_contract.prepared selected in
  let entry = Prepared.entry prepared in
  let found = ref false in
  List.iter (Prepared.sources prepared) ~f:(fun source ->
    Work.charge work 1;
    if equal_source work source t.compiled then found := true);
  !found
  &&
  match t.map with
  | None -> true
  | Some _ -> equal_id work entry (Captured.id t.compiled)
;;

let replacement t ~start_byte ~end_byte ~work =
  let open Result.Let_syntax in
  let%bind authored_range =
    match t.map with
    | None -> Ok (Some (start_byte, end_byte))
    | Some map ->
      (match
         Captured.locate t.compiled ~start_byte ~end_byte ~checkpoint:(fun () ->
           Work.charge work 1)
       with
       | Error _ -> Error Error.Invalid_projection
       | Ok compiled_range ->
         let%bind projected =
           map.authored_replacement compiled_range ~checkpoint:(fun () ->
             Work.charge work 1)
         in
         (match projected with
          | None -> Ok None
          | Some authored_range ->
            if not (valid_location work t.authored authored_range ~empty:false)
            then Error Error.Invalid_projection
            else (
              let span = Captured.Location.span authored_range in
              Ok (Some (span.left.offset, span.right.offset)))))
  in
  match authored_range with
  | None -> Ok None
  | Some (start_byte, end_byte) ->
    if start_byte > t.authored_cursor || t.authored_cursor > end_byte
    then Error Error.Invalid_projection
    else
      Wire.Assistance.Range.create ~source:t.request_source ~start_byte ~end_byte
      |> Result.map ~f:Option.some
      |> Result.map_error ~f:(fun error -> Error.Wire error)
;;
