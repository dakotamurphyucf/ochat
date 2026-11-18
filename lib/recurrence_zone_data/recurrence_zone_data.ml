open Core
module Error = Zone_data_error
module Zone = Recurrence_rules.Zone
module Local_type = Tzif_reader.Local_type
module Transition = Tzif_reader.Transition

let maximum_bytes = Tzif_reader.maximum_bytes
let maximum_transitions = Tzif_reader.maximum_transitions
let maximum_types = Tzif_reader.maximum_types
let maximum_designation_bytes = Tzif_reader.maximum_designation_bytes
let maximum_footer_bytes = Tzif_reader.maximum_footer_bytes

type t =
  { initial : Local_type.t
  ; transitions : Transition.t array
  ; footer : Posix_footer.t option
  ; provenance : Zone.Provenance.t
  }

let last_transition transitions =
  if Array.is_empty transitions then None else Some (Array.last transitions)
;;

let of_string bytes ~source =
  let open Result.Let_syntax in
  if String.length bytes > maximum_bytes
  then Error Error.Too_large
  else (
    let%bind (_ : Zone.Provenance.t) =
      Zone.Provenance.create ~source ~sha256:(String.make 64 '0')
      |> Result.map_error ~f:(fun error -> Error.Invalid error)
    in
    let%bind data = Tzif_reader.of_string bytes in
    let%bind footer = Posix_footer.of_string data.footer ~version:data.version in
    let%bind () =
      match footer, last_transition data.transitions with
      | Some footer, Some last ->
        if Local_type.equal last.local_type (Posix_footer.local_type footer ~at:last.at)
        then Ok ()
        else
          Error
            (Error.Malformed
               { byte_offset = String.length bytes - String.length data.footer - 2
               ; reason = "footer disagrees with final transition type"
               })
      | None, _ | Some _, None -> Ok ()
    in
    let sha256 = Digestif.SHA256.(digest_string bytes |> to_hex) in
    let%map provenance =
      Zone.Provenance.create ~source ~sha256
      |> Result.map_error ~f:(fun error -> Error.Invalid error)
    in
    { initial = data.initial; transitions = data.transitions; footer; provenance })
;;

let provenance t = t.provenance
let nanoseconds time = Time_ns.to_int63_ns_since_epoch time |> Int63.to_int64

let floor_seconds ns =
  let seconds = Int64.(ns / 1_000_000_000L) in
  if Int64.(rem ns 1_000_000_000L < 0L) then Int64.(seconds - 1L) else seconds
;;

let table_type t ~at =
  let rec search lower upper =
    if lower >= upper
    then lower - 1
    else (
      let middle = lower + ((upper - lower) / 2) in
      if Int64.(t.transitions.(middle).at <= at)
      then search (middle + 1) upper
      else search lower middle)
  in
  let index = search 0 (Array.length t.transitions) in
  if index < 0 then t.initial else t.transitions.(index).local_type
;;

let type_at t ~at =
  match last_transition t.transitions with
  | None ->
    (match t.footer with
     | None -> Some t.initial
     | Some footer -> Some (Posix_footer.local_type footer ~at))
  | Some last ->
    if Int64.(at < last.at)
    then Some (table_type t ~at)
    else Option.map t.footer ~f:(fun footer -> Posix_footer.local_type footer ~at)
;;

let validate_type local_type =
  if String.equal local_type.Local_type.designation "-00"
  then Error Error.Coverage_unavailable
  else if local_type.offset_seconds < -86400 || local_type.offset_seconds > 86400
  then Error (Error.Unsupported_offset local_type.offset_seconds)
  else Ok local_type.offset_seconds
;;

let changes t ~from ~until =
  let open Result.Let_syntax in
  let%bind future =
    match t.footer with
    | None -> Ok []
    | Some footer ->
      let first_date, _ = Time_ns.Utc.to_date_and_span_since_start_of_day from in
      let last_date, _ = Time_ns.Utc.to_date_and_span_since_start_of_day until in
      let%map future =
        Posix_footer.transitions
          footer
          ~first_year:(Date.year first_date - 1)
          ~last_year:(Date.year last_date + 1)
      in
      (match last_transition t.transitions with
       | None -> future
       | Some last ->
         List.filter future ~f:(fun (event : Transition.t) -> Int64.(event.at >= last.at)))
  in
  let from_seconds = floor_seconds (nanoseconds from) in
  let until_seconds = floor_seconds (nanoseconds until) in
  let selected =
    List.append (Array.to_list t.transitions) future
    |> List.filter ~f:(fun (event : Transition.t) ->
      Int64.(event.at > from_seconds && event.at <= until_seconds))
    |> List.sort ~compare:(fun (a : Transition.t) (b : Transition.t) ->
      Int64.compare a.at b.at)
  in
  (* Final-table/footer equal-time duplicates have already been proved equal
     against the exact final type. Every other source has strictly ordered times. *)
  Ok
    (List.fold selected ~init:[] ~f:(fun (acc : Transition.t list) event ->
       match acc with
       | previous :: _ when Int64.equal previous.at event.at -> acc
       | [] | _ :: _ -> event :: acc)
     |> List.rev)
;;

let zone t ~name ~from ~until =
  let open Result.Let_syntax in
  let%bind () =
    Zone.validate_name name |> Result.map_error ~f:(fun error -> Error.Invalid error)
  in
  if Time_ns.compare from until > 0
  then Error (Error.Invalid (Invalid "coverage endpoints must be ordered"))
  else (
    let%bind initial =
      match type_at t ~at:(floor_seconds (nanoseconds from)) with
      | None -> Error Error.Coverage_unavailable
      | Some local_type -> validate_type local_type
    in
    let%bind () =
      match type_at t ~at:(floor_seconds (nanoseconds until)) with
      | None -> Error Error.Coverage_unavailable
      | Some _ -> Ok ()
    in
    let%bind changes = changes t ~from ~until in
    let%bind transitions, _, _ =
      List.fold_result
        changes
        ~init:([], initial, 0)
        ~f:(fun (acc, previous, count) (event : Transition.t) ->
          let%bind offset_seconds = validate_type event.local_type in
          if offset_seconds = previous
          then Ok (acc, previous, count)
          else if count = maximum_transitions
          then Error Error.Projection_limit
          else (
            (* Selected seconds lie strictly after the floored lower endpoint
               and at/before the upper endpoint. Their nanoseconds are inside
               the requested Time_ns interval, so neither multiplication nor
               narrowing can overflow. Out-of-window int64 seconds never enter. *)
            let at =
              Int64.(event.at * 1_000_000_000L)
              |> Int63.of_int64_exn
              |> Time_ns.of_int63_ns_since_epoch
            in
            Ok ({ Zone.Transition.at; offset_seconds } :: acc, offset_seconds, count + 1)))
    in
    Zone.create
      ~name
      ~provenance:t.provenance
      ~from
      ~until
      ~initial_offset_seconds:initial
      ~transitions:(List.rev transitions)
    |> Result.map_error ~f:(fun error -> Error.Invalid error))
;;
