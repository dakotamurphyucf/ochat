open Core

module Provenance = struct
  type t =
    { source : string
    ; sha256 : string
    }
  [@@deriving equal, sexp_of]

  let create ~source ~sha256 =
    if
      String.is_empty source
      || String.length source > 256
      || String.length sha256 <> 64
      || not
           (String.for_all sha256 ~f:(fun c ->
              Char.is_digit c || Char.between c ~low:'a' ~high:'f'))
    then Error (Timing_error.Invalid "invalid zone provenance")
    else Ok { source; sha256 }
  ;;

  let source t = t.source
  let sha256 t = t.sha256
end

module Transition = struct
  type t =
    { at : Time_ns.Alternate_sexp.t
    ; offset_seconds : int
    }
  [@@deriving equal, sexp]
end

type t =
  { name : string
  ; provenance : Provenance.t
  ; from : Time_ns.Alternate_sexp.t
  ; until : Time_ns.Alternate_sexp.t
  ; initial_offset_seconds : int
  ; transitions : Transition.t array
  ; offsets : int list
  }

let validate_name name =
  if
    String.is_empty name
    || String.length name > 255
    || (not
          (String.for_all name ~f:(fun c ->
             Char.is_alphanum c || List.mem [ '/'; '_'; '-'; '+' ] c ~equal:Char.equal)))
    || List.exists (String.split name ~on:'/') ~f:String.is_empty
  then Error (Timing_error.Invalid "invalid named zone identifier")
  else Ok ()
;;

let valid_offset offset = offset >= -86_400 && offset <= 86_400

let create ~name ~provenance ~from ~until ~initial_offset_seconds ~transitions =
  let open Result.Let_syntax in
  let%bind () = validate_name name in
  let rec bounded_length remaining = function
    | [] -> true
    | _ :: rest -> remaining > 0 && bounded_length (remaining - 1) rest
  in
  if
    Time_ns.compare from until > 0
    || (not (valid_offset initial_offset_seconds))
    || not (bounded_length 4096 transitions)
  then Error (Timing_error.Invalid "invalid zone coverage, offset or transition count")
  else (
    let%bind _ =
      List.fold_result transitions ~init:from ~f:(fun previous transition ->
        if
          Time_ns.compare previous transition.Transition.at < 0
          && Time_ns.compare transition.at until <= 0
          && valid_offset transition.offset_seconds
        then Ok transition.at
        else Error (Timing_error.Invalid "invalid zone transition ordering or offset"))
    in
    Ok
      { name
      ; provenance
      ; from
      ; until
      ; initial_offset_seconds
      ; transitions = Array.of_list transitions
      ; offsets =
          initial_offset_seconds :: List.map transitions ~f:(fun t -> t.offset_seconds)
          |> List.dedup_and_sort ~compare:Int.compare
      })
;;

let name t = t.name
let provenance t = t.provenance
let coverage t = t.from, t.until

let offset_seconds t at =
  if Time_ns.compare at t.from < 0 || Time_ns.compare at t.until > 0
  then Error Timing_error.Out_of_coverage
  else (
    let rec search lower upper =
      if lower >= upper
      then lower
      else (
        let middle = lower + ((upper - lower) / 2) in
        if Time_ns.compare t.transitions.(middle).at at <= 0
        then search (middle + 1) upper
        else search lower middle)
    in
    let index = search 0 (Array.length t.transitions) in
    Ok
      (if Int.equal index 0
       then t.initial_offset_seconds
       else t.transitions.(index - 1).offset_seconds))
;;

let offset_ns offset = Int64.(of_int offset * 1_000_000_000L)

let to_local t at =
  let open Result.Let_syntax in
  let%bind offset = offset_seconds t at in
  let%map scalar = Time_arithmetic.add_ns at (offset_ns offset) in
  Time_arithmetic.utc_parts scalar
;;

module Resolution = struct
  type t =
    | Candidates of Time_ns.Alternate_sexp.t list
    | Gap of { ends_at : Time_ns.Alternate_sexp.t }
  [@@deriving equal, sexp]
end

let resolve t ~date ~ofday =
  let open Result.Let_syntax in
  let%bind scalar = Time_arithmetic.local_scalar date ofday in
  let local_ns = Time_arithmetic.nanoseconds scalar in
  let margin = 86_400_000_000_000L in
  (* local_ns is a signed Int63 endpoint. Adding/subtracting one day in Int64
     therefore cannot overflow, including at both representable timestamp edges. *)
  if
    Int64.(local_ns - margin < Time_arithmetic.nanoseconds t.from)
    || Int64.(local_ns + margin > Time_arithmetic.nanoseconds t.until)
  then Error Timing_error.Out_of_coverage
  else (
    let%bind candidates =
      List.filter_map t.offsets ~f:(fun offset ->
        let candidate_ns = Int64.(local_ns - offset_ns offset) in
        match Time_arithmetic.of_nanoseconds candidate_ns with
        | Error error -> Some (Error error)
        | Ok candidate ->
          Some
            (let%map actual_offset = offset_seconds t candidate in
             Option.some_if (Int.equal actual_offset offset) candidate))
      |> Result.all
      |> Result.map ~f:List.filter_opt
    in
    match List.dedup_and_sort candidates ~compare:Time_ns.compare with
    | _ :: _ as candidates -> Ok (Resolution.Candidates candidates)
    | [] ->
      let rec find_gap previous_offset index =
        if index >= Array.length t.transitions
        then Error Timing_error.Out_of_coverage
        else (
          let transition = t.transitions.(index) in
          let at = Time_arithmetic.nanoseconds transition.at in
          let lower = Int64.(at + offset_ns previous_offset) in
          let upper = Int64.(at + offset_ns transition.offset_seconds) in
          if Int64.(lower <= local_ns && local_ns < upper)
          then Ok (Resolution.Gap { ends_at = transition.at })
          else find_gap transition.offset_seconds (index + 1))
      in
      find_gap t.initial_offset_seconds 0)
;;
