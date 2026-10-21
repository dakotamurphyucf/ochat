open! Core

module Limits = struct
  type t =
    { max_map_bytes : int
    ; max_segments : int
    ; max_work_steps : int
    }

  let create ~max_map_bytes ~max_segments ~max_work_steps =
    if max_map_bytes <= 0 || max_segments <= 0 || max_work_steps <= 0
    then Or_error.error_string "ChatMD extraction-map limits must be positive"
    else Ok { max_map_bytes; max_segments; max_work_steps }
  ;;
end

module Budget = struct
  type t =
    { limits : Limits.t
    ; mutable remaining_bytes : int
    ; mutable remaining_segments : int
    ; mutable remaining_work : int
    ; mutable exhausted : bool
    }

  let create limits =
    { limits
    ; remaining_bytes = limits.max_map_bytes
    ; remaining_segments = limits.max_segments
    ; remaining_work = limits.max_work_steps
    ; exhausted = false
    }
  ;;

  let charge t ~bytes ~segments ~work =
    if
      t.exhausted
      || bytes < 0
      || segments < 0
      || work < 0
      || bytes > t.remaining_bytes
      || segments > t.remaining_segments
      || work > t.remaining_work
    then (
      t.exhausted <- true;
      false)
    else (
      t.remaining_bytes <- t.remaining_bytes - bytes;
      t.remaining_segments <- t.remaining_segments - segments;
      t.remaining_work <- t.remaining_work - work;
      true)
  ;;
end

module Input_span = struct
  type t =
    { input : Chatmd_parser_input.t
    ; start_byte : int
    ; end_byte : int
    }

  let input t = t.input
  let start_byte t = t.start_byte
  let end_byte t = t.end_byte
end

module Raw_projection = struct
  type t =
    | Exact_inline of Input_span.t
    | External_origin of
        { member : string
        ; body_digest : Chatml_source.Digest.t
        ; start_byte : int
        ; end_byte : int
        }
    | Approximate_declaration of Chatmd_parser_input.Range.t
end

module Segment = struct
  type origin =
    | Exact of
        { input : Chatmd_parser_input.t
        ; start_byte : int
        }
    | Unmapped

  type t =
    { output_start : int
    ; output_end : int
    ; origin : origin
    }
end

type kind =
  | Inline of Segment.t list
  | External of string
  | Degraded

type t =
  { declaration : Chatmd_parser_input.Range.t
  ; body : string
  ; kind : kind
  ; query_work_limit : int
  }

type map = t

type bound =
  { map : t
  ; source : Chatml_source.t
  }

module Producer = struct
  type t =
    { budget : Budget.t
    ; declaration : Chatmd_parser_input.Range.t
    ; checkpoint : unit -> unit
    ; output : Buffer.t
    ; mutable segments_rev : Segment.t list
    ; mutable degraded : bool
    }

  let create budget ~declaration ~checkpoint =
    checkpoint ();
    { budget
    ; declaration
    ; checkpoint
    ; output = Buffer.create 256
    ; segments_rev = []
    ; degraded = budget.exhausted
    }
  ;;

  let degrade t =
    t.degraded <- true;
    t.segments_rev <- []
  ;;

  let charge t ~bytes ~segments ~work =
    if t.degraded
    then false
    else if Budget.charge t.budget ~bytes ~segments ~work
    then true
    else (
      degrade t;
      false)
  ;;

  let append_unmapped t output =
    t.checkpoint ();
    let output_start = Buffer.length t.output in
    Buffer.add_string t.output output;
    if
      charge
        t
        ~bytes:(String.length output)
        ~segments:(if String.is_empty output then 0 else 1)
        ~work:1
      && not (String.is_empty output)
    then
      t.segments_rev
      <- { Segment.output_start; output_end = Buffer.length t.output; origin = Unmapped }
         :: t.segments_rev
  ;;

  let append_text t ~input ~fragments ~output =
    t.checkpoint ();
    let output_start = Buffer.length t.output in
    Buffer.add_string t.output output;
    if charge t ~bytes:(String.length output) ~segments:0 ~work:1
    then (
      let module F = Chatmd_parser_builder.Text_fragment in
      let input_bytes = Chatmd_parser_input.effective input in
      let cursor = ref 0 in
      List.iter fragments ~f:(fun fragment ->
        t.checkpoint ();
        if not t.degraded
        then (
          let text = F.text fragment in
          let start_byte = F.start_byte fragment in
          let end_byte = F.end_byte fragment in
          let length = String.length text in
          if
            start_byte < 0
            || end_byte < start_byte
            || end_byte > String.length input_bytes
            || (not (Int.equal (end_byte - start_byte) length))
            || length > String.length output - !cursor
          then degrade t
          else if charge t ~bytes:0 ~segments:(if length = 0 then 0 else 1) ~work:1
          then (
            let offset = ref 0 in
            while !offset < length && not t.degraded do
              if !offset mod 4096 = 0
              then (
                t.checkpoint ();
                ignore (charge t ~bytes:0 ~segments:0 ~work:1 : bool));
              if
                (not (Char.equal text.[!offset] input_bytes.[start_byte + !offset]))
                || not (Char.equal text.[!offset] output.[!cursor + !offset])
              then degrade t;
              incr offset
            done;
            if (not t.degraded) && length > 0
            then
              t.segments_rev
              <- { Segment.output_start = output_start + !cursor
                 ; output_end = output_start + !cursor + length
                 ; origin = Exact { input; start_byte }
                 }
                 :: t.segments_rev;
            cursor := !cursor + length)));
      if not (Int.equal !cursor (String.length output)) then degrade t)
  ;;

  let finish_inline t =
    t.checkpoint ();
    let body = Buffer.contents t.output in
    ( body
    , { declaration = t.declaration
      ; body
      ; kind = (if t.degraded then Degraded else Inline (List.rev t.segments_rev))
      ; query_work_limit = t.budget.limits.max_work_steps
      } )
  ;;

  let finish_external t ~member ~source_text =
    t.checkpoint ();
    let mapped =
      Int.equal (Buffer.length t.output) 0
      && charge t ~bytes:(String.length source_text) ~segments:1 ~work:1
    in
    ( source_text
    , { declaration = t.declaration
      ; body = source_text
      ; kind = (if mapped then External member else Degraded)
      ; query_work_limit = t.budget.limits.max_work_steps
      } )
  ;;
end

module Error = struct
  type t =
    | Stale_script_source
    | Invalid_compiler_location
    | Invalid_authored_point
  [@@deriving sexp_of]
end

module Authored_point = struct
  type unavailable =
    | Degraded_map
    | External_body
    | Effective_only
    | Foreign_input
    | Query_work_limit
    | Unmapped_or_ambiguous
  [@@deriving sexp_of]

  type t =
    | Exact_compiled_byte of int
    | Unavailable of unavailable
  [@@deriving sexp_of]
end

let equal_bytes left right ~checkpoint =
  if String.length left <> String.length right
  then false
  else (
    let offset = ref 0 in
    let equal = ref true in
    while !equal && !offset < String.length left do
      checkpoint ();
      let stop = !offset + Int.min 4096 (String.length left - !offset) in
      while !equal && !offset < stop do
        equal := Char.equal left.[!offset] right.[!offset];
        incr offset
      done
    done;
    checkpoint ();
    !equal)
;;

let bind t ~compiled_source ~checkpoint =
  checkpoint ();
  if
    equal_bytes t.body (Chatml_source.contents compiled_source) ~checkpoint
    && Chatml_source.Digest.equal
         (Chatml_source.digest compiled_source)
         (Chatml_source.Digest.of_contents t.body ~checkpoint)
  then Ok { map = t; source = compiled_source }
  else Error Error.Stale_script_source
;;

exception Query_limit

let authored_point bound ~input ~authored_byte ~checkpoint =
  let module Point = Authored_point in
  if authored_byte < 0 || authored_byte > Chatmd_parser_input.authored_byte_length input
  then Error Error.Invalid_authored_point
  else (
    let map = bound.map in
    let remaining = ref map.query_work_limit in
    let poll () =
      if !remaining <= 0 then raise Query_limit;
      decr remaining;
      checkpoint ()
    in
    try
      poll ();
      match map.kind with
      | Degraded -> Ok (Point.Unavailable Degraded_map)
      | External _ -> Ok (Point.Unavailable External_body)
      | Inline segments ->
        let seen_input = ref false in
        let candidate = ref None in
        let ambiguous = ref false in
        List.iter segments ~f:(fun segment ->
          poll ();
          match segment.origin with
          | Unmapped -> ()
          | Exact { input = owner; start_byte } when phys_equal owner input ->
            seen_input := true;
            let end_byte = start_byte + (segment.output_end - segment.output_start) in
            if start_byte <= authored_byte && authored_byte <= end_byte
            then (
              let compiled = segment.output_start + (authored_byte - start_byte) in
              match !candidate with
              | None -> candidate := Some compiled
              | Some existing ->
                if not (Int.equal existing compiled) then ambiguous := true)
          | Exact _ -> ());
        (match !seen_input, !candidate with
         | false, _ -> Ok (Point.Unavailable Foreign_input)
         | true, None -> Ok (Point.Unavailable Unmapped_or_ambiguous)
         | true, Some compiled when !ambiguous ->
           Ok (Point.Unavailable Unmapped_or_ambiguous)
         | true, Some compiled ->
           let boundary_safe = ref true in
           let has_left = ref false in
           let has_right = ref false in
           let in_segment = ref false in
           List.iter segments ~f:(fun segment ->
             poll ();
             if Int.equal segment.output_end compiled then has_left := true;
             if Int.equal segment.output_start compiled then has_right := true;
             if segment.output_start < compiled && compiled < segment.output_end
             then in_segment := true;
             if
               Int.equal segment.output_start compiled
               || Int.equal segment.output_end compiled
             then (
               match segment.origin with
               | Unmapped -> boundary_safe := false
               | Exact { input = owner; start_byte } ->
                 let source_byte = start_byte + (compiled - segment.output_start) in
                 if not (phys_equal owner input && Int.equal source_byte authored_byte)
                 then boundary_safe := false));
           if
             (not !in_segment)
             && ((compiled > 0 && not !has_left)
                 || (compiled < String.length map.body && not !has_right))
           then boundary_safe := false;
           if not !boundary_safe
           then Ok (Point.Unavailable Unmapped_or_ambiguous)
           else if
             not
               (Chatmd_parser_input.authored_matches
                  input
                  (Chatmd_parser_input.effective input)
                  ~checkpoint:poll)
           then Ok (Point.Unavailable Effective_only)
           else Ok (Point.Exact_compiled_byte compiled))
    with
    | Query_limit -> Ok (Point.Unavailable Query_work_limit))
;;

let project_raw bound location ~checkpoint =
  let map = bound.map in
  let source = bound.source in
  if
    (not
       (Chatml_source.Id.equal
          (Chatml_source.Location.source location)
          (Chatml_source.id source)))
    || not
         (Chatml_source.Digest.equal
            (Chatml_source.Location.source_digest location)
            (Chatml_source.digest source))
  then Error Error.Invalid_compiler_location
  else (
    let span = Chatml_source.Location.span location in
    let start_byte = span.left.offset in
    let end_byte = span.right.offset in
    if start_byte < 0 || end_byte < start_byte || end_byte > String.length map.body
    then Error Error.Invalid_compiler_location
    else (
      let remaining = ref map.query_work_limit in
      let poll () =
        if !remaining <= 0 then raise Query_limit;
        decr remaining;
        checkpoint ()
      in
      let approximate () = Raw_projection.Approximate_declaration map.declaration in
      try
        poll ();
        match map.kind with
        | Degraded -> Ok (approximate ())
        | External member ->
          Ok
            (Raw_projection.External_origin
               { member; body_digest = Chatml_source.digest source; start_byte; end_byte })
        | Inline segments ->
          let origin_at_empty () =
            let left = ref None in
            let right = ref None in
            List.iter segments ~f:(fun segment ->
              poll ();
              if Int.equal segment.output_end start_byte then left := Some segment;
              if Int.equal segment.output_start start_byte then right := Some segment;
              if segment.output_start < start_byte && start_byte < segment.output_end
              then (
                left := Some segment;
                right := Some segment));
            match !left, !right with
            | Some left, Some right ->
              (match left.origin, right.origin with
               | Exact a, Exact b when phys_equal a.input b.input ->
                 let left_byte = a.start_byte + (start_byte - left.output_start) in
                 let right_byte = b.start_byte + (start_byte - right.output_start) in
                 Option.some_if (Int.equal left_byte right_byte) (a.input, left_byte)
               | _ -> None)
            | Some segment, None when Int.equal start_byte (String.length map.body) ->
              (match segment.origin with
               | Exact { input; start_byte = source_start } ->
                 Some (input, source_start + (start_byte - segment.output_start))
               | Unmapped -> None)
            | None, Some segment when Int.equal start_byte 0 ->
              (match segment.origin with
               | Exact { input; start_byte = source_start } ->
                 Some (input, source_start + (start_byte - segment.output_start))
               | Unmapped -> None)
            | _ -> None
          in
          let exact =
            if Int.equal start_byte end_byte
            then
              Option.map (origin_at_empty ()) ~f:(fun (owner_input, offset) ->
                Input_span.{ input = owner_input; start_byte = offset; end_byte = offset })
            else (
              let next_output = ref start_byte in
              let source_start = ref None in
              let source_end = ref None in
              let owner = ref None in
              List.iter segments ~f:(fun segment ->
                poll ();
                if
                  !next_output < end_byte
                  && segment.output_end > !next_output
                  && segment.output_start <= !next_output
                then (
                  let overlap_end = Int.min end_byte segment.output_end in
                  match segment.origin with
                  | Unmapped -> ()
                  | Exact { input; start_byte = source_byte } ->
                    let from = source_byte + (!next_output - segment.output_start) in
                    let until = from + (overlap_end - !next_output) in
                    let contiguous =
                      match !owner, !source_end with
                      | None, None -> true
                      | Some previous, Some previous_end ->
                        phys_equal previous input && Int.equal previous_end from
                      | _ -> false
                    in
                    if contiguous
                    then (
                      if Option.is_none !owner
                      then (
                        owner := Some input;
                        source_start := Some from);
                      source_end := Some until;
                      next_output := overlap_end)));
              match !owner, !source_start, !source_end with
              | Some owner_input, Some source_start, Some source_end
                when Int.equal !next_output end_byte ->
                Some
                  Input_span.
                    { input = owner_input
                    ; start_byte = source_start
                    ; end_byte = source_end
                    }
              | _ -> None)
          in
          (match exact with
           | None -> Ok (approximate ())
           | Some span ->
             let input = Input_span.input span in
             if
               Chatmd_parser_input.authored_matches
                 input
                 (Chatmd_parser_input.effective input)
                 ~checkpoint:poll
             then Ok (Raw_projection.Exact_inline span)
             else Ok (approximate ()))
      with
      | Query_limit -> Ok (approximate ())))
;;
