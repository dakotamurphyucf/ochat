open Core

type t =
  { max_frame_bytes : int
  ; mutable frame_bytes : int
  ; mutable data_rev : string list
  ; mutable event_name : string option
  ; mutable first_line : bool
  ; mutable failed : bool
  }

type frame =
  | Payload of Jsonaf.t
  | Done

let create ?(max_frame_bytes = 16 * 1024 * 1024) () =
  if max_frame_bytes <= 0
  then Or_error.error_string "SSE frame limit must be positive"
  else
    Ok
      { max_frame_bytes
      ; frame_bytes = 0
      ; data_rev = []
      ; event_name = None
      ; first_line = true
      ; failed = false
      }
;;

let dispatch t =
  let data = List.rev t.data_rev in
  let event_name = t.event_name in
  t.data_rev <- [];
  t.event_name <- None;
  t.frame_bytes <- 0;
  match data with
  | [] -> Ok None
  | _ ->
    let data = String.concat ~sep:"\n" data in
    if String.equal data "[DONE]"
    then Ok (Some Done)
    else
      let open Or_error.Let_syntax in
      let%bind json = Jsonaf.parse data in
      let%map () =
        match event_name, Jsonaf.member "type" json with
        | Some name, Some (`String kind)
          when (not (String.is_empty name)) && not (String.equal name "message") ->
          if String.equal name kind
          then Ok ()
          else Or_error.error_string "SSE event name disagrees with payload type"
        | _ -> Ok ()
      in
      Some (Payload json)
;;

let feed_line t line =
  if t.failed
  then Or_error.error_string "SSE parser already failed"
  else (
    let line = Option.value (String.chop_suffix line ~suffix:"\r") ~default:line in
    let line =
      if t.first_line
      then Option.value (String.chop_prefix line ~prefix:"\239\187\191") ~default:line
      else line
    in
    t.first_line <- false;
    let result =
      if String.is_empty line
      then dispatch t
      else if String.length line >= t.max_frame_bytes - t.frame_bytes
      then Or_error.error_string "SSE frame exceeds configured byte limit"
      else (
        t.frame_bytes <- t.frame_bytes + String.length line + 1;
        (match String.lsplit2 line ~on:':' with
         | Some ("data", value) ->
           let value =
             Option.value (String.chop_prefix value ~prefix:" ") ~default:value
           in
           t.data_rev <- value :: t.data_rev
         | Some ("event", value) ->
           t.event_name
           <- Some (Option.value (String.chop_prefix value ~prefix:" ") ~default:value)
         | None when String.equal line "data" -> t.data_rev <- "" :: t.data_rev
         | None when String.equal line "event" -> t.event_name <- Some ""
         | _ -> ());
        Ok None)
    in
    if Result.is_error result then t.failed <- true;
    result)
;;

let finish t =
  let pending = not (List.is_empty t.data_rev) in
  t.data_rev <- [];
  t.event_name <- None;
  t.frame_bytes <- 0;
  t.failed <- true;
  pending
;;
