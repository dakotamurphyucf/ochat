open Core

type t =
  { max_records : int
  ; max_bytes : int
  ; write : string -> unit
  ; mutable records_rev : string list
  ; mutable retained_bytes : int
  ; mutable dropped_records : int
  }

let state : t option ref = ref None
let sequence = ref 0
let default_max_records = 16_384
let default_max_bytes = 8 * 1024 * 1024

let is_enabled () =
  Sys.getenv "OCHAT_TUI_SCROLL_TRACE"
  |> Option.exists ~f:(fun value -> not (String.is_empty value))
;;

let install_with_limits ~max_records ~max_bytes ~write =
  state
  := Some
       { max_records = Int.max 0 max_records
       ; max_bytes = Int.max 0 max_bytes
       ; write
       ; records_rev = []
       ; retained_bytes = 0
       ; dropped_records = 0
       }
;;

let clear () =
  state := None;
  sequence := 0
;;

let install ~datadir =
  clear ();
  if is_enabled ()
  then
    install_with_limits
      ~max_records:default_max_records
      ~max_bytes:default_max_bytes
      ~write:(fun contents ->
        try Io.log ~dir:datadir ~file:"chat-tui-scroll-trace.jsonl" contents with
        | _ -> ())
;;

let retain_record state record =
  let bytes = String.length record in
  if
    List.length state.records_rev >= state.max_records
    || bytes > state.max_bytes - state.retained_bytes
  then state.dropped_records <- state.dropped_records + 1
  else (
    state.records_rev <- record :: state.records_rev;
    state.retained_bytes <- state.retained_bytes + bytes)
;;

let emit ~phase fields =
  match !state with
  | None -> ()
  | Some state ->
    Int.incr sequence;
    let fields =
      [ "sequence", `Number (Int.to_string !sequence)
      ; "timestamp", `String (Time_ns.to_string_utc (Time_ns.now ()))
      ; "phase", `String phase
      ]
      @ fields
    in
    (try Jsonaf.to_string (`Object fields) ^ "\n" |> retain_record state with
     | _ -> ())
;;

let dropped_record state =
  `Object
    [ "sequence", `Number (Int.to_string (!sequence + 1))
    ; "timestamp", `String (Time_ns.to_string_utc (Time_ns.now ()))
    ; "phase", `String "trace_dropped"
    ; "count", `Number (Int.to_string state.dropped_records)
    ]
  |> Jsonaf.to_string
  |> fun record -> record ^ "\n"
;;

let flush () =
  match !state with
  | None -> ()
  | Some trace ->
    let records = List.rev trace.records_rev in
    let records =
      if trace.dropped_records <= 0 then records else records @ [ dropped_record trace ]
    in
    (try trace.write (String.concat records) with
     | _ -> ());
    clear ()
;;

module For_testing = struct
  let install = install_with_limits

  let retained_records () =
    Option.value_map !state ~default:0 ~f:(fun state -> List.length state.records_rev)
  ;;

  let dropped_records () =
    Option.value_map !state ~default:0 ~f:(fun state -> state.dropped_records)
  ;;
end

let vertical_direction_name = function
  | `Up -> "up"
  | `Down -> "down"
;;

let arrow_direction_name = function
  | `Up -> "up"
  | `Down -> "down"
  | `Left -> "left"
  | `Right -> "right"
;;

let modifiers modifiers =
  List.map modifiers ~f:(function
    | `Ctrl -> "ctrl"
    | `Meta -> "meta"
    | `Shift -> "shift")
  |> String.concat ~sep:","
;;

let special = function
  | `Escape -> "escape"
  | `Enter -> "enter"
  | `Tab -> "tab"
  | `Backspace -> "backspace"
  | `Insert -> "insert"
  | `Delete -> "delete"
  | `Home -> "home"
  | `End -> "end"
  | `Arrow direction -> "arrow-" ^ arrow_direction_name direction
  | `Page direction -> "page-" ^ vertical_direction_name direction
  | `Function number -> "function-" ^ Int.to_string number
;;

let event_name = function
  | `Key (`ASCII char, modifiers_) ->
    sprintf "key-ascii-%02x[%s]" (Char.to_int char) (modifiers modifiers_)
  | `Key (`Uchar uchar, modifiers_) ->
    sprintf "key-uchar-%04x[%s]" (Stdlib.Uchar.to_int uchar) (modifiers modifiers_)
  | `Key ((#Notty.Unescape.special as key), modifiers_) ->
    sprintf "key-%s[%s]" (special key) (modifiers modifiers_)
  | `Mouse (`Press (`Scroll direction_), _, modifiers_) ->
    sprintf
      "mouse-scroll-%s[%s]"
      (vertical_direction_name direction_)
      (modifiers modifiers_)
  | `Mouse (`Press _, _, modifiers_) -> sprintf "mouse-press[%s]" (modifiers modifiers_)
  | `Mouse (`Drag, _, modifiers_) -> sprintf "mouse-drag[%s]" (modifiers modifiers_)
  | `Mouse (`Release, _, modifiers_) -> sprintf "mouse-release[%s]" (modifiers modifiers_)
  | `Paste `Start -> "paste-start"
  | `Paste `End -> "paste-end"
;;
