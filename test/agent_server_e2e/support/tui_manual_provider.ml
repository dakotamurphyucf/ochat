open Core
module P = Tui_stream_provider

type phase =
  | Held
  | Closed

type t =
  { provider : P.t
  ; phases : (int, phase) Hashtbl.t
  }

let create provider = { provider; phases = Hashtbl.create (module Int) }
let message_id index = sprintf "manual-message-%d" index
let reason_id index = sprintf "manual-reason-%d" index
let paused = "Paused test response: waiting for the operator."

let reasoning request index =
  let id = reason_id index in
  let text = "Fixture reasoning: checking the manual TUI workflow." in
  P.emit
    request
    [ P.added (P.reasoning id "")
    ; P.reasoning_delta id text
    ; P.done_ (P.reasoning id text)
    ]
;;

let begin_message request index text =
  let id = message_id index in
  P.emit request [ P.added (P.message id ""); P.text_delta id text ]
;;

let reply request ~env index =
  reasoning request index;
  Eio.Time.sleep (Eio.Stdenv.clock env) 0.5;
  begin_message request index "Manual response";
  let text = ref "Manual response" in
  List.iter
    [ ": streaming"; " text"; " works."; " Ready for the next check." ]
    ~f:(fun delta ->
      Eio.Time.sleep (Eio.Stdenv.clock env) 0.4;
      text := !text ^ delta;
      P.emit request [ P.text_delta (message_id index) delta ]);
  P.emit request [ P.done_ (P.message (message_id index) !text) ];
  P.finish request
;;

let background request index =
  if Poly.equal (Jsonaf.member "stream" (P.body request)) (Some `True)
  then (
    let item = P.message (message_id index) "background-result" in
    P.emit request [ P.added item; P.done_ item ];
    P.finish request)
  else P.reply_summary request "background-result"
;;

let suggestion index =
  String.concat
    ~sep:"\n"
    ([ " suggestion first line"; "second line: wide text 界 and café" ]
     @ List.init 12 ~f:(fun line ->
       sprintf
         "Preview line %02d: inspect scrolling and line-by-line acceptance."
         (line + 3))
     @ [ sprintf "END-OF-SUGGESTION request %d" index ])
;;

let fresh request ~env action index =
  match action with
  | "reply" ->
    reply request ~env index;
    Closed
  | "hold" ->
    reasoning request index;
    begin_message request index paused;
    Held
  | "fork" ->
    reasoning request index;
    P.emit request (P.fork_call_for ~call_id:(sprintf "manual-fork-%d" index));
    P.finish request;
    Closed
  | "summary" ->
    P.reply_summary
      request
      "Manual test summary: draft editing, streaming and tool approval were exercised.";
    Closed
  | "suggest" ->
    P.reply_summary request (suggestion index);
    Closed
  | "background" ->
    background request index;
    Closed
  | _ ->
    failwith
      "expected reply, hold, fork, summary, suggest, or background for a fresh request"
;;

let respond t ~env ~action ~index =
  let request = P.request_at t.provider index |> Option.value_exn in
  let streaming = Poly.equal (Jsonaf.member "stream" (P.body request)) (Some `True) in
  if
    (not (String.equal action "background"))
    && Bool.equal streaming (List.mem [ "summary"; "suggest" ] action ~equal:String.equal)
  then failwith "action does not match streaming/JSON request";
  let phase =
    match Hashtbl.find t.phases index with
    | Some Closed -> failwith "request is already closed"
    | Some Held when String.equal action "finish" ->
      P.emit
        request
        [ P.text_delta (message_id index) " Completed."
        ; P.done_ (P.message (message_id index) (paused ^ " Completed."))
        ];
      P.finish request;
      Closed
    | Some Held -> failwith "held request requires finish"
    | None -> fresh request ~env action index
  in
  Hashtbl.set t.phases ~key:index ~data:phase
;;
