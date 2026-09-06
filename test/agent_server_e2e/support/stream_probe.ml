open Core

type t =
  { mutable bytes : int
  ; mutable reads : int
  ; mutable finished : bool
  ; mutable failed : bool
  ; mutable pending : string
  ; mutable events : int String.Map.t
  ; mutable headers : (string * Jsonaf.t) list
  }

let create () =
  { bytes = 0
  ; reads = 0
  ; finished = false
  ; failed = false
  ; pending = ""
  ; events = String.Map.empty
  ; headers = []
  }
;;

let record_line t line =
  match String.chop_prefix (String.strip line) ~prefix:"event: " with
  | Some name when String.length name < 128 ->
    if
      (Map.length t.events < 128 || Map.mem t.events name)
      && String.for_all name ~f:(fun char ->
        Char.is_alphanum char || Char.equal char '.' || Char.equal char '_')
    then t.events <- Map.update t.events name ~f:(fun n -> Option.value n ~default:0 + 1)
  | _ -> ()
;;

let consume t bytes =
  t.reads <- t.reads + 1;
  t.bytes <- t.bytes + String.length bytes;
  let lines = String.split (t.pending ^ bytes) ~on:'\n' in
  let reversed = List.rev lines in
  t.pending <- List.hd_exn reversed;
  if String.length t.pending > 65_536 then t.pending <- "";
  List.iter (List.rev (List.tl_exn reversed)) ~f:(record_line t);
  bytes
;;

let observe t original =
  let stream = Piaf.Body.to_string_stream original in
  Piaf.Stream.from ~f:(fun () ->
    match Piaf.Stream.take stream with
    | Some bytes -> Some (consume t bytes)
    | None ->
      t.finished <- true;
      t.failed <- Piaf.Body.is_errored original;
      None)
;;

let wrap t response =
  let original = response.Piaf.Response.body in
  t.headers
  <- List.map
       [ "content-type"; "content-encoding"; "transfer-encoding"; "content-length" ]
       ~f:(fun name ->
         ( name
         , Option.value_map
             (Piaf.Headers.get response.headers name)
             ~default:`Null
             ~f:(fun value -> `String value) ));
  let stream = observe t original in
  let body = Piaf.Body.of_string_stream ~length:(Piaf.Body.length original) stream in
  Piaf.Response.with_ response ~body
;;

let metrics t =
  [ "body_bytes_consumed", `Number (Int.to_string t.bytes)
  ; "body_reads", `Number (Int.to_string t.reads)
  ; ("body_closed", if t.finished then `True else `False)
  ; ("body_failed", if t.failed then `True else `False)
  ; ( "event_counts"
    , `Object
        (Map.to_alist t.events
         |> List.map ~f:(fun (name, count) -> name, `Number (Int.to_string count))) )
  ; "response_headers", `Object t.headers
  ]
;;

let self_check () =
  let chunks =
    ref [ "eve"; "nt: response.created\ndata: {}\n\n"; "event: response.completed\n\n" ]
  in
  let expected = String.concat !chunks in
  let stream =
    Piaf.Stream.from ~f:(fun () ->
      match !chunks with
      | [] -> None
      | head :: tail ->
        chunks := tail;
        Some head)
  in
  let probe = create () in
  let response = Piaf.Response.of_string_stream ~body:stream `OK |> wrap probe in
  let actual =
    Piaf.Body.to_string response.body
    |> Result.map_error ~f:Piaf.Error.to_string
    |> Result.ok_or_failwith
  in
  assert (String.equal actual expected);
  assert (probe.finished && not probe.failed);
  assert (probe.reads = 3 && probe.bytes = String.length expected);
  assert (Option.equal Int.equal (Map.find probe.events "response.created") (Some 1));
  assert (Option.equal Int.equal (Map.find probe.events "response.completed") (Some 1))
;;
