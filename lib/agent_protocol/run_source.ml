open! Core
module J = Json_codec

type t =
  { observer : Invocation.observer
  ; generation : int
  ; installation_epoch : int64
  }
[@@deriving equal]

let validate t =
  let open Result.Let_syntax in
  let%bind () =
    Extension_codec.text ~name:"run source script ID" ~max:256 t.observer.script_id
  in
  let valid_digest =
    String.length t.observer.source_sha256 = 64
    && String.for_all t.observer.source_sha256 ~f:(function
      | '0' .. '9' | 'a' .. 'f' -> true
      | _ -> false)
  in
  if t.generation < 0 || Int64.(t.installation_epoch <= 0L) || not valid_digest
  then Error (Protocol_error.invalid_request "invalid run source identity")
  else Ok ()
;;

let create ~observer ~generation ~installation_epoch =
  let t = { observer; generation; installation_epoch } in
  Result.map (validate t) ~f:(fun () -> t)
;;

let to_json t =
  `Object
    [ "script_id", `String t.observer.script_id
    ; "source_sha256", `String t.observer.source_sha256
    ; "generation", `Number (Int.to_string t.generation)
    ; "installation_epoch", `String (Int64.to_string t.installation_epoch)
    ]
;;

let epoch_of_json = function
  | `String encoded ->
    (match Int64.of_string_opt encoded with
     | Some value when Int64.(value > 0L) && String.equal (Int64.to_string value) encoded
       -> Ok value
     | Some _ | None ->
       Error (Protocol_error.invalid_request "invalid run installation epoch"))
  | _ ->
    Error (Protocol_error.invalid_request "run installation epoch must be decimal string")
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind f = J.fields json in
  let%bind script_id = J.required_as f "script_id" J.string in
  let%bind source_sha256 = J.required_as f "source_sha256" J.string in
  let%bind generation =
    J.required_as f "generation" (J.bounded_int ~min:0 ~max:Int.max_value)
  in
  let%bind installation_epoch = J.required_as f "installation_epoch" epoch_of_json in
  create ~observer:{ script_id; source_sha256 } ~generation ~installation_epoch
;;

let sexp_of_t t = Jsonaf.sexp_of_t (to_json t)

let t_of_sexp sexp =
  match of_json (Jsonaf.t_of_sexp sexp) with
  | Ok t -> t
  | Error e -> Sexplib.Conv.of_sexp_error e.message sexp
;;
