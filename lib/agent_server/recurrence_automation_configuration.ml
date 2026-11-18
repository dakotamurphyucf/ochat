open! Core
module P = Agent_protocol

let invalid message = Error (P.Error.invalid_request message)

let check_sexp sexp =
  let rec walk depth nodes bytes = function
    | _ when depth > 16 || nodes <= 0 -> Error ()
    | Sexp.Atom atom ->
      let length = String.length atom in
      if length > bytes then Error () else Ok (nodes - 1, bytes - length)
    | List children ->
      List.fold_result
        children
        ~init:(nodes - 1, bytes)
        ~f:(fun (nodes, bytes) child -> walk (depth + 1) nodes bytes child)
  in
  match walk 0 2048 65536 sexp with
  | Ok _ -> ()
  | Error () -> Sexplib.Conv.of_sexp_error "recurrence configuration exceeds bounds" sexp
;;

module Zone_directory = struct
  type t =
    { directory : string
    ; source : string
    ; from : Time_ns.Alternate_sexp.t
    ; until : Time_ns.Alternate_sexp.t
    }
  [@@deriving compare, equal, sexp]

  let unchecked_t_of_sexp = t_of_sexp

  let validate t =
    let open Result.Let_syntax in
    if
      String.length t.directory > 4096
      || (not (Filename.is_absolute t.directory))
      || String.mem t.directory '\000'
    then invalid "recurrence zone directory must be a bounded absolute path"
    else if Time_ns.compare t.from t.until > 0
    then invalid "recurrence zone coverage endpoints must be ordered"
    else (
      let components =
        if String.equal t.directory "/"
        then []
        else String.drop_prefix t.directory 1 |> String.split ~on:'/'
      in
      let%bind (_ : Chatml_local_source.Directory_path.t) =
        Chatml_local_source.Directory_path.create
          components
          ~max_bytes:4096
          ~max_components:64
          ~checkpoint:Fn.id
        |> Result.map_error ~f:(fun _ ->
          P.Error.invalid_request "recurrence zone directory components are invalid")
      in
      let%map (_ : Recurrence_rules.Zone.Provenance.t) =
        Recurrence_rules.Zone.Provenance.create
          ~source:t.source
          ~sha256:(String.make 64 '0')
        |> Result.map_error ~f:(fun _ ->
          P.Error.invalid_request "recurrence zone source label is invalid")
      in
      ())
  ;;

  let create ~directory ~source ~from ~until =
    let t = { directory; source; from; until } in
    Result.map (validate t) ~f:(fun () -> t)
  ;;

  let t_of_sexp sexp =
    check_sexp sexp;
    let t = unchecked_t_of_sexp sexp in
    match validate t with
    | Ok () -> t
    | Error error -> Sexplib.Conv.of_sexp_error error.P.Error.message sexp
  ;;
end

type t =
  { principal_id : P.Id.Principal.t
  ; controllers : P.Id.Session.t list
  ; expires_at : P.Timestamp.t option
  ; zone_directory : Zone_directory.t option
  }
[@@deriving compare, equal, sexp]

let unchecked_t_of_sexp = t_of_sexp

let validate t =
  let open Result.Let_syntax in
  let%bind (_ : P.Id.Principal.t) =
    P.Id.Principal.of_string (P.Id.Principal.to_string t.principal_id)
  in
  let%bind count, bytes, _ =
    List.fold_result
      t.controllers
      ~init:(0, String.length (P.Id.Principal.to_string t.principal_id), [])
      ~f:(fun (count, bytes, seen) id ->
        let text = P.Id.Session.to_string id in
        let length = String.length text in
        if count >= 256 || length > 65536 - bytes
        then invalid "recurrence controller grants exceed admission bounds"
        else if List.mem seen id ~equal:P.Id.Session.equal
        then invalid "recurrence controller grants contain duplicates"
        else (
          let%map (_ : P.Id.Session.t) = P.Id.Session.of_string text in
          count + 1, bytes + length, id :: seen))
  in
  if count = 0
  then invalid "recurrence automation requires at least one explicit controller"
  else (
    match t.zone_directory with
    | None -> Ok ()
    | Some zone ->
      let%bind () = Zone_directory.validate zone in
      if String.length zone.directory + String.length zone.source > 65536 - bytes
      then invalid "recurrence administrative metadata exceeds admission bounds"
      else Ok ())
;;

let create ~principal_id ~controllers ~expires_at ~zone_directory =
  let t = { principal_id; controllers; expires_at; zone_directory } in
  Result.map (validate t) ~f:(fun () -> t)
;;

let t_of_sexp sexp =
  check_sexp sexp;
  let t = unchecked_t_of_sexp sexp in
  match validate t with
  | Ok () -> t
  | Error error -> Sexplib.Conv.of_sexp_error error.P.Error.message sexp
;;
