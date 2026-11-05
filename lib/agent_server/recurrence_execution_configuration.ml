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
  match walk 0 8192 65536 sexp with
  | Ok _ when String.length (Sexp.to_string_mach sexp) <= 65536 -> ()
  | Ok _ | Error () ->
    Sexplib.Conv.of_sexp_error "recurrence execution configuration exceeds bounds" sexp
;;

module Controller = struct
  type t =
    { session_id : P.Id.Session.t
    ; existing_targets : P.Id.Session.t list
    ; captured_children : bool
    }
  [@@deriving compare, equal, sexp]

  let unchecked_t_of_sexp = t_of_sexp

  let validate t =
    let open Result.Let_syntax in
    let%bind (_ : P.Id.Session.t) =
      P.Id.Session.of_string (P.Id.Session.to_string t.session_id)
    in
    let%bind count, _ =
      List.fold_result t.existing_targets ~init:(0, []) ~f:(fun (count, seen) target ->
        if count >= 256
        then invalid "recurrence execution target grants exceed admission bounds"
        else if List.mem seen target ~equal:P.Id.Session.equal
        then invalid "recurrence execution target grants contain duplicates"
        else (
          let%map (_ : P.Id.Session.t) =
            P.Id.Session.of_string (P.Id.Session.to_string target)
          in
          count + 1, target :: seen))
    in
    if count = 0 && not t.captured_children
    then invalid "recurrence execution requires an explicit target or captured grant"
    else if String.length (Sexp.to_string_mach (sexp_of_t t)) > 65536
    then invalid "recurrence execution controller metadata exceeds admission bounds"
    else Ok ()
  ;;

  let create ~session_id ~existing_targets ~captured_children =
    let t = { session_id; existing_targets; captured_children } in
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
  ; controllers : Controller.t list
  ; expires_at : P.Timestamp.t option
  }
[@@deriving compare, equal, sexp]

let unchecked_t_of_sexp = t_of_sexp

let validate t =
  let open Result.Let_syntax in
  let%bind (_ : P.Id.Principal.t) =
    P.Id.Principal.of_string (P.Id.Principal.to_string t.principal_id)
  in
  let%bind _count, _edges, _ =
    List.fold_result t.controllers ~init:(0, 0, []) ~f:(fun (count, edges, seen) row ->
      if count >= 256
      then invalid "recurrence execution controller grants exceed admission bounds"
      else if List.mem seen row.Controller.session_id ~equal:P.Id.Session.equal
      then invalid "recurrence execution controller grants contain duplicates"
      else (
        let%bind () = Controller.validate row in
        let edges = edges + List.length row.existing_targets in
        if edges > 1024
        then invalid "recurrence execution target edges exceed admission bounds"
        else Ok (count + 1, edges, row.session_id :: seen)))
  in
  if String.length (Sexp.to_string_mach (sexp_of_t t)) > 65536
  then invalid "recurrence execution metadata exceeds admission bounds"
  else Ok ()
;;

let create ~principal_id ~controllers ~expires_at =
  let t = { principal_id; controllers; expires_at } in
  Result.map (validate t) ~f:(fun () -> t)
;;

let t_of_sexp sexp =
  check_sexp sexp;
  let t = unchecked_t_of_sexp sexp in
  match validate t with
  | Ok () -> t
  | Error error -> Sexplib.Conv.of_sexp_error error.P.Error.message sexp
;;
