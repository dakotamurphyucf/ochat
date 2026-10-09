open! Core
module P = Agent_protocol

module Key = struct
  type t =
    { scope_identity : string
    ; session : P.Session_ref.t
    ; generation : int
    ; session_revision : int64
    ; canonical_index : int
    }
  [@@deriving equal]

  let create ~scope_identity ~session ~generation ~session_revision ~canonical_index =
    if
      String.is_empty scope_identity
      || String.length scope_identity > 4096
      || generation < 0
      || Int64.(session_revision < 0L)
      || canonical_index < 0
    then Error (P.Error.invalid_request "invalid search cache observation")
    else Ok { scope_identity; session; generation; session_revision; canonical_index }
  ;;

  let same_session t other = P.Session_ref.equal t.session other.session

  let supersedes t other =
    same_session t other
    && String.equal t.scope_identity other.scope_identity
    && not
         (Int.equal t.generation other.generation
          && Int64.equal t.session_revision other.session_revision)
  ;;

  let accounted_bytes t =
    192
    + String.length t.scope_identity
    + String.length (P.Id.Server.to_string (P.Session_ref.server_id t.session))
    + String.length (P.Id.Session.to_string (P.Session_ref.session_id t.session))
  ;;
end

module Entry = struct
  type t =
    { key : Key.t
    ; value : Search_entry.t option
    ; accounted_bytes : int
    }

  let create key value =
    { key
    ; value
    ; accounted_bytes =
        Key.accounted_bytes key
        + Option.value_map value ~default:32 ~f:Search_entry.accounted_bytes
    }
  ;;
end

type t =
  { max_bytes : int
  ; max_entries : int
  ; max_session_entries : int
  ; mutable recent : Entry.t list
  }

let create ?(max_bytes = 8_388_608) ?(max_entries = 256) ?(max_session_entries = 32) () =
  if
    max_bytes < 1
    || max_bytes > 67_108_864
    || max_entries < 1
    || max_entries > 4096
    || max_session_entries < 1
    || max_session_entries > max_entries
  then Error (P.Error.invalid_request "invalid search cache budgets")
  else Ok { max_bytes; max_entries; max_session_entries; recent = [] }
;;

let find t key =
  match List.find t.recent ~f:(fun entry -> Key.equal entry.key key) with
  | None -> `Miss
  | Some found ->
    t.recent
    <- found :: List.filter t.recent ~f:(fun entry -> not (Key.equal entry.key key));
    `Hit found.value
;;

let add t key value =
  let entry = Entry.create key value in
  let remaining =
    List.filter t.recent ~f:(fun previous ->
      not (Key.equal key previous.key || Key.supersedes key previous.key))
  in
  let candidates =
    if entry.accounted_bytes > t.max_bytes then remaining else entry :: remaining
  in
  let _, _, _, _, kept =
    List.fold
      candidates
      ~init:(false, 0, 0, 0, [])
      ~f:(fun (full, count, bytes, session_count, kept) candidate ->
        let same_session = Key.same_session key candidate.Entry.key in
        if full || (same_session && session_count >= t.max_session_entries)
        then full, count, bytes, session_count, kept
        else if count >= t.max_entries || candidate.accounted_bytes > t.max_bytes - bytes
        then true, count, bytes, session_count, kept
        else
          ( false
          , count + 1
          , bytes + candidate.accounted_bytes
          , session_count + Bool.to_int same_session
          , candidate :: kept ))
  in
  t.recent <- List.rev kept
;;

let invalidate_session t session =
  t.recent
  <- List.filter t.recent ~f:(fun entry ->
       not (P.Session_ref.equal entry.Entry.key.session session))
;;

let clear t = t.recent <- []

module Stats = struct
  type t =
    { entries : int
    ; accounted_bytes : int
    }
  [@@deriving sexp_of]
end

let stats t =
  Stats.
    { entries = List.length t.recent
    ; accounted_bytes =
        List.sum (module Int) t.recent ~f:(fun entry -> entry.Entry.accounted_bytes)
    }
;;
