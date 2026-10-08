open! Core

type state =
  | Live
  | Armed
  | Restored_by_oauth
  | Restored_cleanup
[@@deriving equal, sexp_of]

type error =
  | Already_armed
  | Invalid_expiry
  | Expiry_not_future
[@@deriving equal, sexp_of]

type t =
  { real_now : unit -> float
  ; real_sleep_until : float -> unit
  ; mutable override : float option
  ; mutable state : state
  ; mutable oauth_reset_count : int
  }

let create real =
  { real_now = (fun () -> Eio.Time.now real)
  ; real_sleep_until = Eio.Time.sleep_until real
  ; override = None
  ; state = Live
  ; oauth_reset_count = 0
  }
;;

let host_clock t =
  let module Clock = struct
    type t = unit
    type time = float

    let now () = Option.value t.override ~default:(t.real_now ())
    let sleep_until () at = t.real_sleep_until at
  end
  in
  Eio.Resource.T ((), Eio.Time.Pi.clock (module Clock))
;;

let oauth_validation_clock t =
  let module Clock = struct
    type t = unit
    type time = float

    let now () =
      let now = t.real_now () in
      if Option.is_some t.override
      then (
        t.override <- None;
        t.state <- Restored_by_oauth;
        t.oauth_reset_count <- t.oauth_reset_count + 1);
      now
    ;;

    let sleep_until () at = t.real_sleep_until at
  end
  in
  Eio.Resource.T ((), Eio.Time.Pi.clock (module Clock))
;;

let advance_to_expiry t ~expires_at_ms =
  if Option.is_some t.override
  then Error Already_armed
  else if Int64.(expires_at_ms <= 0L || expires_at_ms = max_value)
  then Error Invalid_expiry
  else (
    let expiry = Int64.to_float expires_at_ms /. 1000. in
    let advanced = Int64.to_float Int64.(expires_at_ms + 1L) /. 1000. in
    let now = t.real_now () in
    if
      (not (Float.is_finite expiry && Float.is_finite advanced && Float.is_finite now))
      || Float.(advanced <= expiry)
    then Error Invalid_expiry
    else if Float.(expiry <= now)
    then Error Expiry_not_future
    else (
      t.override <- Some advanced;
      t.state <- Armed;
      Ok ()))
;;

let restore t =
  if Option.is_some t.override
  then (
    t.override <- None;
    t.state <- Restored_cleanup)
;;

let state t = t.state
let oauth_reset_count t = t.oauth_reset_count

let self_check () =
  let real_now = ref 100.
  and slept = ref [] in
  let module Real = struct
    type t = unit
    type time = float

    let now () = !real_now
    let sleep_until () at = slept := at :: !slept
  end
  in
  let real = Eio.Resource.T ((), Eio.Time.Pi.clock (module Real)) in
  let t = create real in
  let host = host_clock t
  and oauth = oauth_validation_clock t in
  assert (Result.is_error (advance_to_expiry t ~expires_at_ms:0L));
  assert (Result.is_error (advance_to_expiry t ~expires_at_ms:99_000L));
  assert (Result.is_ok (advance_to_expiry t ~expires_at_ms:101_000L));
  assert (equal_state (state t) Armed);
  assert (Float.equal (Eio.Time.now host) 101.001);
  assert (Float.equal (Eio.Time.now real) 100.);
  assert (Result.is_error (advance_to_expiry t ~expires_at_ms:102_000L));
  Eio.Time.sleep_until host 105.;
  Eio.Time.sleep_until oauth 106.;
  assert (List.equal Float.equal !slept [ 106.; 105. ]);
  assert (equal_state (state t) Armed);
  real_now := 100.5;
  assert (Float.equal (Eio.Time.now oauth) 100.5);
  assert (Float.equal (Eio.Time.now host) 100.5);
  assert (equal_state (state t) Restored_by_oauth && oauth_reset_count t = 1);
  ignore (Eio.Time.now oauth : float);
  restore t;
  assert (oauth_reset_count t = 1 && equal_state (state t) Restored_by_oauth);
  assert (Result.is_ok (advance_to_expiry t ~expires_at_ms:102_000L));
  let failed =
    try Exn.protect ~f:(fun () -> raise Exit) ~finally:(fun () -> restore t) with
    | Exit -> true
  in
  assert failed;
  restore t;
  assert (equal_state (state t) Restored_cleanup && oauth_reset_count t = 1);
  assert (Float.equal (Eio.Time.now host) !real_now)
;;
