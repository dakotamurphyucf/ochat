open! Core
module A = Agent_session
module P = Agent_protocol
module B = A.Runtime_builder
module Actor = A.Session_actor
module Owner = Runtime_owner
module CO = A.Close_observation
module Cell = Session_policy_resource
module Lifetime = Native_resource_lifetime

type 'a result = ('a, P.Error.t) Result.t

type t =
  { domain : int
  ; mutable open_ : bool
  ; cell : Cell.t
  ; actor : Actor.t
  ; handle : Agent_store.Session_store.Handle.t
  ; source : B.Source.t
  ; profile : A.Permission_policy.t
  }

type probe = t

let conflict message = P.Error.create Conflict ~message ~retryable:false ()

let protocol_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : P.Error.t)]
;;

let capture f =
  try Ok (f ()) with
  | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
;;

let resume = function
  | Ok value -> value
  | Error (exn, trace) -> Exn.raise_with_original_backtrace exn trace
;;

(* Preserve the original trusted-host fixture's body-first cleanup policy. *)
let protect_cleanup ~finally f =
  match f () with
  | value ->
    Eio.Cancel.protect finally;
    value
  | exception exn ->
    let trace = Stdlib.Printexc.get_raw_backtrace () in
    (try Eio.Cancel.protect finally with
     | _ -> ());
    Exn.raise_with_original_backtrace exn trace
;;

let check_domain t =
  if Int.equal t.domain (Stdlib.Domain.self () :> int)
  then Ok ()
  else
    Error (P.Error.invalid_request "resource testing probe belongs to another Eio domain")
;;

let check_open t =
  let open Result.Let_syntax in
  let%bind () = check_domain t in
  if t.open_ then Ok () else Error (conflict "resource testing probe has expired")
;;

let with_probe ~actor ~handle ~source ~profile f =
  let t =
    { domain = (Stdlib.Domain.self () :> int)
    ; open_ = true
    ; cell = Cell.create ()
    ; actor
    ; handle
    ; source
    ; profile
    }
  in
  Exn.protect ~f:(fun () -> f t) ~finally:(fun () -> t.open_ <- false)
;;

let pending t =
  check_domain t |> protocol_ok;
  Cell.pending t.cell
;;

let close_pending t =
  let open Result.Let_syntax in
  let%bind () = check_domain t in
  Cell.close_pending t.cell
;;

let close_pending_reported t = Cell.close_pending_reported t.cell

let cleanup_port t =
  check_domain t |> protocol_ok;
  Owner.Policy_resource_cleanup.of_cell t.cell
;;

let key t ~expected =
  Cell.Key.capture
    ~actor:t.actor
    ~handle:t.handle
    ~source:t.source
    ~profile:t.profile
    ~expected
;;

let native_key t ~expected =
  Cell.Native_source_key.capture
    ~actor:t.actor
    ~handle:t.handle
    ~source:t.source
    ~profile:t.profile
    ~expected
;;

module Materialization = struct
  type t =
    { product : B.Product.t
    ; binding : B.Product.Binding.t
    ; physical : B.t
    }
end

module Target = struct
  type t = Cell.Cleanup_target.t

  module Status = struct
    type t =
      | Held of
          { native : B.Native_identity.t option
          ; product : B.Product_identity.t option
          }
      | Released
  end

  let equal = Cell.Cleanup_target.equal
  let capture_consumer t ~owner = Owner.Resource_consumer.capture owner ~target:t

  let status t ~cell =
    let open Result.Let_syntax in
    let%bind () = check_domain cell in
    let%map status = Cell.Cleanup_target.check t ~cell:cell.cell in
    match status with
    | Released -> Status.Released
    | Held facets -> Status.Held { native = facets.native; product = facets.product }
  ;;

  let check_foreign t =
    Result.map (Cell.Cleanup_target.check t ~cell:(Cell.create ())) ~f:(fun _ -> ())
  ;;
end

module Loan = struct
  type t = Owner.Policy_resource_access.loan

  let close = Cell.Lease.close
  let close_reported = Cell.Lease.close_reported
  let target = Cell.Lease.cleanup_target

  let disclose t ~cell f =
    let open Result.Let_syntax in
    let%bind () = check_open cell in
    let%bind status = Target.status (target t) ~cell in
    match status with
    | Released -> Error (conflict "resource testing loan target was released")
    | Held _ -> Ok (f t)
  ;;

  let product t ~cell = disclose t ~cell Cell.Lease.product
  let runtime t ~cell = disclose t ~cell Cell.Lease.runtime
end

module Native_loan = struct
  type t =
    { probe : probe
    ; actual : Cell.Native_lease.t
    }

  let target t = Cell.Native_lease.cleanup_target t.actual

  let close t =
    let open Result.Let_syntax in
    let%bind () = check_domain t.probe in
    Cell.Native_lease.close t.actual
  ;;

  let close_reported t = Cell.Native_lease.close_reported t.actual

  let check_current t ~cell ~expected =
    let open Result.Let_syntax in
    let%bind () = check_open cell in
    let%bind _ = Target.status (target t) ~cell in
    Cell.Native_lease.check_current
      t.actual
      ~actor:cell.actor
      ~handle:cell.handle
      ~profile:cell.profile
      ~expected
  ;;

  let check_retained_source t ~source ~expected =
    let open Result.Let_syntax in
    let%bind () = check_open t.probe in
    Cell.Native_lease.check_retained_source
      t.actual
      ~source
      ~actor:t.probe.actor
      ~handle:t.probe.handle
      ~profile:t.probe.profile
      ~expected
  ;;

  let with_resources t ~f =
    let open Result.Let_syntax in
    let%bind () = check_open t.probe in
    Cell.Native_lease.with_resources t.actual ~f
  ;;

  let with_source_resources t ~f =
    let open Result.Let_syntax in
    let%bind () = check_open t.probe in
    Cell.Native_lease.with_source_resources t.actual ~f
  ;;
end

module Ordinary = struct
  type t =
    { probe : probe
    ; build : Cell.Build.t
    ; initial_expected : A.Session_state.t
    ; mutable prepared :
        (A.Session_state.t * Cell.Key.t * B.Restored_policy_access.t) option
    ; mutable runtime : B.t option
    ; mutable closed : bool
    }

  let capture_product t ~product ~binding =
    let open Result.Let_syntax in
    let%bind () = check_open t.probe in
    Cell.Build.product t.build ~product ~binding
  ;;

  let target t = Cell.Build.cleanup_target t.build

  let prepared t =
    match t.prepared with
    | Some actual -> Ok actual
    | None -> Error (conflict "ordinary resource has not completed materialization")
  ;;

  let expected t =
    Option.value_map t.prepared ~default:t.initial_expected ~f:(fun (expected, _, _) ->
      expected)
  ;;

  let access t =
    let open Result.Let_syntax in
    let%bind () = check_open t.probe in
    let%map _, _, access = prepared t in
    access
  ;;

  let publish t =
    let open Result.Let_syntax in
    let%bind () = check_open t.probe in
    let%bind expected, key, access = prepared t in
    Cell.Build.publish_restored t.build ~key ~access ~expected
  ;;

  let close_original t =
    let open Result.Let_syntax in
    let%bind () = check_domain t.probe in
    if t.closed
    then Ok ()
    else (
      let%bind status = Target.status (target t) ~cell:t.probe in
      match status with
      | Released -> Ok ()
      | Held _ ->
        let%bind _ = Cell.Build.binding t.build in
        (match t.runtime with
         | None -> Error (conflict "ordinary resource has no original physical closer")
         | Some runtime ->
           B.close runtime;
           let%map () = Cell.Build.closed t.build in
           t.closed <- true))
  ;;

  let with_probe probe ~basis ~check ~expected ~build ~f =
    let open Result.Let_syntax in
    let%bind () = check_open probe in
    let initial_key = key probe ~expected |> protocol_ok in
    let selection =
      Cell.select probe.cell ~key:initial_key ~expected_product:None |> protocol_ok
    in
    match selection with
    | Reuse loan ->
      Cell.Lease.close loan |> protocol_ok;
      failwith "634 fresh recovered host unexpectedly reused Product"
    | Build actual ->
      let t =
        { probe
        ; build = actual
        ; initial_expected = expected
        ; prepared = None
        ; runtime = None
        ; closed = false
        }
      in
      let result =
        capture (fun () ->
          let runtime =
            build t ~capture_tracking:(fun graph ->
              Cell.Build.tracking actual graph |> protocol_ok)
            |> protocol_ok
          in
          Cell.Build.physical actual runtime |> protocol_ok;
          t.runtime <- Some runtime;
          check () |> protocol_ok;
          let current = Actor.state probe.actor |> protocol_ok in
          A.Policy_restore_basis.check_construction basis ~state:current |> protocol_ok;
          let current_key = key probe ~expected:current |> protocol_ok in
          let access =
            Cell.Build.restored_policy_access
              actual
              ~source:probe.source
              ~basis
              ~expected:current
            |> protocol_ok
          in
          t.prepared <- Some (current, current_key, access);
          f t)
      in
      (match result with
       | Ok (Ok _) -> ()
       | Ok (Error error) -> Cell.Build.fail actual (Protocol error)
       | Error (exn, trace) -> Cell.Build.fail actual (Raised (exn, trace)));
      protect_cleanup
        ~finally:(fun () -> close_pending probe |> protocol_ok)
        (fun () -> resume result)
  ;;
end

module Native = struct
  type t =
    { probe : probe
    ; expected : A.Session_state.t
    ; basis : A.Policy_restore_basis.t
    ; key : Cell.Key.t
    ; native_key : Cell.Native_source_key.t
    ; build : Cell.Native_build.t
    ; prepared : B.Prepared_native.t
    ; loan : Native_loan.t
    }

  let expected t = t.expected
  let basis t = t.basis

  let prepared t =
    let open Result.Let_syntax in
    let%map () = check_open t.probe in
    t.prepared
  ;;

  let loan t = t.loan
  let target t = Cell.Native_build.cleanup_target t.build

  let with_probe probe ~sw ~expected ~host ~input ~on_physical_release ~f =
    let open Result.Let_syntax in
    let%bind () = check_open probe in
    let basis = A.Policy_restore_basis.of_state expected |> protocol_ok in
    let key = key probe ~expected |> protocol_ok in
    let native_key = native_key probe ~expected |> protocol_ok in
    let selection =
      Cell.select_native probe.cell ~key:native_key ~expected_native:None |> protocol_ok
    in
    match selection with
    | Reuse loan ->
      Cell.Native_lease.close loan |> protocol_ok;
      failwith "644 expected fresh native build"
    | Build build ->
      let loan = ref None in
      let result =
        capture (fun () ->
          let lifetime = Lifetime.create ~sw in
          let scope = Lifetime.scope lifetime in
          Cell.Native_build.scope build scope ~close:(fun () ->
            Lifetime.close_observed lifetime)
          |> protocol_ok;
          Lifetime.started lifetime;
          let prepared =
            Runtime_native_scope.prepare scope ~f:(fun ~retain ->
              retain
                ~close_attempt:(fun () -> Lifetime.close_local lifetime)
                ~prepare:(fun () ->
                  let host =
                    host ~capture_tracking:(fun graph ->
                      Lifetime.capture_tracking lifetime graph;
                      Cell.Native_build.tracking build graph |> protocol_ok)
                  in
                  let child =
                    Lifetime.Child.create ~sw ~capture:(Lifetime.capture_child lifetime)
                    |> protocol_ok
                  in
                  Eio.Switch.on_release (Lifetime.Child.switch child) on_physical_release;
                  let input = input () |> protocol_ok in
                  B.Prepared_native.prepare
                    input
                    ~construction:(Lifetime.Child.construction child)
                    ~host))
            |> protocol_ok
          in
          Cell.Native_build.prepared build prepared ~source:probe.source |> protocol_ok;
          let actual = Cell.Native_build.publish build ~key:native_key |> protocol_ok in
          let actual = { Native_loan.probe; actual } in
          loan := Some actual;
          f { probe; expected; basis; key; native_key; build; prepared; loan = actual })
      in
      (match result with
       | Ok (Ok _) -> ()
       | Ok (Error error) -> Cell.Native_build.fail build (Protocol error)
       | Error (exn, trace) -> Cell.Native_build.fail build (Raised (exn, trace)));
      protect_cleanup
        ~finally:(fun () ->
          Option.iter !loan ~f:(fun loan -> Native_loan.close loan |> protocol_ok);
          close_pending probe |> protocol_ok)
        (fun () -> resume result)
  ;;
end

module Attachment = struct
  type t =
    { native : Native.t
    ; promotion : Cell.Promotion.t
    ; mutable closed : bool
    }

  let target t = Cell.Promotion.cleanup_target t.promotion

  let prepared t =
    let open Result.Let_syntax in
    let%map () = check_open t.native.probe in
    Cell.Promotion.prepared t.promotion
  ;;

  let binding t =
    let open Result.Let_syntax in
    let%bind () = check_open t.native.probe in
    Cell.Promotion.binding t.promotion
  ;;

  let publish t ~basis ~expected =
    let open Result.Let_syntax in
    let%bind () = check_open t.native.probe in
    let%bind access =
      Cell.Promotion.restored_policy_access
        t.promotion
        ~source:t.native.probe.source
        ~basis
        ~expected
    in
    let%map loan =
      Cell.Promotion.publish_restored t.promotion ~key:t.native.key ~access ~expected
    in
    loan, access
  ;;

  let close t =
    let open Result.Let_syntax in
    let%bind () = check_domain t.native.probe in
    if t.closed
    then Ok ()
    else (
      let%bind status = Target.status (target t) ~cell:t.native.probe in
      match status with
      | Released -> Ok ()
      | Held _ ->
        let%map () = Cell.Promotion.closed t.promotion in
        t.closed <- true)
  ;;

  let with_probe native ~build ~f =
    let open Result.Let_syntax in
    let probe = native.Native.probe in
    let%bind () = check_open probe in
    let promotion =
      Cell.begin_native_promotion
        probe.cell
        ~loan:native.loan.actual
        ~native_key:native.native_key
        ~key:native.key
      |> protocol_ok
    in
    let t = { native; promotion; closed = false } in
    let result =
      capture (fun () ->
        let prepared = Cell.Promotion.prepared promotion in
        let attachment = B.Product_attachment.create prepared |> protocol_ok in
        Cell.Promotion.attachment promotion attachment |> protocol_ok;
        let actual = build prepared ~attachment |> protocol_ok in
        Cell.Promotion.product
          promotion
          ~product:actual.Materialization.product
          ~binding:actual.binding
        |> protocol_ok;
        Cell.Promotion.physical promotion actual.physical |> protocol_ok;
        f t actual.product)
    in
    match result with
    | Ok (Ok value) -> Ok value
    | Ok (Error error) ->
      Cell.Promotion.fail promotion (Protocol error);
      Error error
    | Error (exn, trace) ->
      Cell.Promotion.fail promotion (Raised (exn, trace));
      Exn.raise_with_original_backtrace exn trace
  ;;
end

module Empty_partial = struct
  type t =
    { probe : probe
    ; build : Cell.Build.t
    ; mutable in_callback : bool
    }

  let target t = Cell.Build.cleanup_target t.build

  let try_unallocated_acknowledgement t =
    let open Result.Let_syntax in
    let%bind () = check_open t.probe in
    if t.in_callback
    then Cell.Build.closed t.build
    else Error (conflict "empty partial callback has expired")
  ;;

  let with_ordinary probe ~expected ~failure ~during ~after_failure =
    let open Result.Let_syntax in
    let%bind () = check_open probe in
    let key = key probe ~expected |> protocol_ok in
    let selected = Cell.select probe.cell ~key ~expected_product:None |> protocol_ok in
    match selected with
    | Reuse loan ->
      Cell.Lease.close loan |> protocol_ok;
      Error (conflict "empty partial unexpectedly reused Product")
    | Build build ->
      let t = { probe; build; in_callback = true } in
      let outcome =
        capture (fun () ->
          Exn.protect ~f:(fun () -> during t) ~finally:(fun () -> t.in_callback <- false))
      in
      (match outcome with
       | Ok () ->
         Cell.Build.fail build (Protocol failure);
         after_failure (target t);
         Ok ()
       | Error (exn, trace) ->
         Cell.Build.fail build (Raised (exn, trace));
         Exn.raise_with_original_backtrace exn trace)
  ;;

  let with_native probe ~expected ~failure ~during ~after_failure =
    let open Result.Let_syntax in
    let%bind () = check_open probe in
    let key = native_key probe ~expected |> protocol_ok in
    let selected =
      Cell.select_native probe.cell ~key ~expected_native:None |> protocol_ok
    in
    match selected with
    | Reuse loan ->
      Cell.Native_lease.close loan |> protocol_ok;
      Error (conflict "empty partial unexpectedly reused native unit")
    | Build build ->
      let target = Cell.Native_build.cleanup_target build in
      (match capture (fun () -> during target) with
       | Ok () ->
         Cell.Native_build.fail build (Protocol failure);
         after_failure target;
         Ok ()
       | Error (exn, trace) ->
         Cell.Native_build.fail build (Raised (exn, trace));
         Exn.raise_with_original_backtrace exn trace)
  ;;
end

module Parent_cleanup = struct
  type t =
    { lifetime : Lifetime.t
    ; parent : Lifetime.Parent.t
    }

  let create ~sw =
    let lifetime = Lifetime.create ~sw in
    { lifetime; parent = Lifetime.parent lifetime }
  ;;

  let capture_release t actual = Lifetime.Parent.capture_release t.parent actual
  let record t ~owner actual = Lifetime.Parent.record t.parent ~owner actual
  let close t = Lifetime.close_observed t.lifetime
end

let pin_native t ~expected_native ~expected =
  let open Result.Let_syntax in
  let%bind () = check_open t in
  let%map actual =
    Cell.pin_native
      t.cell
      ~expected_native
      ~actor:t.actor
      ~handle:t.handle
      ~profile:t.profile
      ~expected
  in
  { Native_loan.probe = t; actual }
;;

let native_access t ~prepare =
  Owner.Native_resource_access.create
    ~prepare:(fun ~admission ~stage ->
      let open Result.Let_syntax in
      let%bind () = check_open t in
      prepare ~admission ~stage)
    ~pin:(fun ~admission ~expected_native ->
      let open Result.Let_syntax in
      let%bind () = check_open t in
      let%bind expected = Actor.state t.actor in
      let%bind () = Owner.Native_resource_admission.check admission ~expected in
      Cell.pin_native
        t.cell
        ~expected_native
        ~actor:t.actor
        ~handle:t.handle
        ~profile:t.profile
        ~expected)
    ~check:(fun actual ~admission ~expected ->
      let open Result.Let_syntax in
      let%bind () = check_open t in
      let%bind () = Owner.Native_resource_admission.check admission ~expected in
      Cell.Native_lease.check_current
        actual
        ~actor:t.actor
        ~handle:t.handle
        ~profile:t.profile
        ~expected)
    ~check_custody:(fun actual query ->
      let open Result.Let_syntax in
      let%bind () = check_open t in
      let%bind expected = Actor.state t.actor in
      match query with
      | Resources ->
        Cell.Native_lease.check_current
          actual
          ~actor:t.actor
          ~handle:t.handle
          ~profile:t.profile
          ~expected
      | Retained_source source ->
        Native_loan.check_retained_source { probe = t; actual } ~source ~expected)
;;

let policy_access t ~restore =
  Owner.Policy_resource_access.create
    ~pin:(fun ~expected_product ->
      let open Result.Let_syntax in
      let%bind () = check_open t in
      let%bind selected =
        Cell.pin_retained t.cell ~expected_product:(Some expected_product)
      in
      match selected with
      | Retained loan -> Ok loan
      | No_retained_product ->
        Error
          (P.Error.create
             Invalid_state
             ~message:"634 recovered host unexpectedly used missing retained Product"
             ~retryable:false
             ()))
    ~prepare_access:(fun loan ~expected ->
      let open Result.Let_syntax in
      let%bind () = check_open t in
      Cell.Lease.policy_access
        loan
        ~actor:t.actor
        ~handle:t.handle
        ~profile:t.profile
        ~expected)
  |> Owner.Policy_resource_access.with_restoration
       ~restore:(fun ~stage ~request ~governance ~authorize ->
         let open Result.Let_syntax in
         let%bind () = check_open t in
         restore ~stage ~request ~governance ~authorize)
       ~check_restored:(fun loan ~access ~expected ->
         let open Result.Let_syntax in
         let%bind () = check_open t in
         Cell.Lease.check_restored_policy_access
           loan
           ~access
           ~actor:t.actor
           ~handle:t.handle
           ~profile:t.profile
           ~expected)
;;
