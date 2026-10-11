open Core
module Accounting = Project_prepare_accounting
module Error = Project_prepare_error
module Limits = Project_prepare_limits
module Policy = Project_prepare_policy

module Reservation = struct
  type t =
    { owner : unit ref
    ; amount : Accounting.t
    ; mutable active : bool
    }
end

type t =
  { witness : unit ref
  ; limits : Limits.t
  ; policy : Policy.t
  ; ceiling : Accounting.t
  ; clock : Eio.Time.Mono.ty Eio.Time.Mono.t
  ; started : Mtime.t
  ; checkpoint : unit -> unit
  ; stop : Error.t -> unit
  ; mutable failure : Error.t option
  ; mutable used : Accounting.t
  ; mutable reserved : Accounting.t
  ; mutable pending : int
  }

let accounting t = t.used

let fail t problem =
  let error = Error.create problem in
  if Option.is_none t.failure then t.failure <- Some error;
  Error error
;;

let seconds_left t =
  Time_ns.Span.to_sec t.limits.wall_time
  -. (Mtime.Span.to_float_ns (Mtime.span t.started (Eio.Time.Mono.now t.clock)) /. 1e9)
;;

let check_deadline t =
  if Float.(seconds_left t <= 0.)
  then (
    let error = Error.create Timed_out in
    t.failure <- Some error;
    t.stop error)
;;

let poison_on_exception t f =
  try f () with
  | exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    if Option.is_none t.failure then t.failure <- Some (Error.create Invalid_accounting);
    Exn.raise_with_original_backtrace exn backtrace
;;

let poll t =
  poison_on_exception t Eio.Fiber.check;
  Option.iter t.failure ~f:t.stop;
  check_deadline t;
  poison_on_exception t t.checkpoint;
  (* Progress callbacks may yield or advance an injected clock. Do not admit
     the next effect using the time observed before that callback. *)
  check_deadline t
;;

let checked_add t first second =
  if not (Accounting.within Accounting.zero ~ceiling:second)
  then fail t Invalid_accounting
  else (
    match Accounting.add first second with
    | Error _ -> fail t Accounting_overflow
    | Ok value -> Ok value)
;;

let remaining t =
  (* Admission below maintains these two checked arithmetic invariants. *)
  let occupied = Accounting.add t.used t.reserved |> Or_error.ok_exn in
  Accounting.subtract t.ceiling occupied |> Or_error.ok_exn
;;

let admit t delta =
  poll t;
  let open Result.Let_syntax in
  let%bind occupied = checked_add t t.used t.reserved in
  let%bind candidate = checked_add t occupied delta in
  if Accounting.within candidate ~ceiling:t.ceiling then Ok () else fail t Limit_exceeded
;;

let charge t delta =
  let open Result.Let_syntax in
  let%bind () = admit t delta in
  let%map candidate = checked_add t t.used delta in
  t.used <- candidate
;;

let reserve t amount =
  let open Result.Let_syntax in
  let%bind () = admit t amount in
  if t.pending = Int.max_value
  then fail t Accounting_overflow
  else (
    let%map reserved = checked_add t t.reserved amount in
    let reservation = { Reservation.owner = t.witness; amount; active = true } in
    t.reserved <- reserved;
    t.pending <- t.pending + 1;
    reservation)
;;

let finish t reservation ~actual =
  poll t;
  if
    (not (phys_equal reservation.Reservation.owner t.witness))
    || (not reservation.active)
    || (not (Accounting.within Accounting.zero ~ceiling:actual))
    || not (Accounting.within actual ~ceiling:reservation.amount)
  then fail t Invalid_accounting
  else
    let open Result.Let_syntax in
    let%map used = checked_add t t.used actual in
    let reserved = Accounting.subtract t.reserved reservation.amount |> Or_error.ok_exn in
    (* Allocate all replacements before consuming the token. No callbacks here. *)
    reservation.active <- false;
    t.used <- used;
    t.reserved <- reserved;
    t.pending <- t.pending - 1
;;

let allowance t constructor =
  poll t;
  match constructor (remaining t) with
  | Ok limits -> Ok limits
  | Error _ -> fail t Limit_exceeded
;;

let roots_limits t =
  let module L = Chatml_local_roots.Limits in
  let policy = t.policy.roots in
  allowance t (fun remaining ->
    L.create
      ~max_locator_bytes:(L.locator_bytes policy)
      ~max_locator_components:(L.locator_components policy)
      ~max_anchors:(L.anchors policy)
      ~max_retained_roots:(L.retained_roots policy)
      ~max_work_steps:(Int.min (L.work_steps policy) remaining.work_steps))
;;

let local_limits t =
  let module L = Chatml_local_capture.Limits in
  let policy = t.policy.local in
  allowance t (fun remaining ->
    L.create
      ~max_directory_entries:
        (Int.min (L.directory_entries policy) remaining.local_directory_entries)
      ~max_sources:(Int.min (L.sources policy) remaining.source_count)
      ~max_source_bytes:(Int.min (L.source_bytes policy) remaining.source_bytes)
      ~max_total_source_bytes:
        (Int.min (L.total_source_bytes policy) remaining.source_bytes)
      ~max_path_components:(L.path_components policy)
      ~max_work_steps:(Int.min (L.work_steps policy) remaining.work_steps))
;;

let git_object_limits t =
  let module L = Chatml_git_catalog.Limits in
  let policy = t.policy.git_objects in
  allowance t (fun remaining ->
    L.create
      ~max_object_bytes:(Int.min (L.max_object_bytes policy) remaining.git_object_bytes)
      ~max_total_object_bytes:
        (Int.min (L.max_total_object_bytes policy) remaining.git_object_bytes)
      ~max_objects:(Int.min (L.max_objects policy) remaining.git_objects)
      ~max_tree_entries:(Int.min (L.max_tree_entries policy) remaining.git_tree_entries)
      ~max_sources:(Int.min (L.max_sources policy) remaining.source_count)
      ~max_source_bytes:(Int.min (L.max_source_bytes policy) remaining.source_bytes)
      ~max_total_source_bytes:
        (Int.min (L.max_total_source_bytes policy) remaining.source_bytes)
      ~max_path_components:(L.max_path_components policy)
      ~max_work_steps:(Int.min (L.max_work_steps policy) remaining.work_steps)
      ~document:(L.document policy))
;;

let git_native_limits t =
  let module L = Chatml_git_host.Limits in
  let policy = t.policy.git_native in
  allowance t (fun remaining ->
    L.create
      ~metadata_file_bytes:(L.metadata_file_bytes policy)
      ~pack_file_bytes:(L.pack_file_bytes policy)
      ~max_acquisition_payload_bytes:
        (Int.min
           (L.acquisition_payload_bytes policy)
           remaining.acquisition_payload_reserved)
      ~max_request_bytes:
        (Int.min (L.maximum_request_bytes policy) remaining.native_request_bytes)
      ~max_output_bytes:
        (Int.min (L.maximum_output_bytes policy) remaining.native_output_bytes)
      ~max_work_steps:(Int.min (L.maximum_work_steps policy) remaining.work_steps)
      ~wall_time:
        (Time_ns.Span.min (L.wall_time policy) (Time_ns.Span.of_sec (seconds_left t)))
      ~cpu_seconds_per_process:(L.cpu_seconds_per_process policy)
      ~memory_bytes_per_process:(L.memory_bytes_per_process policy)
      ~open_files_per_process:(L.open_files_per_process policy))
;;

let composition_limits t =
  let module L = Chatml_project_build.Limits in
  let policy = t.policy.composition in
  allowance t (fun remaining ->
    L.create
      ~max_work_steps:(Int.min (L.max_work_steps policy) remaining.work_steps)
      ~max_input_bytes:
        (Some
           (Option.value_map
              (L.max_input_bytes policy)
              ~default:t.limits.prepared_input_bytes
              ~f:(Int.min t.limits.prepared_input_bytes)))
      ~document:(L.document policy))
;;

let run ~limits ~policy ~clock ~checkpoint ~f =
  let exception Stop of Error.t in
  let ceiling =
    { Accounting.packages = limits.Limits.packages
    ; dependency_visits = limits.Limits.dependency_visits
    ; source_count = limits.Limits.source_count
    ; source_bytes = limits.Limits.source_bytes
    ; local_directory_entries = limits.Limits.local_directory_entries
    ; git_objects = limits.Limits.git_objects
    ; git_object_bytes = limits.Limits.git_object_bytes
    ; git_tree_entries = limits.Limits.git_tree_entries
    ; native_request_bytes = limits.Limits.native_request_bytes
    ; native_output_bytes = limits.Limits.native_output_bytes
    ; acquisition_payload_reserved = limits.Limits.acquisition_payload_reserved
    ; work_steps = limits.Limits.work_steps
    }
  in
  let t =
    { witness = ref ()
    ; limits
    ; policy
    ; ceiling
    ; clock :> Eio.Time.Mono.ty Eio.Time.Mono.t
    ; started = Eio.Time.Mono.now clock
    ; checkpoint
    ; stop = (fun error -> raise (Stop error))
    ; failure = None
    ; used = Accounting.zero
    ; reserved = Accounting.zero
    ; pending = 0
    }
  in
  try
    poll t;
    let open Result.Let_syntax in
    let%bind result = f t in
    poll t;
    if t.pending <> 0 then fail t Invalid_accounting else Ok (result, t.used)
  with
  | Stop error -> Error error
;;
