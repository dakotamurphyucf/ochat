open Core
module P = Agent_protocol

module Key = struct
  type t =
    | Schedule of P.Id.Schedule.t
    | Subscription of P.Id.Subscription.t
  [@@deriving compare, equal, hash, sexp]
end

type anchor =
  { started : Mtime.t
  ; delay : Mtime.span
  }

type t = anchor Hashtbl.M(Key).t

let create () = Hashtbl.create (module Key)

let capture t key ~now ~delay_ms =
  Hashtbl.set
    t
    ~key
    ~data:
      { started = now
      ; delay = Mtime.Span.of_uint64_ns Int64.(of_int delay_ms * 1_000_000L)
      }
;;

let reconcile t ~retained ~wall_now ~monotonic_now =
  let keys = Hash_set.create (module Key) in
  List.iter retained ~f:(fun (key, due) ->
    Hash_set.add keys key;
    match Hashtbl.mem t key with
    | true -> ()
    | false ->
      let ns stamp =
        P.Timestamp.to_time_ns stamp |> Time_ns.to_int_ns_since_epoch |> Int64.of_int
      in
      (* Subtract in int64: two valid Time_ns timestamps may differ by more than
         a signed OCaml-int span. The nonnegative result fits an unsigned span. *)
      let remaining = Int64.max 0L Int64.(ns due - ns wall_now) in
      Hashtbl.set
        t
        ~key
        ~data:{ started = monotonic_now; delay = Mtime.Span.of_uint64_ns remaining });
  Hashtbl.filter_keys_inplace t ~f:(Hash_set.mem keys)
;;

let is_due t key ~now =
  match Hashtbl.find t key with
  | Some anchor ->
    Ok
      (Mtime.compare now anchor.started >= 0
       && Mtime.Span.compare (Mtime.span now anchor.started) anchor.delay >= 0)
  | None ->
    Error
      (P.Error.create
         Invalid_state
         ~message:"owned work has no elapsed-time anchor"
         ~retryable:false
         ())
;;
