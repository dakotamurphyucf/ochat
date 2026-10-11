open Core
module Profile = Git_host_profile
module Storage = Git_host_storage
module Native = Git_host_native
module Attempt = Git_host_attempt
module Pin = Chatml_project.Git_pin
module Error = Profile.Error
module Grant = Profile.Grant

let storage_error = Profile.Error.of_storage_error
let stored result = Result.map_error result ~f:storage_error
let protected f = Eio.Cancel.protect f

let poll checkpoint =
  Eio.Fiber.check ();
  checkpoint ()
;;

module Limits = struct
  type t =
    { max_cache_repositories : int
    ; max_cache_payload_bytes : int
    ; max_managed_entries : int
    ; max_path_bytes : int
    ; max_work_steps : int
    }

  let create
        ~max_cache_repositories
        ~max_cache_payload_bytes
        ~max_managed_entries
        ~max_path_bytes
        ~max_work_steps
    =
    if
      max_cache_repositories < 0
      || max_cache_payload_bytes < 0
      || max_managed_entries <= 0
      || max_path_bytes <= 0
      || max_work_steps <= 0
    then Or_error.error_string "invalid Git store limits"
    else
      Ok
        { max_cache_repositories
        ; max_cache_payload_bytes
        ; max_managed_entries
        ; max_path_bytes
        ; max_work_steps
        }
  ;;
end

type cache =
  { key : string
  ; payload_bytes : int
  ; entries : int
  }

type store =
  { root : Storage.t
  ; cache_directory : Storage.t
  ; stages_directory : Storage.t
  ; lease : Storage.t
  ; native_root : string
  ; deployment : Profile.Deployment.t
  ; clock : Eio.Time.Mono.ty Eio.Time.Mono.t
  ; limits : Limits.t
  ; gate : Eio.Mutex.t
  ; mutable cache_payload_bytes : int
  ; mutable managed_entries : int
  ; mutable caches : cache String.Map.t
  ; mutable generation : int
  ; mutable inventory_valid : bool
  ; mutable active : bool
  ; mutable closing : bool
  ; drained : unit Eio.Promise.t
  ; resolve_drained : unit Eio.Promise.u
  ; closed : unit Eio.Promise.t
  ; resolve_closed : unit Eio.Promise.u
  }

let close_store t =
  protected (fun () ->
    let first =
      Eio.Mutex.use_rw ~protect:true t.gate (fun () ->
        if t.closing
        then false
        else (
          t.closing <- true;
          if not t.active then Eio.Promise.resolve t.resolve_drained ();
          true))
    in
    if first
    then
      Fun.protect
        ~finally:(fun () -> Eio.Promise.resolve t.resolve_closed ())
        (fun () ->
           Eio.Promise.await t.drained;
           Storage.close t.stages_directory;
           Storage.close t.cache_directory;
           Storage.close t.lease;
           Storage.close t.root)
    else Eio.Promise.await t.closed)
;;

let is_cache_key name =
  String.length name = 64
  && String.for_all name ~f:(fun c ->
    Char.is_digit c || (Char.( >= ) c 'a' && Char.( <= ) c 'f'))
;;

module Store = struct
  type t = store

  let open_root root ~sw ~deployment ~clock ~limits ~checkpoint =
    let open Result.Let_syntax in
    let native_root = Eio.Path.native root in
    match native_root with
    | None -> Error (Error.create Unsupported_provider)
    | Some native_root ->
      if
        limits.Limits.max_work_steps < 32
        || limits.max_managed_entries < 3
        || String.length native_root > limits.Limits.max_path_bytes - 128
        || String.is_empty native_root
        || not (Char.equal native_root.[0] '/')
      then Error (Error.create Invalid_input)
      else (
        let acquired = ref [] in
        let transferred = ref false in
        let acquire result =
          match stored result with
          | Error error -> Error error
          | Ok t ->
            (try
               acquired := t :: !acquired;
               Ok t
             with
             | exn ->
               let backtrace = Stdlib.Printexc.get_raw_backtrace () in
               Storage.close t;
               Exn.raise_with_original_backtrace exn backtrace)
        in
        Fun.protect
          ~finally:(fun () ->
            if not !transferred then List.iter !acquired ~f:Storage.close)
          (fun () ->
             let%bind root = acquire (Storage.borrow_root root ~checkpoint) in
             let%bind lease = acquire (Storage.lease root ~name:"lease") in
             let%bind names = stored (Storage.names root ~max_entries:3 ~checkpoint) in
             if
               not
                 (List.for_all names ~f:(fun name ->
                    List.mem [ "lease"; "cache"; "stages" ] name ~equal:String.equal))
             then Error (Error.create Cache_corrupt)
             else (
               let%bind cache_directory =
                 acquire
                   (Storage.directory root ~name:"cache" ~create:true ~exclusive:false)
               in
               let%bind stages_directory =
                 acquire
                   (Storage.directory root ~name:"stages" ~create:true ~exclusive:false)
               in
               let%bind stages =
                 Result.map_error
                   (Storage.names stages_directory ~max_entries:0 ~checkpoint)
                   ~f:(function
                   | Native.Code.Too_large -> Error.create Cache_corrupt
                   | code -> storage_error code)
               in
               (* Unknown existing stage ownership is never guessed or recursively deleted. *)
               if not (List.is_empty stages)
               then Error (Error.create Cache_corrupt)
               else (
                 let work = ref 32 in
                 let remaining_work = limits.max_work_steps - !work in
                 let%bind () =
                   if remaining_work < 4
                   then Error (Error.create Limit_exceeded)
                   else Ok ()
                 in
                 let maximum_names =
                   Int.min
                     (Int.min
                        limits.max_cache_repositories
                        (limits.max_managed_entries - 3))
                     ((remaining_work - 4) / 2)
                 in
                 let%bind keys =
                   stored
                     (Storage.names
                        cache_directory
                        ~max_entries:maximum_names
                        ~checkpoint)
                 in
                 let key_count = List.length keys in
                 work := !work + (2 * key_count) + 4;
                 (* Three weighted comparison stages per bit of the admitted key
                    count cover balanced map insertion; key comparisons are bounded
                    by the fixed 64-byte hex identity. This is not a CPU proof. *)
                 let map_work = 3 * (Int.ceil_log2 (Int.max 1 key_count) + 1) in
                 let bounded_checkpoint () =
                   (* The fixed admitted inventory bounds the stage count before this
                   scan. Checkpoint carries only cancellation/progress. *)
                   poll checkpoint
                 in
                 let%bind caches, cache_payload_bytes, managed_entries =
                   List.fold_result
                     keys
                     ~init:(String.Map.empty, 0, 3)
                     ~f:(fun (caches, bytes, entries) key ->
                       if not (is_cache_key key)
                       then Error (Error.create Cache_corrupt)
                       else if entries >= limits.max_managed_entries
                       then Error (Error.create Limit_exceeded)
                       else if map_work > limits.max_work_steps - !work
                       then Error (Error.create Limit_exceeded)
                       else (
                         work := !work + map_work;
                         Result.map_error
                           (Storage.with_directory
                              cache_directory
                              ~name:key
                              ~create:false
                              ~exclusive:false
                              ~f:(fun cache ->
                                let%map inventory =
                                  Storage.inventory
                                    cache
                                    ~max_work_steps:(limits.max_work_steps - !work)
                                    ~max_entries:
                                      (Int.max
                                         0
                                         (limits.max_managed_entries - entries - 1))
                                    ~max_depth:3
                                    ~max_payload_bytes:
                                      (limits.max_cache_payload_bytes - bytes)
                                    ~checkpoint:bounded_checkpoint
                                in
                                work := !work + Storage.Inventory.work_steps inventory;
                                let item =
                                  { key
                                  ; payload_bytes =
                                      Storage.Inventory.payload_bytes inventory
                                  ; entries = Storage.Inventory.entries inventory + 1
                                  }
                                in
                                ( Map.set caches ~key ~data:item
                                , bytes + item.payload_bytes
                                , entries + item.entries )))
                           ~f:storage_error))
                 in
                 let drained, resolve_drained = Eio.Promise.create () in
                 let closed, resolve_closed = Eio.Promise.create () in
                 let t =
                   { root
                   ; cache_directory
                   ; stages_directory
                   ; lease
                   ; native_root
                   ; deployment
                   ; clock :> Eio.Time.Mono.ty Eio.Time.Mono.t
                   ; limits
                   ; gate = Eio.Mutex.create ()
                   ; caches
                   ; cache_payload_bytes
                   ; managed_entries
                   ; generation = 0
                   ; inventory_valid = true
                   ; active = false
                   ; closing = false
                   ; drained
                   ; resolve_drained
                   ; closed
                   ; resolve_closed
                   }
                 in
                 let success = Ok t in
                 poll checkpoint;
                 Eio.Switch.on_release sw (fun () -> close_store t);
                 transferred := true;
                 success))))
  ;;
end

module Request = struct
  type store = Store.t

  type t =
    { store : Store.t
    ; grant : Grant.t
    ; pin : Pin.t
    ; limits : Attempt.Limits.t
    ; generation : int
    ; reservation : int
    }

  let payload_bytes_reserved t = t.reservation
  let store t = t.store
  let grant t = t.grant
  let pin t = t.pin
  let limits t = t.limits
  let generation t = t.generation

  let prepare store ~grant ~pin ~limits =
    let open Result.Let_syntax in
    if store.closing
    then Error (Error.create Closed)
    else if not store.inventory_valid
    then Error (Error.create Cache_corrupt)
    else if store.active
    then Error (Error.create Busy)
    else if Grant.input_bytes grant > Attempt.Limits.maximum_request_bytes limits
    then Error (Error.create Limit_exceeded)
    else (
      let%bind reservation = Attempt.acquisition_reservation limits in
      Ok { store; grant; pin; limits; generation = store.generation; reservation })
  ;;
end

type t = store

let open_root = Store.open_root
let root t = t.root
let cache_directory t = t.cache_directory
let stages_directory t = t.stages_directory
let native_root t = t.native_root
let native_workspace t = Filename.concat (Filename.concat t.native_root "stages") "active"
let deployment t = t.deployment
let clock t = t.clock
let generation t = t.generation
let maximum_cache_payload_bytes t = t.limits.max_cache_payload_bytes
let maximum_managed_entries t = t.limits.max_managed_entries
let contains t ~key = Map.mem t.caches key

module Cache = struct
  type t = cache

  let payload_bytes t = t.payload_bytes
end

let cache t ~key = Map.find t.caches key

let with_operation store ~generation ~f =
  let entered = ref false in
  Fun.protect
    ~finally:(fun () ->
      if !entered
      then
        protected (fun () ->
          Eio.Mutex.use_rw ~protect:true store.gate (fun () ->
            store.active <- false;
            if store.closing then Eio.Promise.resolve store.resolve_drained ())))
    (fun () ->
       let admission =
         Eio.Mutex.use_rw ~protect:true store.gate (fun () ->
           if store.closing
           then Error (Error.create Closed)
           else if not store.inventory_valid
           then Error (Error.create Cache_corrupt)
           else if store.active
           then Error (Error.create Busy)
           else if generation <> store.generation
           then Error (Error.create Stale_request)
           else if store.generation = Int.max_value
           then Error (Error.create Limit_exceeded)
           else (
             store.active <- true;
             entered := true;
             Ok ()))
       in
       match admission with
       | Error error -> Error (`Host error)
       | Ok () ->
         (match stored (Storage.validate_root store.root) with
          | Error error -> Error (`Host error)
          | Ok () -> f ()))
;;

let admit_workspace store =
  if 6 > store.limits.max_managed_entries - store.managed_entries
  then Error (Error.create Limit_exceeded)
  else Ok ()
;;

let admit_acquisition store limits =
  let open Result.Let_syntax in
  let%bind _ = Attempt.acquisition_reservation limits in
  let bytes = store.cache_payload_bytes
  and entries = store.managed_entries in
  let maximum_new_bytes =
    (2 * Attempt.Limits.metadata_file_bytes limits)
    + (4 * Attempt.Limits.pack_file_bytes limits)
  in
  if
    Map.length store.caches >= store.limits.max_cache_repositories
    || maximum_new_bytes > store.limits.max_cache_payload_bytes - bytes
    || 64 > store.limits.max_managed_entries - entries
  then Error (Error.create Limit_exceeded)
  else Ok ()
;;

let publish_copy store ~key inventory =
  let bytes = store.cache_payload_bytes
  and entries = store.managed_entries in
  let payload_bytes = Storage.Inventory.payload_bytes inventory in
  let new_entries = Storage.Inventory.entries inventory + 1 in
  if
    (not (is_cache_key key))
    || Map.mem store.caches key
    || payload_bytes > store.limits.max_cache_payload_bytes - bytes
    || new_entries > store.limits.max_managed_entries - entries
  then Error (Error.create Limit_exceeded)
  else (
    let item = { key; payload_bytes; entries = new_entries } in
    let caches = Map.set store.caches ~key ~data:item in
    let success = Ok () in
    let committed = ref false in
    Fun.protect
      ~finally:(fun () -> if not !committed then store.inventory_valid <- false)
      (fun () ->
         protected (fun () ->
           match
             stored
               (Storage.publish
                  store.stages_directory
                  ~name:"copy"
                  ~destination:store.cache_directory
                  ~destination_name:key)
           with
           | Error error -> Error error
           | Ok () ->
             store.caches <- caches;
             store.cache_payload_bytes <- bytes + payload_bytes;
             store.managed_entries <- entries + new_entries;
             store.generation <- store.generation + 1;
             committed := true;
             success)))
;;
