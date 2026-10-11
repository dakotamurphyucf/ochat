open Core
module Receipt = Agent_store.Prompt_source_receipt
module Local = Chatml_local_source
module Prepare = Chatml_project_prepare

module Limits = struct
  type t =
    { max_path_components : int
    ; max_runtime_leases : int
    }
  [@@deriving equal]

  let create ~max_path_components ~max_runtime_leases =
    if max_path_components <= 0 || max_runtime_leases < 0
    then Or_error.error_string "invalid original source-owner limits"
    else Ok { max_path_components; max_runtime_leases }
  ;;
end

module Error = struct
  type code =
    | Invalid_binding
    | Foreign_receipt
    | Missing_live_root
    | Busy
    | Revoked
    | Closed
    | Lease_limit
    | Filesystem
    | Capture
    | Foreign_capture
    | Project_preparation
    | Document_capture
  [@@deriving equal, sexp_of]

  type t =
    { code : code
    ; filesystem : Local.Error.t option
    ; project_error : (Prepare.Error.t option[@sexp.opaque])
    ; document_error : (Chatmd_content_capture.Error.t option[@sexp.opaque])
    ; capture_diagnostics : Prompt_revision_builder.Diagnostic.t list option
    }
  [@@deriving sexp_of]

  let code t = t.code
  let capture_diagnostics t = t.capture_diagnostics
  let project_error t = t.project_error
  let document_error t = t.document_error

  let create code =
    { code
    ; filesystem = None
    ; project_error = None
    ; document_error = None
    ; capture_diagnostics = None
    }
  ;;

  let filesystem filesystem = { (create Filesystem) with filesystem = Some filesystem }
  let missing_live_root () = create Missing_live_root
  let foreign_receipt () = create Foreign_receipt
  let project error = { (create Project_preparation) with project_error = Some error }
  let document error = { (create Document_capture) with document_error = Some error }

  let capture diagnostics =
    { (create Capture) with capture_diagnostics = Some diagnostics }
  ;;
end

module Policy = struct
  type t =
    { receipt : Receipt.t
    ; limits : Limits.t
    ; mutable revoked : bool
    ; mutable leases : int
    }

  let create receipt ~limits = { receipt; limits; revoked = false; leases = 0 }
  let receipt t = t.receipt
  let accepts_limits t ~limits = Limits.equal t.limits limits
  let revoke t = t.revoked <- true
  let check t = if t.revoked then Error (Error.create Revoked) else Ok ()
end

type t =
  { root : [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
  ; local : Local.t
  ; policy : Policy.t
  ; capture_affinity : unit ref
  ; mutable closed : bool
  ; mutable release_hook : Eio.Switch.hook
  ; release_lease : unit -> unit
  }

let receipt t = Policy.receipt t.policy
let policy t = t.policy
let check t = if t.closed then Error (Error.create Closed) else Policy.check t.policy

let close t =
  Eio.Cancel.protect (fun () ->
    if not t.closed
    then (
      t.closed <- true;
      Eio.Switch.remove_hook t.release_hook;
      t.release_hook <- Eio.Switch.null_hook;
      Fun.protect ~finally:t.release_lease (fun () -> Local.close t.local)))
;;

(* Only this invocation's tag converts expected policy failures. Caller exceptions
   of arbitrary domain type never become a Capture/Filesystem failure. *)
let checked_poll t checkpoint stop =
  let require () =
    match check t with
    | Ok () -> ()
    | Error error -> stop error
  in
  require ();
  Eio.Fiber.check ();
  checkpoint ();
  Eio.Fiber.check ();
  require ()
;;

(* The Eio path is only a parser diagnostic/context label. Local owns the sole
   native duplicate. A closed original Eio root is never reopened through this
   label, and early close does not depend on backend Dir.Close. *)
let acquire root ~sw ~policy ~capture_affinity ~checkpoint ~release_lease ~acquire_local =
  let module Abort = struct
    exception Stop of Error.t
  end
  in
  let acquired = ref None in
  let transferred = ref false in
  let poll () =
    (match Policy.check policy with
     | Ok () -> ()
     | Error error -> raise (Abort.Stop error));
    Eio.Fiber.check ();
    checkpoint ();
    Eio.Fiber.check ();
    match Policy.check policy with
    | Ok () -> ()
    | Error error -> raise (Abort.Stop error)
  in
  Fun.protect
    ~finally:(fun () ->
      if not !transferred
      then
        Eio.Cancel.protect (fun () ->
          match !acquired with
          | Some t -> close t
          | None -> release_lease ()))
    (fun () ->
       try
         poll ();
         match acquire_local poll with
         | Error error -> Error (Error.filesystem error)
         | Ok local ->
           let t =
             { root
             ; local
             ; policy
             ; capture_affinity
             ; closed = false
             ; release_hook = Eio.Switch.null_hook
             ; release_lease
             }
           in
           acquired := Some t;
           t.release_hook <- Eio.Switch.on_release_cancellable sw (fun () -> close t);
           poll ();
           transferred := true;
           Ok t
       with
       | Abort.Stop error -> Error error)
;;

let open_owned root ~sw ~policy ~checkpoint =
  acquire
    root
    ~sw
    ~policy
    ~capture_affinity:(ref ())
    ~checkpoint
    ~release_lease:(fun () -> ())
    ~acquire_local:(fun checkpoint ->
      Local.open_root
        root
        ~sw
        ~max_path_components:policy.Policy.limits.max_path_components
        ~checkpoint)
;;

let lease t ~sw ~checkpoint =
  match check t with
  | Error error -> Error error
  | Ok () ->
    if t.policy.leases >= t.policy.limits.max_runtime_leases
    then Error (Error.create Lease_limit)
    else (
      t.policy.leases <- t.policy.leases + 1;
      let released = ref false in
      let release_lease () =
        if not !released
        then (
          released := true;
          t.policy.leases <- t.policy.leases - 1)
      in
      let module Abort = struct
        exception Stop of Error.t
      end
      in
      try
        acquire
          t.root
          ~sw
          ~policy:t.policy
          ~capture_affinity:t.capture_affinity
          ~release_lease
          ~acquire_local:(fun checkpoint ->
            let directory =
              Local.Directory_path.create [] ~max_bytes:0 ~max_components:0 ~checkpoint
              |> Or_error.ok_exn
            in
            Local.open_subroot t.local ~sw ~directory ~checkpoint)
          ~checkpoint:(fun () ->
            checked_poll t checkpoint (fun error -> raise (Abort.Stop error)))
      with
      | Abort.Stop error -> Error error)
;;

module Capture = struct
  type t =
    { value : Prompt_revision_builder.Capture.t
    ; receipt : Receipt.t
    ; affinity : unit ref
    }

  let value t = t.value
  let receipt t = t.receipt

  module Route = struct
    type t =
      { member : Source.Portable_path.t
      ; original_directory : string
      }

    let member t = t.member

    let local_root t ~locator ~project ~max_locator_bytes ~checkpoint =
      checkpoint ();
      let parent = Filename.dirname (Source.Portable_path.to_string t.member) in
      let length = String.length locator in
      let prefix_length =
        if Filename.is_absolute locator
        then 0
        else String.length t.original_directory + String.length parent + 2
      in
      if
        max_locator_bytes < 0
        || prefix_length > max_locator_bytes
        || length > max_locator_bytes - prefix_length
      then
        Or_error.error_string
          "original declaration project locator exceeds byte allowance"
      else (
        for i = 0 to length - 1 do
          if i mod 256 = 0 then checkpoint ()
        done;
        let locator =
          if Filename.is_absolute locator
          then locator
          else Filename.concat (Filename.concat t.original_directory parent) locator
        in
        checkpoint ();
        Ok (Prepare.Root.Local { locator; project }))
    ;;
  end

  let route t input ~checkpoint =
    let found = ref false in
    List.iter
      (Prompt_revision_builder.Capture.root t.value
       :: List.map (Prompt_revision_builder.Capture.agents t.value) ~f:snd)
      ~f:(fun parsed ->
        checkpoint ();
        List.iter (Prompt.Chat_markdown.Parsed.inputs parsed) ~f:(fun candidate ->
          checkpoint ();
          if phys_equal candidate input then found := true));
    if not !found
    then Or_error.error_string "foreign original-source parser input"
    else
      Result.map
        (Source.Portable_path.create (Chatmd_parser_input.path input))
        ~f:(fun member ->
          ({ member; original_directory = Filename.dirname (Receipt.root_file t.receipt) }
           : Route.t))
  ;;
end

type owner = t

module Scope = struct
  type pending =
    { admitted : unit Eio.Promise.t
    ; mutable joined : unit Eio.Promise.or_exn option
    ; mutable cancel : Eio.Cancel.t option
    ; mutable requested : bool
    }

  type t =
    { owner : owner
    ; sw : Eio.Switch.t
    ; mutable closed : bool
    ; mutable failure : Error.t option
    ; mutable pending : pending option
    }

  let check t =
    if t.closed
    then Error (Error.create Closed)
    else (
      match t.failure with
      | Some error -> Error error
      | None -> check t.owner)
  ;;

  let stop_pending pending =
    pending.requested <- true;
    Option.iter pending.cancel ~f:(fun cancel ->
      Eio.Cancel.cancel cancel (Failure "source scope closed"));
    Eio.Promise.await pending.admitted;
    Option.iter pending.joined ~f:(fun joined ->
      ignore (Eio.Promise.await joined : (unit, exn) Result.t))
  ;;

  let close t =
    let pending = t.pending in
    t.closed <- true;
    Eio.Cancel.protect (fun () ->
      Option.iter pending ~f:stop_pending;
      Option.is_some pending)
  ;;

  let run t f =
    match check t with
    | Error error -> Error error
    | Ok () ->
      (match t.pending with
       | Some _ ->
         let error = Error.create Busy in
         t.failure <- Some error;
         Error error
       | None ->
         Eio.Fiber.check ();
         Eio.Switch.check t.sw;
         let result, resolve_result = Eio.Promise.create () in
         let admitted, resolve_admitted = Eio.Promise.create () in
         let pending = { admitted; joined = None; cancel = None; requested = false } in
         t.pending <- Some pending;
         (try
            let joined =
              Eio.Fiber.fork_promise ~sw:t.sw (fun () ->
                let outcome =
                  try
                    let value =
                      Eio.Cancel.sub (fun cancel ->
                        pending.cancel <- Some cancel;
                        Fun.protect
                          ~finally:(fun () -> pending.cancel <- None)
                          (fun () ->
                             if pending.requested
                             then Eio.Cancel.cancel cancel (Failure "source scope closed");
                             Eio.Fiber.check ();
                             match check t with
                             | Error error -> Error error
                             | Ok () -> f ()))
                    in
                    Ok value
                  with
                  | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
                in
                Eio.Promise.resolve resolve_result outcome)
            in
            pending.joined <- Some joined;
            Eio.Promise.resolve resolve_admitted ()
          with
          | exn ->
            let backtrace = Stdlib.Printexc.get_raw_backtrace () in
            t.pending <- None;
            Eio.Promise.resolve resolve_admitted ();
            Stdlib.Printexc.raise_with_backtrace exn backtrace);
         let outcome =
           try
             (match pending.joined with
              | None -> assert false
              | Some joined -> Eio.Promise.await_exn joined);
             let outcome = Eio.Promise.await result in
             Eio.Fiber.check ();
             outcome
           with
           | exn ->
             let backtrace = Stdlib.Printexc.get_raw_backtrace () in
             Eio.Cancel.protect (fun () -> stop_pending pending);
             Stdlib.Printexc.raise_with_backtrace exn backtrace
         in
         t.pending <- None;
         (match outcome with
          | Error (exn, backtrace) ->
            t.failure <- Some (Error.create Capture);
            Stdlib.Printexc.raise_with_backtrace exn backtrace
          | Ok (Error error) ->
            t.failure <- Some error;
            Error error
          | Ok (Ok value) -> Result.map (check t) ~f:(fun () -> value)))
  ;;

  let capture t ~env ~definition ~preprocess ~limits ~checkpoint =
    run t (fun () ->
      let receipt = receipt t.owner in
      if
        (not
           (Agent_protocol.Id.Prompt_definition.equal
              definition.Prompt_definition.id
              (Receipt.definition_id receipt)))
        || not (String.equal definition.root_file (Receipt.root_file receipt))
      then Error (Error.foreign_receipt ())
      else (
        match Source.Portable_path.create (Filename.basename definition.root_file) with
        | Error _ -> Error (Error.create Invalid_binding)
        | Ok entry ->
          let module Abort = struct
            exception Stop of Error.t
            exception Callback of exn * Stdlib.Printexc.raw_backtrace
          end
          in
          let invoke f =
            try f () with
            | exn ->
              let backtrace = Stdlib.Printexc.get_raw_backtrace () in
              raise (Abort.Callback (exn, backtrace))
          in
          let user_checkpoint () = invoke checkpoint in
          let preprocess contents = invoke (fun () -> preprocess contents) in
          let poll () =
            match check t with
            | Error error -> raise (Abort.Stop error)
            | Ok () ->
              checked_poll t.owner user_checkpoint (fun error -> raise (Abort.Stop error))
          in
          (try
             let loader =
               Source_loader.retained_filesystem
                 ~root:(t.owner.root :> Eio.Fs.dir_ty Eio.Path.t)
                 ~owner:t.owner.local
                 ~checkpoint:poll
             in
             match
               Prompt_revision_builder.capture_with_loader
                 ~loader
                 ~entry
                 ~env
                 ~preprocess
                 ~limits
                 ~checkpoint:poll
                 definition
             with
             | Error diagnostics -> Error (Error.capture diagnostics)
             | Ok value ->
               poll ();
               let capture : Capture.t =
                 { value; receipt; affinity = t.owner.capture_affinity }
               in
               Ok capture
           with
           | Abort.Stop error -> Error error
           | Abort.Callback (exn, backtrace) ->
             Stdlib.Printexc.raise_with_backtrace exn backtrace
           | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
           | exn ->
             let message =
               match exn with
               | Failure message -> String.prefix message 16384
               | _ -> "Original prompt capture failed"
             in
             Error
               (Error.capture
                  [ Prompt_revision_builder.Diagnostic.
                      { code = "prompt.capture_failed"
                      ; message
                      ; source = Some definition.root_file
                      }
                  ]))))
  ;;

  let capture_documents t ~capture ~limits ~checkpoint =
    run t (fun () ->
      if
        (not (phys_equal capture.Capture.affinity t.owner.capture_affinity))
        || not (Receipt.equal capture.receipt (receipt t.owner))
      then Error (Error.create Foreign_capture)
      else
        let module Abort = struct
          exception Stop of Error.t
        end
        in
        let poll () =
          match check t with
          | Error error -> raise (Abort.Stop error)
          | Ok () ->
            checked_poll t.owner checkpoint (fun error -> raise (Abort.Stop error))
        in
        try
          poll ();
          (* Builder admits at most256 authored paths; these immutable contexts
             have already been constructed by its actual bounded parser walker. *)
          let value = capture.value in
          let contexts =
            Prompt_revision_builder.Capture.root value
            :: List.map (Prompt_revision_builder.Capture.agents value) ~f:snd
          in
          match
            Chatmd_content_capture.run
              t.owner.local
              ~parsed_contexts:contexts
              ~limits
              ~checkpoint:poll
          with
          | Error error -> Error (Error.document error)
          | Ok result ->
            poll ();
            Ok result
        with
        | Abort.Stop error -> Error error)
  ;;

  let prepare_sources
        t
        ~capture
        ~requests
        ~additional_anchors
        ~git_store
        ~git_grants
        ~root_limits
        ~max_grants
        ~max_grant_bytes
        ~policy
        ~limits
        ~clock
        ~checkpoint
    =
    run t (fun () ->
      if
        (not (phys_equal capture.Capture.affinity t.owner.capture_affinity))
        || not (Receipt.equal capture.receipt (receipt t.owner))
      then Error (Error.create Foreign_capture)
      else
        let module Abort = struct
          exception Stop of Error.t
        end
        in
        let poll () =
          match check t with
          | Error error -> raise (Abort.Stop error)
          | Ok () ->
            checked_poll t.owner checkpoint (fun error -> raise (Abort.Stop error))
        in
        try
          poll ();
          match
            Chatml_local_roots.Anchor.of_local
              t.owner.local
              ~host_prefix:(Filename.dirname (Receipt.root_file (receipt t.owner)))
              ~limits:root_limits
              ~checkpoint:poll
          with
          | Error _ -> Error (Error.create Project_preparation)
          | Ok anchor ->
            (match
               Prepare.Host.create
                 (anchor :: additional_anchors)
                 ~git_store
                 ~git_grants
                 ~max_grants
                 ~max_grant_bytes
             with
             | Error _ -> Error (Error.create Project_preparation)
             | Ok host ->
               (match
                  Prepare.prepare_sources
                    host
                    requests
                    ~policy
                    ~limits
                    ~clock
                    ~checkpoint:poll
                with
                | Error error -> Error (Error.project error)
                | Ok sources ->
                  poll ();
                  Ok sources))
        with
        | Abort.Stop error -> Error error)
  ;;
end

let with_scope owner ~f =
  match check owner with
  | Error error -> Error error
  | Ok () ->
    Eio.Switch.run (fun sw ->
      let scope = Scope.{ owner; sw; closed = false; failure = None; pending = None } in
      match f scope with
      | outcome ->
        let unfinished = Scope.close scope in
        if unfinished
        then Error (Error.create Busy)
        else (
          match outcome with
          | Error _ as error -> error
          | Ok value ->
            (match scope.failure with
             | Some error -> Error error
             | None -> Result.map (check owner) ~f:(fun () -> value)))
      | exception exn ->
        let backtrace = Stdlib.Printexc.get_raw_backtrace () in
        ignore (Scope.close scope : bool);
        Stdlib.Printexc.raise_with_backtrace exn backtrace)
;;
