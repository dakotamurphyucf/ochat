open! Core

let check_domain_exn domain =
  if not (Int.equal domain (Stdlib.Domain.self () :> int))
  then invalid_arg "failed startup observation belongs to another domain"
;;

let writer_failure ~progress ~(outcome : Session_commit_observation.Outcome.t) =
  match progress, outcome with
  | Session_commit_observation.Progress.Writer_entered _, Returned (Error failure) ->
    Some (Actor_reply.Returned (Error failure))
  | Writer_entered _, Raised (exn, backtrace) ->
    Some (Actor_reply.Raised (exn, backtrace))
  | (Unobserved | Before_writer | Journal_acknowledged _), _
  | Writer_entered _, (Not_called | In_progress | Returned (Ok ())) -> None
;;

module Input = struct
  type close =
    { outcome : unit Actor_reply.t
    ; normal_root_close : bool
    }

  type root =
    { observation : Session_commit_observation.Owner.t
    ; requested_candidate : bool
    }

  type t =
    { domain : int
    ; mutable producer_exit : unit Actor_reply.t option
    ; mutable close : close option
    ; mutable root : root option
    }

  let create () =
    { domain = (Stdlib.Domain.self () :> int)
    ; producer_exit = None
    ; close = None
    ; root = None
    }
  ;;

  let record_producer_exit t outcome =
    check_domain_exn t.domain;
    match t.producer_exit with
    | None -> t.producer_exit <- Some outcome
    | Some _ -> invalid_arg "held input producer exit is already recorded"
  ;;

  let producer_exit t =
    check_domain_exn t.domain;
    t.producer_exit
  ;;

  let record_close t ~normal_root_close outcome =
    check_domain_exn t.domain;
    match t.close with
    | None -> t.close <- Some { outcome; normal_root_close }
    | Some _ -> ()
  ;;

  let close t =
    check_domain_exn t.domain;
    t.close
  ;;

  let record_root t ~requested_candidate observation =
    check_domain_exn t.domain;
    match t.root with
    | None -> t.root <- Some { observation; requested_candidate }
    | Some _ -> invalid_arg "held root persistence invocation is already recorded"
  ;;

  let root t =
    check_domain_exn t.domain;
    t.root
  ;;
end

module Authored = struct
  type settled_lifecycle =
    | Legacy
    | Extension of Agent_protocol.Id.Moderator_execution.t

  type cut =
    | First_lifecycle
    | Final_aggregate of settled_lifecycle

  type lifecycle =
    | Uninstalled
    | Installed
    | Legacy_acknowledged
    | Callback of
        { id : Agent_protocol.Id.Moderator_execution.t
        ; completion : Callback_cleanup_data.Completion.t
        }

  type 'a write =
    | Unobserved
    | Observed of 'a option

  type t =
    { domain : int
    ; mutable producer_exit : unit Actor_reply.t option
    ; mutable lifecycle : lifecycle
    ; mutable first : unit Actor_reply.t write
    ; mutable final : (settled_lifecycle * unit Actor_reply.t) write
    }

  let create () =
    { domain = (Stdlib.Domain.self () :> int)
    ; producer_exit = None
    ; lifecycle = Uninstalled
    ; first = Unobserved
    ; final = Unobserved
    }
  ;;

  let record_producer_exit t outcome =
    check_domain_exn t.domain;
    match t.producer_exit with
    | None -> t.producer_exit <- Some outcome
    | Some _ -> ()
  ;;

  let producer_exit t =
    check_domain_exn t.domain;
    t.producer_exit
  ;;

  let acknowledge_lifecycle t =
    check_domain_exn t.domain;
    match t.lifecycle with
    | Uninstalled -> t.lifecycle <- Installed
    | Installed | Legacy_acknowledged | Callback _ -> ()
  ;;

  let record_legacy_acknowledgement t =
    check_domain_exn t.domain;
    match t.lifecycle with
    | Installed -> t.lifecycle <- Legacy_acknowledged
    | Uninstalled | Legacy_acknowledged | Callback _ -> ()
  ;;

  let install_callback t ~id completion =
    check_domain_exn t.domain;
    match t.lifecycle with
    | Installed -> t.lifecycle <- Callback { id; completion }
    | Uninstalled | Legacy_acknowledged | Callback _ -> ()
  ;;

  let lifecycle t =
    check_domain_exn t.domain;
    t.lifecycle
  ;;

  let record_write t ~cut ~requested_candidate observation =
    check_domain_exn t.domain;
    let failure () =
      if requested_candidate
      then
        writer_failure
          ~progress:(Session_commit_observation.progress observation)
          ~outcome:(Session_commit_observation.outcome observation)
      else None
    in
    match cut with
    | First_lifecycle ->
      (match t.first with
       | Unobserved -> t.first <- Observed (failure ())
       | Observed _ -> ())
    | Final_aggregate lifecycle ->
      (match t.final with
       | Unobserved ->
         t.final
         <- Observed (Option.map (failure ()) ~f:(fun failure -> lifecycle, failure))
       | Observed _ -> ())
  ;;

  let observed = function
    | Unobserved -> None
    | Observed failure -> failure
  ;;

  let first_lifecycle_failure t =
    check_domain_exn t.domain;
    observed t.first
  ;;

  let final_aggregate_failure t =
    check_domain_exn t.domain;
    observed t.final
  ;;
end

module Initialization = struct
  type t =
    | Unseen
    | One of Callback_cleanup_data.Initialization.t
    | Ambiguous
end

type t =
  { domain : int
  ; mutable initialization : Initialization.t
  ; mutable input : Input.t option
  ; mutable authored : Authored.t option
  ; mutable original_end : unit Actor_reply.t option
  }

let create () =
  { domain = (Stdlib.Domain.self () :> int)
  ; initialization = Initialization.Unseen
  ; input = None
  ; authored = None
  ; original_end = None
  }
;;

let install_input t input =
  check_domain_exn t.domain;
  match t.input with
  | None -> t.input <- Some input
  | Some current when phys_equal current input -> ()
  | Some _ -> invalid_arg "failed startup already owns another held input"
;;

let input t =
  check_domain_exn t.domain;
  t.input
;;

let install_authored t authored =
  check_domain_exn t.domain;
  match t.authored with
  | None -> t.authored <- Some authored
  | Some current when phys_equal current authored -> ()
  | Some _ -> invalid_arg "failed startup already owns another authored attempt"
;;

let authored t =
  check_domain_exn t.domain;
  t.authored
;;

let record_original_end t outcome =
  check_domain_exn t.domain;
  match t.original_end with
  | None -> t.original_end <- Some outcome
  | Some _ -> ()
;;

let original_end t =
  check_domain_exn t.domain;
  t.original_end
;;

let record_initialization t parent =
  check_domain_exn t.domain;
  t.initialization
  <- (match t.initialization with
      | Unseen -> One parent
      | One current when phys_equal current parent -> One current
      | One _ | Ambiguous -> Ambiguous)
;;

let initialization t =
  check_domain_exn t.domain;
  t.initialization
;;
