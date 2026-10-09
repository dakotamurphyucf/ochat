open! Core
module P = Agent_protocol
module Actor = Operator_authorization
module DTO = P.Provider_operator
module Service = Provider_operator
module B = Inference_host.Backend
module Port = Agent_server.Provider_operator_port

module Opened = struct
  type t =
    { service : Service.t
    ; backend : B.t
    ; incarnation : DTO.Revision.t
    ; setup_receipt :
        actor:Actor.t -> DTO.Setup_request.t -> (DTO.Revision.t, DTO.Error.t) Result.t
    ; close : unit -> unit
    }

  let create ~service ~backend ~incarnation ~setup_receipt ~close =
    { service; backend; incarnation; setup_receipt; close }
  ;;

  let close t = t.close ()
end

type initialization =
  { stop : unit Eio.Promise.t
  ; stop_u : unit Eio.Promise.u
  ; finished : unit Eio.Promise.t
  ; result : ((Opened.t, DTO.Error.t) Result.t, exn) Result.t Eio.Promise.t
  }

type state =
  | Unconfigured
  | Initializing of initialization
  | Opened of Opened.t
  | Closed

type t =
  { sw : Eio.Switch.t
  ; server_id : P.Id.Server.t
  ; authorize_setup : Actor.t -> bool
  ; authorize_status : Actor.t -> bool
  ; setup_receipt :
      actor:Actor.t -> DTO.Setup_request.t -> (P.Command_receipt.t, DTO.Error.t) Result.t
  ; initialize :
      sw:Eio.Switch.t
      -> actor:Actor.t
      -> DTO.Setup_request.t
      -> (Opened.t, DTO.Error.t) Result.t
  ; mutable state : state
  }

let close t =
  let previous = t.state in
  t.state <- Closed;
  match previous with
  | Opened opened -> opened.close ()
  | Initializing initialization ->
    Eio.Cancel.protect (fun () ->
      if not (Eio.Promise.is_resolved initialization.stop)
      then Eio.Promise.resolve initialization.stop_u ();
      Eio.Promise.await initialization.finished)
  | Unconfigured | Closed -> ()
;;

let create
      ~sw
      ~server_id
      ~authorize_setup
      ~authorize_status
      ~setup_receipt
      ~existing
      ~initialize
  =
  let open Result.Let_syntax in
  let%map opened = existing ~sw in
  let t =
    { sw
    ; server_id
    ; authorize_setup
    ; authorize_status
    ; setup_receipt
    ; initialize
    ; state = Option.value_map opened ~default:Unconfigured ~f:(fun x -> Opened x)
    }
  in
  Eio.Switch.on_release sw (fun () -> close t);
  t
;;

let opened t =
  match t.state with
  | Opened opened -> Ok opened
  | Unconfigured -> Error DTO.Error.Store_unavailable
  | Initializing _ -> Error DTO.Error.Busy
  | Closed -> Error DTO.Error.Closed
;;

let rec backend_view t bound =
  let selected () =
    let open Result.Let_syntax in
    let%bind opened =
      opened t
      |> Result.map_error ~f:(fun _ ->
        Inference_runtime.Preparation_error.Target_unavailable)
    in
    match bound with
    | None -> Ok opened.Opened.backend
    | Some max_body_bytes -> B.with_response_limit opened.backend ~max_body_bytes
  in
  B.create
    ~capture_profile:(fun ~current ~profile ->
      let open Result.Let_syntax in
      let%bind backend = selected () in
      B.capture_profile backend ~current ~profile)
    ~capture:(fun ~current ~model ~settings ->
      let open Result.Let_syntax in
      let%bind backend = selected () in
      B.capture backend ~current ~model ~settings)
    ~resolve:(fun target ->
      let open Result.Let_syntax in
      let%bind backend = selected () in
      B.resolve backend target)
    ~with_response_limit:(fun ~max_body_bytes ->
      if max_body_bytes <= 0
      then Error Inference_runtime.Preparation_error.Invalid_preparation
      else (
        let bound =
          Some
            (Option.value_map bound ~default:max_body_bytes ~f:(Int.min max_body_bytes))
        in
        Ok (backend_view t bound)))
;;

let backend t = backend_view t None

let setup t ~actor request =
  if
    not
      (Actor.is_current actor
       && P.Principal.has_scope (Actor.principal actor) Provider_manage
       && t.authorize_setup actor)
  then Error DTO.Error.Denied
  else (
    match t.state with
    | Closed -> Error DTO.Error.Closed
    | Initializing _ -> Error DTO.Error.Busy
    | Opened opened ->
      opened.setup_receipt ~actor request
      |> Result.map ~f:(fun revision ->
        { DTO.Setup_result.server_id = t.server_id; revision })
    | Unconfigured ->
      let stop, stop_u = Eio.Promise.create () in
      let finished, finished_u = Eio.Promise.create () in
      let result, result_u = Eio.Promise.create () in
      let initialization = { stop; stop_u; finished; result } in
      t.state <- Initializing initialization;
      Eio.Fiber.fork ~sw:t.sw (fun () ->
        let outcome =
          try
            Ok
              (Eio.Fiber.first
                 (fun () -> t.initialize ~sw:t.sw ~actor request)
                 (fun () ->
                    Eio.Promise.await stop;
                    Error DTO.Error.Closed))
          with
          | exn -> Error exn
        in
        let outcome =
          match outcome, t.state with
          | Ok (Ok newly_opened), Closed ->
            (try
               newly_opened.close ();
               Ok (Error DTO.Error.Closed)
             with
             | exn -> Error exn)
          | Ok (Ok newly_opened), _ ->
            t.state <- Opened newly_opened;
            Ok (Ok newly_opened)
          | _, Closed -> outcome
          | _, _ ->
            t.state <- Unconfigured;
            outcome
        in
        Eio.Promise.resolve result_u outcome;
        Eio.Promise.resolve finished_u ());
      (match Eio.Promise.await result with
       | Error exn -> Exn.reraise exn "Provider setup failed"
       | Ok (Error error) -> Error error
       | Ok (Ok newly_opened) ->
         Ok
           { DTO.Setup_result.server_id = t.server_id
           ; revision = newly_opened.incarnation
           }))
;;

let dispatch t ~actor command =
  let open Result.Let_syntax in
  match command with
  | P.Command.Provider_setup request ->
    setup t ~actor request
    |> Result.map ~f:(fun result -> P.Method_result.Provider_setup result)
  | Provider_status _
    when match t.state with
         | Unconfigured -> true
         | _ -> false ->
    if
      not
        (Actor.is_current actor
         && P.Principal.has_scope (Actor.principal actor) Provider_view
         && t.authorize_status actor)
    then Error DTO.Error.Denied
    else
      Ok
        (P.Method_result.Provider_status
           { DTO.Status_result.server_id = t.server_id
           ; setup_required = true
           ; profiles = []
           ; flows = []
           ; selection = None
           })
  | _ ->
    let%bind opened = opened t in
    let service = opened.service in
    (match command with
     | Provider_status request ->
       Service.status service ~actor request
       |> Result.map ~f:(fun x -> P.Method_result.Provider_status x)
     | Provider_login_begin request ->
       Service.begin_login service ~actor request
       |> Result.map ~f:(fun x -> P.Method_result.Provider_login_begin x)
     | Provider_login_challenge request ->
       Service.challenge service ~actor ~flow:request.flow
       |> Result.map ~f:(fun x -> P.Method_result.Provider_login_challenge x)
     | Provider_login_cancel request ->
       Service.cancel service ~actor request
       |> Result.map ~f:(fun x -> P.Method_result.Provider_login_cancel x)
     | Provider_logout request ->
       Service.logout service ~actor ~sw:t.sw request
       |> Result.map ~f:(fun x -> P.Method_result.Provider_logout x)
     | Provider_select request ->
       Service.select service ~actor request
       |> Result.map ~f:(fun x -> P.Method_result.Provider_select x)
     | Provider_configure_environment request ->
       Service.configure_environment service ~actor request
       |> Result.map ~f:(fun x -> P.Method_result.Provider_configure_environment x)
     | _ -> Error DTO.Error.Unsupported)
;;

let receipt t ~actor command =
  let open Result.Let_syntax in
  match command with
  | P.Command.Provider_setup request ->
    if
      not
        (Actor.is_current actor
         && P.Principal.has_scope (Actor.principal actor) Provider_manage
         && t.authorize_setup actor)
    then Error DTO.Error.Denied
    else (
      match t.state with
      | Closed -> Error DTO.Error.Closed
      | _ -> t.setup_receipt ~actor request)
  | _ ->
    let%bind opened = opened t in
    Service.command_receipt opened.service ~actor command
;;

let enroll_private_key t ~actor ~profile ~key ~source_reference ~sw ~read =
  let open Result.Let_syntax in
  let%bind opened = opened t in
  Service.enroll_private_key
    opened.service
    ~actor
    ~profile
    ~key
    ~source_reference
    ~sw
    ~read
;;

let operator_port t =
  Port.create ~dispatch:(dispatch t) ~receipt:(receipt t) ~close:(fun () -> close t)
;;
