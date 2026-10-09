open! Core
module P = Agent_protocol

let fail message = raise_s [%sexp "run read conformance", (message : string)]

let checked = function
  | Ok value -> value
  | Error error -> raise_s [%sexp "run read protocol error", (error : P.Error.t)]
;;

let non_history response =
  match checked response with
  | P.Public.Result.Non_history value -> P.Public.Result.Non_history.value value
  | _ -> fail "expected non-history run result"
;;

let check
      receipt
      ~session_id
      ~(request : P.Command.t -> (P.Public.Result.t, P.Error.t) result)
  =
  let host =
    match request Server_info |> non_history with
    | Server_info info -> info.server_id
    | _ -> fail "expected advertised host"
  in
  let session = P.Session_ref.create ~server_id:host ~session_id in
  let lookup =
    P.Run_query.Lookup_request.{ session; run_id = receipt.P.Run_receipt.run_id }
  in
  let view =
    match request (Session_run lookup) |> non_history with
    | Session_run (Available view) -> view
    | Session_run (Unavailable _) -> fail "committed run unavailable"
    | _ -> fail "expected retained run lookup"
  in
  if
    not
      (P.Id.Run.equal view.run.id receipt.run_id
       && P.Session_ref.equal view.run.session session
       && P.Run_source.equal view.run.source receipt.source
       && Option.exists view.admission_receipt ~f:(P.Run_receipt.equal receipt))
  then fail "run lookup lost original admission identity";
  let page_request =
    P.Run_query.Request.create
      ~session
      ~page:(P.Page.Request.create ~limit:8 () |> checked)
    |> checked
  in
  let page =
    match request (Session_runs page_request) |> non_history with
    | Session_runs page -> page
    | _ -> fail "expected retained run page"
  in
  if
    not
      (Int.equal
         (List.count page.items ~f:(fun row ->
            P.Id.Run.equal row.P.Run_query.View.run.id receipt.run_id))
         1)
  then fail "run page omitted or duplicated committed run";
  let missing = P.Id.Run.create () in
  (match request (Session_run { lookup with run_id = missing }) |> non_history with
   | Session_run (Unavailable id) when P.Id.Run.equal missing id -> ()
   | _ -> fail "missing run did not preserve unavailable identity");
  let foreign = P.Session_ref.create ~server_id:(P.Id.Server.create ()) ~session_id in
  match request (Session_run { lookup with session = foreign }) with
  | Error { code = Invalid_request; _ } -> ()
  | Error error -> raise_s [%sexp "wrong host rejection", (error : P.Error.t)]
  | Ok _ -> fail "foreign host run lookup succeeded"
;;
