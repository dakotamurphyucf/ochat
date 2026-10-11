open! Core
module P = Agent_protocol
module S = P.Prompt_dependency_selection
module Store = Agent_store.Idempotency_store
module Receipt = Dependency_update_receipt

let protocol_ok = function
  | Ok value -> value
  | Error error -> failwith (Sexp.to_string_hum (P.Error.sexp_of_t error))
;;

let store_ok = function
  | Ok value -> value
  | Error error -> failwith (Sexp.to_string_hum (Agent_store.Store_error.sexp_of_t error))
;;

let principal = P.Id.Principal.of_string "pri_dependency_operator" |> protocol_ok
let now = P.Timestamp.of_string "2026-10-11T12:00:00Z" |> protocol_ok
let generation value = S.Generation.of_int value |> protocol_ok

let request ?(generation_number = 3) ?(names = [ "prd_beta"; "prd_alpha" ]) key =
  S.Update_request.create
    ~definitions:
      (List.map names ~f:(fun name ->
         P.Id.Prompt_definition.of_string name |> protocol_ok))
    ~expected_generation:(generation generation_number)
    ~idempotency_key:(P.Idempotency_key.of_string key |> protocol_ok)
  |> protocol_ok
;;

let committed request =
  let previous_generation = S.Update_request.expected_generation request in
  S.Update_result.create
    ~previous_generation
    ~generation:(generation (S.Generation.to_int previous_generation + 1))
    ~targets:
      (List.map (S.Update_request.definitions request) ~f:(fun definition_id ->
         S.Update_result.Target.
           { definition_id
           ; outcome =
               Ready (P.Id.Prompt_revision.of_string "prv_actual_revision" |> protocol_ok)
           }))
  |> protocol_ok
;;

let identity request =
  let canonical =
    S.Update_request.to_json request |> P.Json_codec.canonical_string |> protocol_ok
  in
  ( Store.Key.
      { principal_id = principal
      ; session_id = None
      ; method_name = "prompt.dependencies.update"
      ; idempotency_key = S.Update_request.idempotency_key request
      }
  , Digestif.SHA256.digest_string canonical |> Digestif.SHA256.to_hex )
;;

let lookup store request =
  let key, request_digest = identity request in
  Store.lookup store ~key ~request_digest
;;

let assert_pending store request =
  match lookup store request with
  | Store.Replay
      { outcome = Pending
      ; retention = Protected
      ; accepted_transaction_sequence = None
      ; _
      } -> ()
  | _ -> failwith "expected durable protected Pending"
;;

let assert_success store request =
  match lookup store request with
  | Store.Replay { outcome = Success json; retention = Protected; _ } ->
    assert (
      S.Update_result.equal
        (protocol_ok (S.Update_result.of_json json))
        (committed request))
  | _ -> failwith "expected durable protected success"
;;

let assert_error result code =
  match result with
  | Error error ->
    assert (P.Error.equal_code error.P.Error.code code);
    assert (not error.retryable)
  | Ok _ -> failwith "expected rejection"
;;

let dispatch ?(authorize = fun () -> Ok ()) ?(checkpoint = ignore) receipt request execute
  =
  Receipt.dispatch
    receipt
    ~principal_id:principal
    ~request
    ~authorize
    ~checkpoint
    ~execute
;;

let with_directory f =
  Eio_main.run (fun env ->
    let random = Cstruct.create 16 in
    Eio.Flow.read_exact env#secure_random random;
    let basename =
      Cstruct.to_string random
      |> String.to_list
      |> List.map ~f:(fun byte -> sprintf "%02x" (Char.to_int byte))
      |> String.concat
    in
    let directory = Eio.Path.(env#fs / "/tmp" / ("och230-" ^ basename)) in
    Eio.Path.mkdir ~perm:0o700 directory;
    Exn.protect
      ~f:(fun () -> f env directory)
      ~finally:(fun () -> Eio.Cancel.protect (fun () -> Eio.Path.rmtree directory)))
;;

let path directory = Eio.Path.native_exn Eio.Path.(directory / "responses.sexp")

let open_store env directory =
  Store.open_or_create ~env ~path:(path directory) |> store_ok
;;

let receipt store mutex = Receipt.create ~store ~mutex ~now:(fun () -> now)

(* A real provider failure before/after the actual index rename. The control is
   test-owned; no writer injection is added to the production receipt owner. *)
let fault_env env ~armed ~marker =
  let rec wrap
    : 'tags. ([> Eio.Fs.dir_ty ] as 'tags) Eio.Resource.t -> 'tags Eio.Resource.t
    =
    fun (Eio.Resource.T (directory, handler)) ->
    let module Original = (val Eio.Resource.get handler Eio.Fs.Pi.Dir) in
    let module Directory = struct
      include Original

      let open_dir directory ~sw path = wrap (Original.open_dir directory ~sw path)

      let rename directory source destination target =
        match !armed with
        | Some after when String.equal target "responses.sexp" ->
          armed := None;
          if after then Original.rename directory source destination target;
          raise marker
        | Some _ | None -> Original.rename directory source destination target
      ;;
    end
    in
    Eio.Resource.T
      ( directory
      , Eio.Resource.handler
          (H (Eio.Fs.Pi.Dir, (module Directory)) :: Eio.Resource.bindings handler) )
  in
  let directory, prefix = env#fs in
  object
    method fs = wrap directory, prefix
    method cwd = env#cwd
    method stdin = env#stdin
    method stdout = env#stdout
    method stderr = env#stderr
    method net = env#net
    method domain_mgr = env#domain_mgr
    method process_mgr = env#process_mgr
    method clock = env#clock
    method mono_clock = env#mono_clock
    method secure_random = env#secure_random
    method debug = env#debug
    method backend_id = env#backend_id
  end
;;

let io_marker directory =
  match Eio.Path.load Eio.Path.(directory / "missing-marker") with
  | _ -> failwith "missing fixture unexpectedly exists"
  | exception (Eio.Io _ as exn) -> exn
;;
