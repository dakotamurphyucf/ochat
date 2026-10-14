open! Core
module P = Agent_protocol
module D = Document_schema
module Store = Agent_store

let ok = function
  | Ok value -> value
  | Error _ -> failwith "developer intent fixture rejected a genuine document"
;;

(** Test-owned observation of the documented private intent layout. A path is
    comparison evidence only; production authority remains in the publisher
    and recovery owners. These helpers never turn a path into a result grant. *)
let path data_root ~(owner : P.Execution_owner.developer) ~job ~attempt =
  assert (attempt > 0);
  let base = Store.Data_root.developer_result_intents_path data_root in
  let run = "run_" ^ P.Id.Developer_run.to_string owner.run_id in
  let epoch = "epoch_" ^ Int.to_string owner.epoch in
  let filename =
    "job_" ^ P.Id.Job.to_string job ^ "-" ^ Int.to_string attempt ^ ".frame"
  in
  Filename.concat (Filename.concat (Filename.concat base run) epoch) filename
;;

let present env path =
  match Eio.Path.kind ~follow:false Eio.Path.(Eio.Stdenv.fs env / path) with
  | `Regular_file -> true
  | `Not_found -> false
  | _ -> failwith "developer intent fixture has an unexpected file kind"
;;

let limits =
  D.Limits.create
    ~max_bytes:(64 * 1024)
    ~max_depth:256
    ~max_fields:1_000_000
    ~max_nodes:2_000_000
  |> ok
;;

let read_document env path =
  if not (present env path) then failwith "developer intent fixture is absent";
  let bytes = Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path) in
  if String.length bytes > 128 * 1024
  then failwith "developer intent fixture exceeds its frame bound";
  Store.Document_record.decode_file ~limits ~expected_digest:None bytes
  |> Result.map ~f:Store.Document_record.document
  |> ok
;;

let write_document env path document =
  let bytes = Store.Document_record.encode document ~limits ~flags:0 |> ok in
  if String.length bytes > 128 * 1024
  then failwith "developer intent fixture exceeds its frame bound";
  Eio.Path.save ~create:(`Or_truncate 0o600) Eio.Path.(Eio.Stdenv.fs env / path) bytes
;;

let field object_ name =
  match object_ with
  | `Object fields ->
    List.Assoc.find fields name ~equal:String.equal
    |> Option.value_or_thunk ~default:(fun () ->
      failwith ("developer intent fixture lacks " ^ name))
  | _ -> failwith "developer intent fixture expected an object"
;;

let string_field object_ name =
  match field object_ name with
  | `String value -> value
  | _ -> failwith ("developer intent fixture expected string " ^ name)
;;

let replace_fields object_ updates =
  match object_ with
  | `Object fields ->
    List.iter updates ~f:(fun (name, _) ->
      if not (List.Assoc.mem fields name ~equal:String.equal)
      then failwith ("developer intent fixture lacks " ^ name));
    `Object
      (List.map fields ~f:(fun (name, value) ->
         ( name
         , Option.value (List.Assoc.find updates name ~equal:String.equal) ~default:value
         )))
  | _ -> failwith "developer intent fixture expected an object"
;;

let replace_payload document payload =
  let json = replace_fields (D.Document.json document) [ "payload", payload ] in
  D.Document.inspect ~limits json |> ok
;;
