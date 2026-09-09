open Core
module L = Chatml.Chatml_lang
module V = Chatml.Chatml_value_codec
module R = Chatml_host_runtime
module Id = Agent_protocol.Id.Job

type handlers =
  { start_tool : name:string -> input:Jsonaf.t -> (Id.t, string) result
  ; start_script : Jsonaf.t -> (Id.t, string) result
  ; get : Id.t -> (Jsonaf.t, string) result
  ; cancel : Id.t -> (unit, string) result
  ; rollback_start : Id.t -> unit
  }

type transaction =
  { handlers : handlers
  ; prepare : Id.t list -> (unit -> unit, string) result
  }

let dynamic_handlers current =
  let active f =
    match current () with
    | None -> Error "background job transaction is not installed"
    | Some transaction -> f transaction.handlers
  in
  { start_tool = (fun ~name ~input -> active (fun h -> h.start_tool ~name ~input))
  ; start_script = (fun request -> active (fun h -> h.start_script request))
  ; get = (fun id -> active (fun h -> h.get id))
  ; cancel = (fun id -> active (fun h -> h.cancel id))
  ; rollback_start =
      (fun id ->
        match current () with
        | Some transaction -> transaction.handlers.rollback_start id
        | None -> failwith "recorded job start lost its active transaction")
  }
;;

let id = function
  | L.VString value ->
    Id.of_string value
    |> Result.map_error ~f:(fun error -> error.Agent_protocol.Error.message)
  | _ -> Error "expected job identity"
;;

let start_id (operation : L.eff) =
  match operation.op, operation.args with
  | "Job.start_tool", [ returned; L.VString _; _ ] | "Job.start_script", [ returned; _ ]
    -> id returned |> Result.map ~f:Option.some
  | ("Job.start_tool" | "Job.start_script"), _ -> Error "invalid recorded job start"
  | _ -> Ok None
;;

let split_starts effects =
  let open Result.Let_syntax in
  let seen = Hash_set.create (module Id) in
  let%map ids, ordinary =
    List.fold_result effects ~init:([], []) ~f:(fun (ids, ordinary) operation ->
      let%bind found = start_id operation in
      match found with
      | None -> Ok (ids, operation :: ordinary)
      | Some id when Hash_set.mem seen id -> Error "duplicate recorded job start"
      | Some id ->
        Hash_set.add seen id;
        Ok (id :: ids, ordinary))
  in
  List.rev ids, List.rev ordinary
;;

let install ?control ~handlers (config : R.runtime_config) =
  let open Result.Let_syntax in
  let start name perform : R.op_def =
    { name
    ; kind =
        Local_transactional_with_result
          { rollback =
              (fun args ->
                match start_id L.{ op = name; args } with
                | Ok (Some id) -> handlers.rollback_start id
                | _ -> failwith "host recorded an invalid job reservation")
          }
    ; phase_check = R.allow_all_phases
    ; perform =
        (fun _ args ->
          let%bind job = perform args in
          let value = L.VString (Id.to_string job) in
          let%map _ = id value in
          value)
    }
  in
  let immediate name perform : R.op_def =
    { name
    ; kind = External_sync
    ; phase_check = R.allow_all_phases
    ; perform =
        (fun _ args ->
          match args with
          | [ value ] ->
            let%bind id = id value in
            perform id
          | _ -> Error (name ^ ": expected a job identity"))
    }
  in
  let operations =
    [ start "Job.start_tool" (function
        | [ L.VString name; input ] ->
          let%bind input = V.export_json ?control input in
          handlers.start_tool ~name ~input
        | _ -> Error "Job.start_tool: expected tool name and JSON input")
    ; start "Job.start_script" (function
        | [ request ] ->
          let%bind request = V.export_json ?control request in
          handlers.start_script request
        | _ -> Error "Job.start_script: expected a one-off script request")
    ; immediate "Job.get" (fun id ->
        let%map value = handlers.get id in
        V.import_json ?control value)
    ; immediate "Job.cancel" (fun id ->
        let%map () = handlers.cancel id in
        L.VUnit)
    ]
  in
  { config with
    operations =
      operations
      @ List.filter config.operations ~f:(fun existing ->
        not
          (List.exists operations ~f:(fun operation ->
             String.equal operation.name existing.name)))
  }
;;
