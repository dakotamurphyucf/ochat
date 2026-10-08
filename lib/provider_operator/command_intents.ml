open! Core
module P = Agent_protocol
module DTO = P.Provider_operator
module M = Credential_registry_model
module S = Private_storage

module Error = struct
  type t =
    | Corrupt
    | Full
    | Conflict
    | Busy
    | Storage of S.Error.t
  [@@deriving sexp_of]
end

module Intent = struct
  type t =
    { principal : P.Id.Principal.t
    ; key : P.Idempotency_key.t
    ; method_name : string
    ; digest : string
    ; operation : M.Id.t
    ; committed : P.Command_receipt.committed option
    }

  let operation t = t.operation
  let committed t = t.committed
end

type admission =
  | Fresh of Intent.t
  | Existing of Intent.t

type t =
  { directory : S.Directory.t
  ; host : M.Id.t
  ; maximum_records : int
  }

let name value =
  match S.Name.create value with
  | Ok name -> name
  | Error _ -> failwith "invalid static command intent filename"
;;

let metadata_name = name "operator-command-intents.json"
let lock_name = name "operator-command-intents.lock"

let storage error =
  if S.Error.equal_code (S.Error.code error) Busy then Error.Busy else Storage error
;;

let create directory ~host ~maximum_records =
  if maximum_records <= 0 || maximum_records > 1024
  then Error Error.Full
  else Ok { directory; host; maximum_records }
;;

let supported = function
  | "provider.setup"
  | "provider.login.begin"
  | "provider.login.cancel"
  | "provider.logout"
  | "provider.select"
  | "provider.configure_environment"
  | "provider.configure_private_key" -> true
  | _ -> false
;;

let allowed_result = function
  | P.Command_receipt.Provider_setup _
  | Provider_login _
  | Provider_cancel _
  | Provider_logout _
  | Provider_selection _
  | Provider_configuration _ -> true
  | _ -> false
;;

let matches_method method_name result =
  match method_name, result with
  | "provider.setup", P.Command_receipt.Provider_setup _
  | "provider.login.begin", Provider_login _
  | "provider.login.cancel", Provider_cancel _
  | "provider.logout", Provider_logout _
  | "provider.select", Provider_selection _
  | ( ("provider.configure_environment" | "provider.configure_private_key")
    , Provider_configuration _ ) -> true
  | _ -> false
;;

let fields expected json =
  match json with
  | `Object fields
    when List.equal
           String.equal
           (List.map fields ~f:fst |> List.sort ~compare:String.compare)
           (List.sort expected ~compare:String.compare) -> Ok fields
  | _ -> Error Error.Corrupt
;;

let intent_json (t : Intent.t) =
  `Object
    [ "principal", P.Id.Principal.to_json t.principal
    ; "key", P.Idempotency_key.to_json t.key
    ; "method", `String t.method_name
    ; "digest", `String t.digest
    ; "operation", `String (M.Id.to_string t.operation)
    ; ( "committed"
      , match t.committed with
        | None -> `Null
        | Some result -> P.Command_receipt.to_json (Committed result) )
    ]
;;

let intent_of_json json =
  let open Result.Let_syntax in
  let%bind fields =
    fields [ "principal"; "key"; "method"; "digest"; "operation"; "committed" ] json
  in
  let get key decode =
    List.Assoc.find_exn fields key ~equal:String.equal
    |> decode
    |> Result.map_error ~f:(fun _ -> Error.Corrupt)
  in
  let string = function
    | `String s -> Ok s
    | _ -> Error Error.Corrupt
  in
  let%bind principal = get "principal" P.Id.Principal.of_json in
  let%bind key = get "key" P.Idempotency_key.of_json in
  let%bind method_name = get "method" string in
  let%bind digest = get "digest" string in
  let%bind () =
    if
      supported method_name
      && String.length digest = 64
      && String.for_all digest ~f:(function
        | '0' .. '9' | 'a' .. 'f' -> true
        | _ -> false)
    then Ok ()
    else Error Error.Corrupt
  in
  let%bind operation =
    get "operation" string
    |> Result.bind ~f:(fun s ->
      M.Id.create s |> Result.map_error ~f:(fun _ -> Error.Corrupt))
  in
  let%bind committed =
    get "committed" (function
      | `Null -> Ok None
      | value ->
        P.Command_receipt.of_json value
        |> Result.map_error ~f:(fun _ -> Error.Corrupt)
        |> Result.bind ~f:(function
          | Committed result when allowed_result result -> Ok (Some result)
          | _ -> Error Error.Corrupt))
  in
  let%map () =
    if Option.for_all committed ~f:(matches_method method_name)
    then Ok ()
    else Error Error.Corrupt
  in
  { Intent.principal; key; method_name; digest; operation; committed }
;;

let identity (a : Intent.t) (b : Intent.t) =
  P.Id.Principal.equal a.principal b.principal
  && P.Idempotency_key.equal a.key b.key
  && String.equal a.method_name b.method_name
;;

let read t =
  match S.Directory.read_bounded t.directory metadata_name ~max_bytes:(512 * 1024) with
  | Error error when S.Error.equal_code (S.Error.code error) Missing -> Ok []
  | Error error -> Error (storage error)
  | Ok bytes ->
    let open Result.Let_syntax in
    let%bind json =
      Result.try_with (fun () -> Jsonaf.of_string (Bytes.to_string bytes))
      |> Result.map_error ~f:(fun _ -> Error.Corrupt)
    in
    let%bind fields = fields [ "version"; "host"; "intents" ] json in
    let%bind values =
      match
        ( List.Assoc.find_exn fields "version" ~equal:String.equal
        , List.Assoc.find_exn fields "host" ~equal:String.equal
        , List.Assoc.find_exn fields "intents" ~equal:String.equal )
      with
      | `Number "1", `String host, `Array values
        when String.equal host (M.Id.to_string t.host) -> Ok values
      | _ -> Error Error.Corrupt
    in
    let%bind () =
      if List.length values <= t.maximum_records then Ok () else Error Error.Full
    in
    let%bind intents = List.map values ~f:intent_of_json |> Result.all in
    let%bind () =
      if
        Option.is_some
          (List.find_a_dup
             (List.map intents ~f:(fun item -> M.Id.to_string item.Intent.operation))
             ~compare:String.compare)
      then Error Error.Corrupt
      else Ok ()
    in
    let%map () =
      if
        List.existsi intents ~f:(fun i item ->
          List.exists (List.take intents i) ~f:(identity item))
      then Error Error.Corrupt
      else Ok ()
    in
    intents
;;

let write t intents =
  let encoded =
    Jsonaf.to_string
      (`Object
          [ "version", `Number "1"
          ; "host", `String (M.Id.to_string t.host)
          ; "intents", `Array (List.map intents ~f:intent_json)
          ])
  in
  if String.length encoded > 512 * 1024
  then Error Error.Full
  else
    S.Directory.replace_metadata t.directory metadata_name (Bytes.of_string encoded)
    |> Result.map_error ~f:storage
;;

let locked t f =
  Eio.Switch.run (fun sw ->
    match S.Lock.acquire t.directory lock_name ~sw ~mode:Exclusive with
    | Error error -> Error (storage error)
    | Ok lease -> Exn.protect ~finally:(fun () -> S.Lock.release lease) ~f)
;;

let candidate ~principal ~key ~method_name ~params ~operation =
  if not (supported method_name)
  then Error Error.Conflict
  else
    P.Json_codec.canonical_string params
    |> Result.map_error ~f:(fun _ -> Error.Corrupt)
    |> Result.map ~f:(fun encoded ->
      { Intent.principal
      ; key
      ; method_name
      ; digest = Digestif.SHA256.(digest_string encoded |> to_hex)
      ; operation
      ; committed = None
      })
;;

let begin_ t ~principal ~key ~method_name ~params ~operation =
  let open Result.Let_syntax in
  let%bind candidate = candidate ~principal ~key ~method_name ~params ~operation in
  locked t (fun () ->
    let%bind intents = read t in
    match List.find intents ~f:(identity candidate) with
    | Some existing ->
      if String.equal existing.digest candidate.digest
      then Ok (Existing existing)
      else Error Error.Conflict
    | None ->
      let%bind () =
        if
          List.exists intents ~f:(fun existing -> M.Id.equal existing.operation operation)
        then Error Error.Conflict
        else if List.length intents >= t.maximum_records
        then Error Error.Full
        else Ok ()
      in
      let%map () = write t (intents @ [ candidate ]) in
      Fresh candidate)
;;

let lookup t ~principal ~key ~method_name ~params =
  (* Operation never participates in lookup identity; validated fixed dummy is
     used only to reuse pure digest construction. No operation is published. *)
  let operation =
    match M.Id.create "lookup" with
    | Ok id -> id
    | Error _ -> assert false
  in
  let open Result.Let_syntax in
  let%bind candidate = candidate ~principal ~key ~method_name ~params ~operation in
  locked t (fun () ->
    let%bind intents = read t in
    match List.find intents ~f:(identity candidate) with
    | None -> Ok None
    | Some existing ->
      if String.equal existing.digest candidate.digest
      then Ok (Some existing)
      else Error Error.Conflict)
;;

let complete t intent result =
  if not (allowed_result result)
  then Error Error.Conflict
  else
    locked t (fun () ->
      let open Result.Let_syntax in
      let%bind intents = read t in
      let%bind existing =
        List.find intents ~f:(identity intent) |> Result.of_option ~error:Error.Conflict
      in
      let%bind () =
        if matches_method existing.method_name result then Ok () else Error Error.Conflict
      in
      let%bind () =
        if
          M.Id.equal existing.operation intent.Intent.operation
          && String.equal existing.digest intent.digest
        then Ok ()
        else Error Error.Conflict
      in
      match existing.committed with
      | Some previous ->
        if
          Jsonaf.exactly_equal
            (P.Command_receipt.to_json (Committed previous))
            (P.Command_receipt.to_json (Committed result))
        then Ok ()
        else Error Error.Conflict
      | None ->
        write
          t
          (List.map intents ~f:(fun item ->
             if identity item intent then { item with committed = Some result } else item)))
;;

let has_operation t ~method_name ~operation =
  locked t (fun () ->
    let open Result.Let_syntax in
    let%map intents = read t in
    List.exists intents ~f:(fun intent ->
      String.equal intent.Intent.method_name method_name
      && M.Id.equal intent.operation operation))
;;
