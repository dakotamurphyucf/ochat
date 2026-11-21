open! Core

type t = string

let key_bytes = 32
let unavailable _ = Store_error.Corrupt "protected approval key is unavailable"

let load_or_create ~env ~root =
  let module Storage = Private_storage in
  let open Result.Let_syntax in
  let store_result result = Result.map_error result ~f:unavailable in
  let anchor = Eio.Path.(Eio.Stdenv.fs env / Data_root.path root) in
  let indexes = Data_root.indexes_path root in
  let%bind component = Storage.Name.create (Filename.basename indexes) |> store_result in
  let%bind filename = Storage.Name.create "protected-approval-key.v1" |> store_result in
  Eio.Switch.run (fun sw ->
    let%bind directory =
      Storage.Directory.open_or_create ~sw ~anchor ~components:[ component ]
      |> store_result
    in
    match Storage.Directory.read_bounded directory filename ~max_bytes:key_bytes with
    | Ok key when Int.equal (Bytes.length key) key_bytes ->
      (* Retry directory durability before using a key left by a previous
         creation whose publication reached rename but did not confirm sync. *)
      let%map () = Durable_file.sync_directory ~env ~path:indexes in
      Bytes.to_string key
    | Error error when Storage.Error.equal_code (Storage.Error.code error) Missing ->
      Eio.Cancel.protect (fun () ->
        let key = Mirage_crypto_rng.generate key_bytes |> Bytes.of_string in
        let%map () =
          Storage.Directory.create_immutable directory filename key |> store_result
        in
        Bytes.to_string key)
    | Ok _ | Error _ -> Error (Store_error.Corrupt "invalid protected approval key file"))
;;

let commit
      key
      ~owner
      ~generation
      ~profile_revision
      ~tool_name
      ~tool_revision
      ~resource_fingerprint
      ~input
  =
  let message =
    `Array
      [ `String "ochat.protected-native-approval.v1"
      ; Agent_protocol.Session_ref.to_json owner
      ; `Number (Int.to_string generation)
      ; `String profile_revision
      ; `String tool_name
      ; `String tool_revision
      ; `String resource_fingerprint
      ; input
      ]
    |> Jsonaf.to_string
  in
  "protected-v1:" ^ Digestif.SHA256.(hmac_string ~key message |> to_hex)
;;
