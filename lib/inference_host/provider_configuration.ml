open! Core
module M = Credential_registry_model
module C = Credential_registry
module B = Credential_bridge

module Mode = struct
  type t =
    | Existing
    | Initialize of M.Id.t
end

module Error = struct
  type t =
    | Invalid_configuration
    | Storage of Private_storage.Error.t
    | Secret_store of Provider_secret_store.Error.t
    | Lifecycle of C.Error.t
    | Bridge of B.Error.t
  [@@deriving sexp_of]
end

type t =
  { anchor : Eio.Fs.dir_ty Eio.Path.t
  ; components : Private_storage.Name.t list
  ; host : M.Id.t
  ; secret_namespace : Provider_secret_store.Namespace.t
  ; mode : Mode.t
  }

let create ~anchor ~components ~host ~secret_namespace ~mode =
  if List.is_empty components || List.length components > 16
  then Error Error.Invalid_configuration
  else Ok { anchor; components; host; secret_namespace; mode }
;;

module Opened = struct
  type t =
    { bridge : B.t
    ; registry : C.t
    }

  let bridge t = t.bridge
  let registry t = t.registry
end

let open_host
      t
      ~sw
      ~env
      ~driver
      ~new_operation
      ~environment
      ~oauth
      ~mappings
      ~authorize
      ~maximum_wait
      ~transport_policy
      ~limits
  =
  let open Result.Let_syntax in
  let%bind metadata_admission =
    C.Metadata_admission.wait ~clock:(Eio.Stdenv.mono_clock env) ~maximum_wait
    |> Result.map_error ~f:(fun _ -> Error.Invalid_configuration)
  in
  let%bind directory =
    Private_storage.Directory.open_or_create ~sw ~anchor:t.anchor ~components:t.components
    |> Result.map_error ~f:(fun error -> Error.Storage error)
  in
  let failed = ref true in
  Exn.protect
    ~finally:(fun () -> if !failed then Private_storage.Directory.close directory)
    ~f:(fun () ->
      let%bind secrets =
        Provider_secret_store.open_private_files
          ~sw
          ~directory
          ~namespace:t.secret_namespace
        |> Result.map_error ~f:(fun error -> Error.Secret_store error)
      in
      Exn.protect
        ~finally:(fun () -> if !failed then Provider_secret_store.close secrets)
        ~f:(fun () ->
          Eio.Switch.on_release sw (fun () -> Provider_secret_store.close secrets);
          let environment = Option.map environment ~f:B.Environment.port in
          let registry =
            match t.mode with
            | Mode.Existing ->
              C.open_existing
                ~metadata_admission
                ~sw
                ~wall_clock:(Eio.Stdenv.clock env)
                ~new_operation
                ~directory
                ~secrets
                ~environment
                ~host:t.host
            | Initialize incarnation ->
              C.initialize_new
                ~metadata_admission
                ~sw
                ~wall_clock:(Eio.Stdenv.clock env)
                ~new_operation
                ~directory
                ~secrets
                ~environment
                ~host:t.host
                ~incarnation
          in
          let%bind registry =
            registry |> Result.map_error ~f:(fun error -> Error.Lifecycle error)
          in
          Exn.protect
            ~finally:(fun () -> if !failed then C.close registry)
            ~f:(fun () ->
              Eio.Switch.on_release sw (fun () -> C.close registry);
              let%map bridge =
                B.create
                  ?oauth
                  driver
                  ~registry
                  ~mappings
                  ~authorize
                  ~clock:(Eio.Stdenv.mono_clock env)
                  ~maximum_wait
                  ~transport_policy
                  ~limits
                |> Result.map_error ~f:(fun error -> Error.Bridge error)
              in
              failed := false;
              { Opened.bridge; registry })))
;;
