open! Core
module P = Agent_protocol
module X = Persistence_codec
module J = P.Json_codec
module D = Document_schema
module Occurrence = Agent_store.Installed_document_originals.Occurrence

let invalid message = Error (P.Error.invalid_request message)
let max_bindings = 256

module Binding = struct
  type t =
    { row : P.History.Id.t
    ; revision : P.History.Content_revision.t
    ; part : int
    ; original : Occurrence.t
    }

  let create ~row ~revision ~part ~original =
    if part < 0 || part >= 4096
    then invalid "installed original part is outside the recovery part range"
    else Ok { row; revision; part; original }
  ;;

  let to_jsonaf t =
    `Object
      [ "row", P.History.Id.to_json t.row
      ; "revision", P.History.Content_revision.to_json t.revision
      ; "part", X.host_counter_to_json t.part
      ; "original", `String (Occurrence.digest t.original)
      ]
  ;;

  let of_jsonaf json =
    let open Result.Let_syntax in
    let%bind fields = J.fields json in
    let%bind row = X.required fields "row" P.History.Id.of_json in
    let%bind revision = X.required fields "revision" P.History.Content_revision.of_json in
    let%bind part = X.required fields "part" X.host_counter_of_json in
    let%bind digest = X.required fields "original" J.string in
    let%bind original =
      Occurrence.of_digest digest
      |> Result.map_error ~f:(fun _ ->
        P.Error.invalid_request "invalid installed original occurrence digest")
    in
    create ~row ~revision ~part ~original
  ;;
end

let validate_bindings bindings =
  let originals = Hash_set.create (module String) in
  let parts = Hashtbl.create (module P.History.Id) in
  List.fold_result bindings ~init:() ~f:(fun () (binding : Binding.t) ->
    let original = Occurrence.digest binding.original in
    if
      Hash_set.length originals >= max_bindings
      || Hash_set.mem originals original
      || List.mem (Hashtbl.find_multi parts binding.row) binding.part ~equal:Int.equal
    then invalid "duplicate or excessive installed original binding"
    else (
      Hash_set.add originals original;
      Hashtbl.add_multi parts ~key:binding.row ~data:binding.part;
      Ok ()))
;;

module Initial = struct
  type t =
    { definition : P.Id.Prompt_definition.t
    ; revision : P.Id.Prompt_revision.t
    ; allocated : (P.History.entry * Binding.t list) list
    }

  let create ~definition ~revision rows =
    let open Result.Let_syntax in
    let%bind allocated =
      List.fold_result rows ~init:[] ~f:(fun acc (entry, parts) ->
        match parts with
        | [] -> Ok acc
        | _ ->
          let row = History_codec.to_protocol entry in
          let%map bindings =
            List.map parts ~f:(fun (part, original) ->
              Binding.create ~row:row.id ~revision:row.content_revision ~part ~original)
            |> Result.all
          in
          (row, bindings) :: acc)
    in
    match allocated with
    | [] -> Ok None
    | _ ->
      let%map () = validate_bindings (List.concat_map allocated ~f:snd) in
      Some { definition; revision; allocated = List.rev allocated }
  ;;

  let matches_source t ~definition ~revision =
    Option.exists definition ~f:(P.Id.Prompt_definition.equal t.definition)
    && P.Id.Prompt_revision.equal t.revision revision
  ;;
end

type t =
  { owner : P.Session_ref.t
  ; generation : int
  ; definition : P.Id.Prompt_definition.t
  ; revision : P.Id.Prompt_revision.t
  ; bindings : Binding.t list
  }

let definition t = t.definition
let revision t = t.revision
let bindings t = t.bindings

let belongs_to t ~owner ~generation =
  P.Session_ref.equal t.owner owner && Int.equal t.generation generation
;;

let validate t ~session_id ~generation =
  if
    Int.equal t.generation generation
    && P.Id.Session.equal (P.Session_ref.session_id t.owner) session_id
  then Ok ()
  else invalid "installed original receipt belongs to a different session generation"
;;

let to_jsonaf t =
  `Object
    [ "owner", P.Session_ref.to_json t.owner
    ; "generation", X.host_counter_to_json t.generation
    ; "definition", P.Id.Prompt_definition.to_json t.definition
    ; "revision", P.Id.Prompt_revision.to_json t.revision
    ; "bindings", `Array (List.map t.bindings ~f:Binding.to_jsonaf)
    ]
;;

let equal a b = D.Json.equal (to_jsonaf a) (to_jsonaf b)

let storage_limits =
  D.Limits.create ~max_bytes:(128 * 1024) ~max_fields:4096 ~max_nodes:4096 ~max_depth:12
  |> Result.ok
  |> Option.value_exn
;;

let validate_encoded = D.Json.validate ~limits:storage_limits

let validate_json json =
  validate_encoded json
  |> Result.map_error ~f:(fun _ ->
    P.Error.invalid_request "installed original receipt exceeds its storage budget")
;;

let create ~owner ~generation ~definition ~revision bindings =
  let open Result.Let_syntax in
  let namespace = P.Session_ref.session_id owner |> P.Id.Session.to_string in
  let%bind () =
    if
      generation >= 0
      && (not (List.is_empty bindings))
      && List.for_all bindings ~f:(fun (binding : Binding.t) ->
        String.equal namespace (History_entry.Id.namespace binding.row))
    then validate_bindings bindings
    else invalid "invalid installed original receipt scope or empty bindings"
  in
  let value = { owner; generation; definition; revision; bindings } in
  let%map () = validate_json (to_jsonaf value) in
  value
;;

let admit (initial : Initial.t) ~owner ~generation ~initial_rows =
  let expected = Hashtbl.create (module P.History.Id) in
  List.iter initial.allocated ~f:(fun (row, bindings) ->
    Hashtbl.set expected ~key:row.id ~data:(row, bindings));
  let bindings =
    List.concat_map initial_rows ~f:(fun row ->
      match Hashtbl.find expected row.P.History.id with
      | Some (allocated, bindings) when P.History.equal_entry allocated row -> bindings
      | _ -> [])
  in
  match bindings with
  | [] -> Ok None
  | _ ->
    create
      ~owner
      ~generation
      ~definition:initial.definition
      ~revision:initial.revision
      bindings
    |> Result.map ~f:Option.some
;;

let of_jsonaf json =
  let open Result.Let_syntax in
  let%bind () = validate_json json in
  let%bind fields = J.fields json in
  let%bind owner = X.required fields "owner" P.Session_ref.of_json in
  let%bind generation = X.required fields "generation" X.host_counter_of_json in
  let%bind definition = X.required fields "definition" P.Id.Prompt_definition.of_json in
  let%bind revision = X.required fields "revision" P.Id.Prompt_revision.of_json in
  let%bind bindings =
    X.required fields "bindings" (function
      | `Array values when Stdlib.List.compare_length_with values max_bindings <= 0 ->
        List.map values ~f:Binding.of_jsonaf |> Result.all
      | _ -> invalid "invalid installed original bindings")
  in
  create ~owner ~generation ~definition ~revision bindings
;;

let shape =
  X.shape_exn
    [ "owner", X.fields_shape [ "server_id"; "session_id" ]
    ; "generation", D.Shape.value
    ; "definition", D.Shape.value
    ; "revision", D.Shape.value
    ; ( "bindings"
      , X.array_shape_exn
          ~identity_field:"original"
          (X.fields_shape [ "row"; "revision"; "part"; "original" ]) )
    ]
;;
