open! Core
module P = Agent_protocol
module D = Document_schema
module O = Recurrence_run_origin
module X = Persistence_codec

module Run_id = struct
  include P.Id.Run
  include Comparator.Make (P.Id.Run)
end

module Occurrence = struct
  include Recurrence_identity.Occurrence
  include Comparator.Make (Recurrence_identity.Occurrence)
end

type row =
  { origin : O.t
  ; json : Jsonaf.t
  }

type present =
  { rows : row list
  ; by_id : row Map.M(Run_id).t
  }

type t =
  | Missing
  | Null
  | Present of present

let empty = Missing

let rows = function
  | Missing | Null -> []
  | Present present -> present.rows
;;

let entries t = List.map (rows t) ~f:(fun row -> row.origin)

let find t id =
  match t with
  | Missing | Null -> None
  | Present present -> Map.find present.by_id id |> Option.map ~f:(fun row -> row.origin)
;;

let fields = function
  | Missing -> []
  | Null -> [ "recurrence_origins", `Null ]
  | Present present ->
    [ "recurrence_origins", `Array (List.map present.rows ~f:(fun row -> row.json)) ]
;;

let invalid message = Error (P.Error.invalid_request message)

let check rows =
  let open Result.Let_syntax in
  let%bind () = P.Run_limits.check_count (List.length rows) in
  let%map by_id, _ =
    List.fold_result
      rows
      ~init:(Map.empty (module Run_id), Set.empty (module Occurrence))
      ~f:(fun (by_id, occurrences) row ->
        let id = O.run_id row.origin in
        let occurrence = O.occurrence row.origin in
        if Map.mem by_id id || Set.mem occurrences occurrence
        then invalid "duplicate recurrence Run or occurrence origin"
        else Ok (Map.set by_id ~key:id ~data:row, Set.add occurrences occurrence))
  in
  { rows; by_id }
;;

let add t origin =
  let open Result.Let_syntax in
  match find t (O.run_id origin) with
  | Some retained when O.equal retained origin -> Ok t
  | Some _ -> invalid "Run recurrence origin is immutable"
  | None ->
    let row =
      { origin
      ; json =
          `Object
            [ "run_id", P.Id.Run.to_json (O.run_id origin); "origin", O.to_json origin ]
      }
    in
    let rows = rows t @ [ row ] in
    let%map present = check rows in
    Present present
;;

let of_field = function
  | None -> Ok Missing
  | Some `Null -> Ok Null
  | Some json ->
    let open Result.Let_syntax in
    let%bind () =
      P.Extension_codec.validate_json
        ~max_bytes:P.Run_limits.max_document_bytes
        ~max_depth:P.Run_limits.max_depth
        json
    in
    let%bind rows =
      P.Run_limits.list
        (fun json ->
           let%bind fields = X.object_ json in
           let%bind id = X.required fields "run_id" P.Id.Run.of_json in
           let%bind origin = X.required fields "origin" O.of_json in
           if P.Id.Run.equal id (O.run_id origin)
           then Ok { origin; json }
           else invalid "recurrence origin wrapper differs from its Admission Run")
        json
    in
    let%map present = check rows in
    Present present
;;

let validate_transition ~previous next =
  let open Result.Let_syntax in
  let%bind () = P.Run_limits.check_count (List.length (rows next)) in
  let%bind () =
    match previous, next with
    | Missing, Missing | Null, Null | (Missing | Null | Present _), Present _ -> Ok ()
    | Missing, Null | Null, Missing | Present _, (Missing | Null) ->
      invalid "Run recurrence origin presence regressed"
  in
  let old = rows previous in
  let prefix = List.take (rows next) (List.length old) in
  if List.equal (fun left right -> D.Json.equal left.json right.json) old prefix
  then Ok ()
  else invalid "immutable Run recurrence origin changed or disappeared"
;;
