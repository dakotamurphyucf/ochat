open! Core
module P = Agent_protocol
module D = Document_schema

type t =
  { bytes : int
  ; fields : int
  ; nodes : int
  }

let zero = { bytes = 0; fields = 0; nodes = 0 }
let encoded_bytes t = t.bytes
let fields t = t.fields
let nodes t = t.nodes
let exhausted () = Error (P.Error.invalid_request "reserved JSON capacity exhausted")

let create ~bytes ~fields ~nodes =
  if bytes < 0 || fields < 0 || nodes < 0
  then exhausted ()
  else Ok { bytes; fields; nodes }
;;

let add left right =
  if
    left.bytes > Int.max_value - right.bytes
    || left.fields > Int.max_value - right.fields
    || left.nodes > Int.max_value - right.nodes
  then exhausted ()
  else
    create
      ~bytes:(left.bytes + right.bytes)
      ~fields:(left.fields + right.fields)
      ~nodes:(left.nodes + right.nodes)
;;

let positive_difference t ~previous =
  { bytes = Int.max 0 (t.bytes - previous.bytes)
  ; fields = Int.max 0 (t.fields - previous.fields)
  ; nodes = Int.max 0 (t.nodes - previous.nodes)
  }
;;

let maximum left right =
  { bytes = Int.max left.bytes right.bytes
  ; fields = Int.max left.fields right.fields
  ; nodes = Int.max left.nodes right.nodes
  }
;;

let document_error error =
  P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error))
;;

let measure json ~limits =
  let open Result.Let_syntax in
  let%bind bytes =
    D.Json.validate_and_measure ~limits json |> Result.map_error ~f:document_error
  in
  (* The shared validator has already bounded depth, total nodes and fields;
     this second allocation-free traversal cannot exceed that admitted graph. *)
  let rec count = function
    | `Object fields ->
      List.fold
        fields
        ~init:(List.length fields, 1)
        ~f:(fun (fields, nodes) (_, value) ->
          let child_fields, child_nodes = count value in
          fields + child_fields, nodes + child_nodes)
    | `Array values ->
      List.fold values ~init:(0, 1) ~f:(fun (fields, nodes) value ->
        let child_fields, child_nodes = count value in
        fields + child_fields, nodes + child_nodes)
    | `Null | `True | `False | `Number _ | `String _ -> 0, 1
  in
  let fields, nodes = count json in
  create ~bytes ~fields ~nodes
;;

let remaining_limits t limits =
  let bytes = D.Limits.max_bytes limits - t.bytes in
  let fields = D.Limits.max_fields limits - t.fields in
  let nodes = D.Limits.max_nodes limits - t.nodes in
  if bytes <= 0 || fields <= 0 || nodes <= 0
  then exhausted ()
  else
    D.Limits.create
      ~max_bytes:bytes
      ~max_depth:(D.Limits.max_depth limits)
      ~max_fields:fields
      ~max_nodes:nodes
    |> Result.map_error ~f:document_error
;;
