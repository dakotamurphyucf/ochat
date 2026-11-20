open! Core
module J = Json_codec
module W = Recovery_wire

type delivery =
  | Definitely_not_submitted
  | Possibly_submitted
  | Response_started

type phase =
  | Allocated
  | Running
  | Returned of delivery
  | Interrupted of delivery

type t =
  { ordinal : int64
  ; tracked : bool
  ; phase : phase
  }

let invalid message = Error (Protocol_error.invalid_request message)

let delivery_name = function
  | Definitely_not_submitted -> "definitely_not_submitted"
  | Possibly_submitted -> "possibly_submitted"
  | Response_started -> "response_started"
;;

let delivery_names =
  List.map [ Definitely_not_submitted; Possibly_submitted; Response_started ] ~f:(fun d ->
    delivery_name d, d)
;;

let to_json a =
  let phase, delivery =
    match a.phase with
    | Allocated -> "allocated", None
    | Running -> "running", None
    | Returned d -> "returned", Some d
    | Interrupted d -> "interrupted", Some d
  in
  `Object
    ([ "ordinal", W.revision_to_json a.ordinal
     ; ("tracked", if a.tracked then `True else `False)
     ; "phase", `String phase
     ]
     @ Option.to_list
         (Option.map delivery ~f:(fun d -> "delivery", `String (delivery_name d))))
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind () = W.admit_coordinate json in
  let%bind f = W.fields json [ "ordinal"; "tracked"; "phase"; "delivery" ] in
  let%bind ordinal = J.required_as f "ordinal" W.revision_of_json in
  let%bind tracked = J.required_as f "tracked" J.bool in
  let%bind name = J.required_as f "phase" J.string in
  let%bind delivery =
    J.optional_as f "delivery" (J.enum ~name:"recovery delivery" delivery_names)
  in
  let%map phase =
    match name, delivery with
    | "allocated", None -> Ok Allocated
    | "running", None -> Ok Running
    | "returned", Some d -> Ok (Returned d)
    | "interrupted", Some d -> Ok (Interrupted d)
    | _ -> invalid "invalid recovery attempt phase"
  in
  { ordinal; tracked; phase }
;;
