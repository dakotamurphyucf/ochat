open! Core
module Error = Protocol_error

module Wire = struct
  type t =
    { subscription_id : Id.Subscription.t
    ; epoch : int
    }
  [@@deriving equal, sexp]
end

type t = Wire.t =
  { subscription_id : Id.Subscription.t
  ; epoch : int
  }
[@@deriving equal]

let validate t =
  let open Result.Let_syntax in
  let%bind _ = Id.Subscription.of_string (Id.Subscription.to_string t.subscription_id) in
  if t.epoch >= 0
  then Ok ()
  else Error (Error.invalid_request "delivery subscription epoch must be nonnegative")
;;

let create ~subscription_id ~epoch =
  let t = { subscription_id; epoch } in
  Result.map (validate t) ~f:(fun () -> t)
;;

let to_json t =
  `Object
    [ "subscription_id", Id.Subscription.to_json t.subscription_id
    ; "epoch", `Number (Int.to_string t.epoch)
    ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind () = Json_codec.validate_limits ~max_bytes:512 ~max_depth:2 json in
  let%bind fields = Json_codec.fields json in
  let%bind () = Extension_codec.closed fields [ "subscription_id"; "epoch" ] in
  let%bind subscription_id =
    Json_codec.required_as fields "subscription_id" Id.Subscription.of_json
  in
  let%bind epoch =
    Json_codec.required_as
      fields
      "epoch"
      (Json_codec.bounded_int ~min:0 ~max:Int.max_value)
  in
  create ~subscription_id ~epoch
;;

let sexp_of_t = Wire.sexp_of_t

let t_of_sexp sexp =
  let t = Wire.t_of_sexp sexp in
  match validate t with
  | Ok () -> t
  | Error error -> Sexplib.Conv.of_sexp_error error.message sexp
;;
