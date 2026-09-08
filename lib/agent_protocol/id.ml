open Core

module Generator = struct
  type t = { bytes : int -> string }

  let create ~bytes = { bytes }
  let secure = create ~bytes:Mirage_crypto_rng.generate
  let bytes t length = t.bytes length
end

module type S = sig
  type t [@@deriving compare, hash, sexp]

  val create : unit -> t
  val create_with : Generator.t -> t
  val of_string : string -> (t, Protocol_error.t) result
  val to_string : t -> string
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Protocol_error.t) result
end

module Make (Name : sig
    val prefix : string
  end) : S = struct
  type t = string [@@deriving compare, hash, sexp]

  let maximum_length = 96
  let random_byte_count = 18
  let expected_prefix = Name.prefix ^ "_"

  let is_allowed_character = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' -> true
    | _ -> false
  ;;

  let validation_error message = Protocol_error.invalid_request message

  let validate value =
    if String.is_empty value
    then Error (validation_error "identifier must be nonempty")
    else if String.length value > maximum_length
    then Error (validation_error "identifier exceeds the maximum length")
    else if String.length value = String.length expected_prefix
    then Error (validation_error "identifier token must be nonempty")
    else if not (String.is_prefix value ~prefix:expected_prefix)
    then Error (validation_error "identifier has the wrong type prefix")
    else if not (String.for_all value ~f:is_allowed_character)
    then Error (validation_error "identifier contains an invalid character")
    else Ok value
  ;;

  let of_string = validate
  let to_string t = t

  let create_with generator =
    let bytes = Generator.bytes generator random_byte_count in
    if String.length bytes <> random_byte_count
    then invalid_arg "identifier generator returned the wrong byte count";
    let token = Base64.encode_exn ~pad:false ~alphabet:Base64.uri_safe_alphabet bytes in
    expected_prefix ^ token
  ;;

  let create () = create_with Generator.secure
  let to_json t = `String t

  let of_json = function
    | `String value -> of_string value
    | _ -> Error (validation_error "identifier must be a JSON string")
  ;;
end

module Server = Make (struct
    let prefix = "srv"
  end)

module Session = Make (struct
    let prefix = "ses"
  end)

module Attachment = Make (struct
    let prefix = "att"
  end)

module Operation = Make (struct
    let prefix = "op"
  end)

module Event_cursor = Make (struct
    let prefix = "evt"
  end)

module Transaction = Make (struct
    let prefix = "txn"
  end)

module Job = Make (struct
    let prefix = "job"
  end)

module Invocation = Make (struct
    let prefix = "inv"
  end)

module Subscription = Make (struct
    let prefix = "sub"
  end)

module Delivery = Make (struct
    let prefix = "dlv"
  end)

module Capability = Make (struct
    let prefix = "cap"
  end)

module Schedule = Make (struct
    let prefix = "sch"
  end)

module Permission = Make (struct
    let prefix = "per"
  end)

module Grant = Make (struct
    let prefix = "grt"
  end)

module Workspace_definition = Make (struct
    let prefix = "wsd"
  end)

module Workspace_instance = Make (struct
    let prefix = "wsi"
  end)

module Prompt_definition = Make (struct
    let prefix = "prd"
  end)

module Prompt_revision = Make (struct
    let prefix = "prv"
  end)

module Principal = Make (struct
    let prefix = "pri"
  end)

module Blob = Make (struct
    let prefix = "blb"
  end)

module Idempotency_record = Make (struct
    let prefix = "idr"
  end)
