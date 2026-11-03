open! Core

module type S = sig
  type t [@@deriving equal, sexp_of]

  val max_bytes : int
  val of_string : string -> (t, Protocol_error.t) result
  val to_string : t -> string
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Protocol_error.t) result
end

module Make (Policy : sig
    val max_bytes : int
    val allow_blank : bool
  end) : S = struct
  type t = string [@@deriving equal, sexp_of]

  let max_bytes = Policy.max_bytes

  let of_string value =
    let invalid message = Error (Protocol_error.invalid_request message) in
    if String.length value > max_bytes
    then invalid "goal text exceeds its UTF-8 byte limit"
    else if (not Policy.allow_blank) && String.is_empty (String.strip value)
    then invalid "goal text must be nonblank"
    else (
      let valid =
        Uutf.String.fold_utf_8
          (fun valid _ -> function
             | `Malformed _ -> false
             | `Uchar scalar ->
               let code = Stdlib.Uchar.to_int scalar in
               valid
               && (code = 9 || code = 10 || (code >= 32 && (code < 127 || code > 159))))
          true
          value
      in
      if valid
      then Ok value
      else invalid "goal text contains malformed UTF-8 or a control character")
  ;;

  let to_string t = t
  let to_json t = `String t
  let of_json json = Result.bind (Json_codec.string json) ~f:of_string
end

module Objective = Make (struct
    let max_bytes = 8 * 1024
    let allow_blank = false
  end)

module Progress = Make (struct
    let max_bytes = 16 * 1024
    let allow_blank = true
  end)

module Reason = Make (struct
    let max_bytes = 4 * 1024
    let allow_blank = false
  end)
