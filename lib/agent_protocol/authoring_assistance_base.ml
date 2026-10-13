open! Core
module C = Authoring_codec
module Context = Authoring_context_core.Context
module Source = Authoring_common.Source
module Digest = Chatml_source.Digest

let ( let* ) result f = Result.bind result ~f
let obj fields = `Object fields
let str value = `String value
let field fields name decode = C.required_as fields name decode

let bounded value =
  if value >= 0 && value <= C.maximum_wire_integer
  then Ok value
  else C.invalid "invalid assistance byte coordinate"
;;

let text ~max value = C.utf8_text ~max_bytes:max (`String value)

let at_most limit values =
  let rec loop count = function
    | [] -> true
    | _ when count = limit -> false
    | _ :: rest -> loop (count + 1) rest
  in
  loop 0 values
;;

module Limits = struct
  let max_result_bytes = 2 * 1024 * 1024
  let max_spans = 8192
  let max_issues = 512
  let max_suggestions = 512
  let max_label_bytes = 1024 * 1024
end
