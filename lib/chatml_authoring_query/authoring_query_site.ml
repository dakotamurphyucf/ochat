open Core
module Captured = Chatml_source
module Wire = Agent_protocol.Authoring_analysis
module Budget = Authoring_query_budget
module Error = Authoring_query_error

module Site_projection = struct
  type t =
    Captured.Location.t -> checkpoint:(unit -> unit) -> (Wire.Site.t, Error.t) Result.t
end

let find_source inventory location ~budget =
  let rec loop = function
    | [] -> Error Error.Source_unavailable
    | source :: tail ->
      Budget.checkpoint budget;
      if
        Budget.equal_source_id
          budget
          (Captured.id source)
          (Captured.Location.source location)
      then
        if
          Captured.Digest.equal
            (Captured.digest source)
            (Captured.Location.source_digest location)
        then Ok source
        else Error Error.Stale_source
      else loop tail
  in
  loop inventory
;;

let project inventory location ~projection ~budget =
  let open Result.Let_syntax in
  let%bind captured = find_source inventory location ~budget in
  Budget.checkpoint budget;
  let%bind projected =
    projection location ~checkpoint:(fun () -> Budget.checkpoint budget)
  in
  let native = Wire.Site.native projected in
  let wire_source = Wire.Byte_range.source native in
  let span = Captured.Location.span location in
  if
    (not
       (Budget.equal_source_id budget (Wire.Source.id wire_source) (Captured.id captured)))
    || (not
          (Captured.Digest.equal
             (Wire.Source.digest wire_source)
             (Captured.digest captured)))
    || (not
          (Int.equal
             (Wire.Source.byte_length wire_source)
             (Captured.byte_length captured)))
    || (not (Int.equal (Wire.Byte_range.start_byte native) span.left.offset))
    || not (Int.equal (Wire.Byte_range.end_byte native) span.right.offset)
  then Error Error.Invalid_site_projection
  else Ok projected
;;
