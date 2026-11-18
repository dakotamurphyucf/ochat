open! Core
module F = Document_fields
module P = Agent_protocol

type t =
  | Reserved
  | Applied of int
[@@deriving sexp_of]

let field = "lifecycle_remove_evidence"

(* Reserve the largest encoded generation before mutation, at identical byte,
   field and node cost. Existing records omit this field entirely. *)
let reserved = "-------------------"
let generation_text value = Printf.sprintf "%019d" value

let to_json = function
  | Reserved -> `String reserved
  | Applied value -> `String (generation_text value)
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind encoded = F.string json in
  if String.equal encoded reserved
  then Ok Reserved
  else if String.length encoded <> String.length reserved
  then F.invalid field "invalid reservation length"
  else (
    match Int.of_string_opt encoded with
    | Some value when value >= 0 && String.equal encoded (generation_text value) ->
      Ok (Applied value)
    | Some _ | None -> F.invalid field "invalid generation")
;;

module Evidence = struct
  type t =
    { session_id : P.Id.Session.t
    ; generation : int
    }

  let valid_result ~session_id ~generation (result : P.Method_result.Delete.t) =
    generation >= 0
    && P.Id.Session.equal result.session_id session_id
    && Option.is_none result.archive
  ;;

  let of_delete_json ~session_id ~generation json =
    match P.Method_result.Delete.of_json json with
    | Ok result when valid_result ~session_id ~generation result ->
      Ok { session_id; generation }
    | Ok _ | Error _ ->
      Error (Store_error.Corrupt "private removal result differs from its session")
  ;;
end

module Index = struct
  module Session_key = struct
    module T = struct
      type t = P.Id.Session.t [@@deriving sexp]

      let compare = P.Id.Session.compare
    end

    include T
    include Comparator.Make (T)
  end

  type t = (Session_key.t, Evidence.t, Session_key.comparator_witness) Map.t

  let empty : t = Map.empty (module Session_key)

  let add t (evidence : Evidence.t) =
    Map.update t evidence.session_id ~f:(function
      | Some (retained : Evidence.t) when retained.generation >= evidence.generation ->
        retained
      | Some _ | None -> evidence)
  ;;

  let find t ~session_id = Map.find t session_id
end
