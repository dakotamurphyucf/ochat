open! Core
module D = Document_schema
module F = Document_fields

type t = Jsonaf.t String.Map.t

module Prepared = struct
  type t =
    { terminal : Jsonaf.t
    ; pending_custody : Jsonaf.t option
    }

  let terminal t = t.terminal
  let pending_custody t = t.pending_custody
end

let capture document =
  match document with
  | None -> Ok String.Map.empty
  | Some document ->
    let open Result.Let_syntax in
    let%bind records =
      F.required (D.Document.payload document) "records" F.array |> F.store
    in
    List.fold_result records ~init:String.Map.empty ~f:(fun captured record ->
      let%bind id = F.required record "record_id" F.digest |> F.store in
      let%bind outcome = F.required record "outcome" Result.return |> F.store in
      if Map.mem captured id
      then Error (Store_error.Corrupt "duplicate raw receipt identity")
      else Ok (Map.set captured ~key:id ~data:outcome))
;;

let raw_outcome captured ~record_id ~authored =
  let open Result.Let_syntax in
  let open Prepared in
  let original = Map.find captured record_id in
  match original with
  | None -> Ok { terminal = authored; pending_custody = None }
  | Some original ->
    let%bind tag = F.required original "tag" F.string |> F.store in
    (match tag with
     | "success" | "failure" -> Ok { terminal = original; pending_custody = None }
     | "pending" ->
       let pending_custody =
         match original with
         | `Object fields
           when List.exists fields ~f:(fun (name, _) -> not (String.equal name "tag")) ->
           Some original
         | `Object _ -> None
         | _ -> Some original
       in
       Ok { terminal = authored; pending_custody }
     | "terminal" ->
       Error (Store_error.Corrupt "terminal reference cannot be republished inline")
     | _ -> Error (Store_error.Corrupt "unknown original receipt outcome"))
;;

let replace document ~references ~limits =
  let open Result.Let_syntax in
  let%bind records =
    F.required (D.Document.payload document) "records" F.array |> F.store
  in
  let records =
    List.map records ~f:(fun record ->
      match record with
      | `Object fields ->
        (match List.Assoc.find fields "record_id" ~equal:String.equal with
         | Some (`String id) ->
           (match Map.find references id with
            | None -> record
            | Some reference ->
              `Object
                (List.map fields ~f:(fun (name, value) ->
                   ( name
                   , if String.equal name "outcome"
                     then Idempotency_outcome.Reference.to_jsonaf reference
                     else value ))))
         | None | Some _ -> record)
      | _ -> record)
  in
  let replace_member fields name value =
    List.map fields ~f:(fun (key, old) ->
      key, if String.equal key name then value else old)
  in
  match D.Document.json document, D.Document.payload document with
  | `Object envelope, `Object payload ->
    let payload = `Object (replace_member payload "records" (`Array records)) in
    D.Document.inspect ~limits (`Object (replace_member envelope "payload" payload))
    |> F.store
  | _ -> Error (Store_error.Corrupt "invalid receipt metadata envelope")
;;
