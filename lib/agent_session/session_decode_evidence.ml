open! Core

module Field = struct
  type 'a t =
    { decode : Jsonaf.t -> ('a, Agent_protocol.Error.t) Result.t
    ; json : Jsonaf.t
    ; value : 'a
    }

  let admit ~decode json =
    Result.map (decode json) ~f:(fun value -> { decode; json; value })
  ;;

  let readmit t ~json =
    if Jsonaf.exactly_equal t.json json then Ok t else admit ~decode:t.decode json
  ;;

  let value t = t.value
  let reuse t ~json = if Jsonaf.exactly_equal t.json json then Some t else None
end

module Sequence = struct
  type 'a t =
    { decode : Jsonaf.t -> ('a, Agent_protocol.Error.t) Result.t
    ; entries : 'a Field.t list
    ; identity_field : string option
    ; by_identity : 'a Field.t String.Map.t
    }

  let identity json ~field =
    match Document_schema.Json.field json ~name:field with
    | Value (`String identity) -> Some identity
    | Absent | Null | Value _ -> None
  ;;

  let index entries ~identity_field =
    match identity_field with
    | None -> String.Map.empty
    | Some field ->
      List.fold entries ~init:String.Map.empty ~f:(fun indexed entry ->
        match identity entry.Field.json ~field with
        | None -> indexed
        | Some key ->
          if Map.mem indexed key then indexed else Map.set indexed ~key ~data:entry)
  ;;

  let admit ?identity_field ~decode json =
    Agent_protocol.Json_codec.list (Field.admit ~decode) json
    |> Result.map ~f:(fun entries ->
      { decode; entries; identity_field; by_identity = index entries ~identity_field })
  ;;

  let reuse t ~position ~json =
    let positional = Option.bind position ~f:(fun entry -> Field.reuse entry ~json) in
    match positional with
    | Some _ -> positional
    | None ->
      Option.bind t.identity_field ~f:(fun field ->
        Option.bind (identity json ~field) ~f:(fun key ->
          Option.bind (Map.find t.by_identity key) ~f:(fun entry ->
            Field.reuse entry ~json)))
  ;;

  let readmit t ~json =
    match json with
    | `Array values ->
      let rec readmit_entries previous values result =
        match values with
        | [] -> List.rev result |> Result.all
        | json :: values ->
          let position, previous =
            match previous with
            | [] -> None, []
            | entry :: previous -> Some entry, previous
          in
          let entry =
            match reuse t ~position ~json with
            | Some entry -> Ok entry
            | None -> Field.admit ~decode:t.decode json
          in
          readmit_entries previous values (entry :: result)
      in
      Result.map (readmit_entries t.entries values []) ~f:(fun entries ->
        { decode = t.decode
        ; entries
        ; identity_field = t.identity_field
        ; by_identity = index entries ~identity_field:t.identity_field
        })
    | _ -> admit ?identity_field:t.identity_field ~decode:t.decode json
  ;;

  let values t = List.map t.entries ~f:Field.value
end
