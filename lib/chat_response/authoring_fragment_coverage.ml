open Core
module G = Agent_protocol.Authoring_guidance

type coverage =
  { topic : G.topic
  ; total : int
  ; parts : string Int.Map.t
  ; conflicting : bool
  }

let key topic = G.sexp_of_topic { topic with complete = true } |> Sexp.to_string_mach

let complete guidance =
  let open Result.Let_syntax in
  let%map () = List.map guidance ~f:G.validate |> Result.all_unit in
  let whole, groups =
    List.fold
      guidance
      ~init:(String.Map.empty, String.Map.empty)
      ~f:(fun (whole, groups) guidance ->
        match guidance.G.purpose with
        | Rediscovery -> whole, groups
        | Primer | Preload | Reference ->
          List.fold guidance.topics ~init:(whole, groups) ~f:(fun (whole, groups) topic ->
            let id = key topic in
            match
              List.find guidance.fragments ~f:(fun fragment ->
                String.equal fragment.G.topic_id topic.id)
            with
            | None ->
              (match topic.complete with
               | true -> Map.set whole ~key:id ~data:topic, groups
               | false -> whole, groups)
            | Some fragment ->
              let before =
                Map.find groups id
                |> Option.value
                     ~default:
                       { topic = { topic with complete = true }
                       ; total = fragment.total_parts
                       ; parts = Int.Map.empty
                       ; conflicting = false
                       }
              in
              let parts, conflicting =
                List.fold
                  fragment.parts
                  ~init:
                    ( before.parts
                    , before.conflicting || before.total <> fragment.total_parts )
                  ~f:(fun (parts, conflicting) part ->
                    match Map.find parts part.G.index with
                    | None ->
                      Map.set parts ~key:part.index ~data:part.item_sha256, conflicting
                    | Some hash ->
                      parts, conflicting || not (String.equal hash part.item_sha256))
              in
              whole, Map.set groups ~key:id ~data:{ before with parts; conflicting }))
  in
  Map.fold groups ~init:whole ~f:(fun ~key ~data complete ->
    match (not data.conflicting) && Map.length data.parts = data.total with
    | true -> Map.set complete ~key ~data:data.topic
    | false -> complete)
  |> Map.data
;;
