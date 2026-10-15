open Core
module Recorder = Chatml_semantic_recorder
module Binding = Chatml_semantic_binding
module Values = Chatml_value_scope
module Types = Chatml_type_alias
module Inferred = Chatml_inferred_type

let take = Recorder.take_exn

module Pending = struct
  type t =
    { binding : Binding.t
    ; path_rev : string list
    ; remaining : (string * Inferred.typ) Sequence.t
    ; remaining_rows : Inferred.typ
    ; children_rev : (string * Binding.t) list
    }
end

let module_binding_exn frame group name inferred ~checkpoint =
  let recorder = Recorder.Frame.recorder frame in
  let unit = Recorder.Frame.unit frame in
  let checkpoint () = Recorder.poll recorder ~checkpoint |> take in
  let rec closed_row = function
    | Inferred.Row (_, tail) ->
      checkpoint ();
      closed_row tail
    | Empty_row ->
      checkpoint ();
      true
    | _ ->
      checkpoint ();
      false
  in
  let rec descend stack name inferred parent_rev =
    checkpoint ();
    let root = Recorder.stage_root recorder group inferred ~checkpoint |> take in
    (* Each node/root is admitted before allocating its path/child cursor. *)
    let path_rev = name :: parent_rev in
    let kind =
      match inferred with
      | Inferred.Module _ -> Binding.Kind.Module
      | _ -> Value
    in
    let binding =
      Recorder.define_host
        recorder
        unit
        ~name
        ~kind
        ~member_path_rev:path_rev
        ~scheme:(Recorder.Group_stage.id group, root)
        ~checkpoint
      |> take
    in
    match inferred with
    | Module row when closed_row row ->
      next
        ({ Pending.binding
         ; path_rev
         ; remaining = Sequence.empty
         ; remaining_rows = row
         ; children_rev = []
         }
         :: stack)
    | _ -> finish stack binding
  and next stack =
    checkpoint ();
    match stack with
    | [] -> assert false
    | pending :: rest ->
      (match Sequence.next pending.Pending.remaining with
       | Some ((name, inferred), remaining) ->
         let pending = { pending with remaining } in
         let duplicate =
           List.exists pending.children_rev ~f:(fun (earlier, _) ->
             checkpoint ();
             Binding.Private.equal_name earlier name ~checkpoint)
         in
         if duplicate
         then next (pending :: rest)
         else descend (pending :: rest) name inferred pending.path_rev
       | None ->
         (match pending.remaining_rows with
          | Row (fields, tail) ->
            next
              ({ pending with remaining = Map.to_sequence fields; remaining_rows = tail }
               :: rest)
          | Empty_row ->
            let values =
              List.fold pending.children_rev ~init:[] ~f:(fun values child ->
                checkpoint ();
                child :: values)
            in
            let members =
              Recorder.members recorder ~values ~types:[] ~checkpoint |> take
            in
            let binding =
              Recorder.finish_members recorder pending.binding members ~checkpoint |> take
            in
            finish rest binding
          | _ -> assert false))
  and finish stack binding =
    match stack with
    | [] -> binding
    | pending :: rest ->
      next
        ({ pending with
           Pending.children_rev = (Binding.name binding, binding) :: pending.children_rev
         }
         :: rest)
  in
  descend [] name inferred []
;;

let values_exn frame values ~surface ~globals ~modules ~checkpoint =
  let recorder = Recorder.Frame.recorder frame in
  let unit = Recorder.Frame.unit frame in
  let poll () = Recorder.poll recorder ~checkpoint |> take in
  let add values ~name ~binding =
    let context =
      Recorder.join_context
        recorder
        unit
        ~values:(Values.semantic_context values)
        ~types:None
        ~checkpoint
      |> take
    in
    let context =
      Recorder.define_context
        recorder
        unit
        context
        ~namespace:Value
        ~name
        ~binding
        ~checkpoint
      |> take
    in
    Values.with_semantic_exn values ~name ~binding ~context
  in
  let definitions = globals @ modules in
  match Recorder.Host_baseline.values recorder ~surface ~checkpoint |> take with
  | Some cached ->
    let rec reuse values definitions cached =
      poll ();
      match definitions, cached with
      | [], [] -> values
      | (name, _) :: definitions, (cached_name, binding) :: cached ->
        if not (Binding.Private.equal_name name cached_name ~checkpoint:poll)
        then invalid_arg "Semantic_host.values_exn: changed static surface";
        reuse (add values ~name ~binding) definitions cached
      | [], _ :: _ | _ :: _, [] ->
        invalid_arg "Semantic_host.values_exn: changed static surface length"
    in
    reuse values definitions cached
  | None ->
    let values, reversed =
      List.fold
        definitions
        ~init:(values, [])
        ~f:(fun (values, reversed) (name, inferred) ->
          checkpoint ();
          let group = Recorder.stage_group recorder ~checkpoint |> take in
          let binding = module_binding_exn frame group name inferred ~checkpoint in
          Recorder.commit_group recorder group ~checkpoint |> take;
          add values ~name ~binding, (name, binding) :: reversed)
    in
    Recorder.Host_baseline.retain_values recorder ~surface (List.rev reversed) ~checkpoint
    |> take;
    values
;;

let alias_exn frame types ~surface ~ordinal ~name ~body ~checkpoint =
  let recorder = Recorder.Frame.recorder frame in
  let unit = Recorder.Frame.unit frame in
  let binding =
    match Recorder.Host_baseline.alias recorder ~surface ~ordinal ~checkpoint |> take with
    | Some binding ->
      let poll () = Recorder.poll recorder ~checkpoint |> take in
      if
        (not (Binding.Kind.equal (Binding.kind binding) Type_alias))
        || not (Binding.Private.equal_name name (Binding.name binding) ~checkpoint:poll)
      then invalid_arg "Semantic_host.alias_exn: changed static alias";
      binding
    | None ->
      let group = Recorder.stage_group recorder ~checkpoint |> take in
      let root = Recorder.stage_root recorder group body ~checkpoint |> take in
      let binding =
        Recorder.define_host
          recorder
          unit
          ~name
          ~kind:Type_alias
          ~member_path_rev:[ name ]
          ~scheme:(Recorder.Group_stage.id group, root)
          ~checkpoint
        |> take
      in
      Recorder.commit_group recorder group ~checkpoint |> take;
      Recorder.Host_baseline.retain_alias recorder ~surface ~ordinal binding ~checkpoint
      |> take;
      binding
  in
  let context =
    Recorder.join_context
      recorder
      unit
      ~values:None
      ~types:(Types.semantic_context types)
      ~checkpoint
    |> take
  in
  let context =
    Recorder.define_context
      recorder
      unit
      context
      ~namespace:Type
      ~name
      ~binding
      ~checkpoint
    |> take
  in
  binding, context
;;
