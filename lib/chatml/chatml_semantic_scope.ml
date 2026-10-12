open Core
module Recorder = Chatml_semantic_recorder
module Binding = Chatml_semantic_binding
module Values = Chatml_value_scope
module Types = Chatml_type_alias
module Inferred = Chatml_inferred_type
module Pattern = Chatml.Chatml_binding_pattern
module Name = Chatml.Chatml_syntax_name

let take = Recorder.take_exn

let context_exn frame ~values ~types ~checkpoint =
  Recorder.join_context
    (Recorder.Frame.recorder frame)
    (Recorder.Frame.unit frame)
    ~values:(Values.semantic_context values)
    ~types:(Types.semantic_context types)
    ~checkpoint
  |> take
;;

let install_exn frame ~values ~types ~binding ~checkpoint =
  let recorder = Recorder.Frame.recorder frame in
  let unit = Recorder.Frame.unit frame in
  let context = context_exn frame ~values ~types ~checkpoint in
  let context =
    Recorder.define_context
      recorder
      unit
      context
      ~namespace:Value
      ~name:(Binding.name binding)
      ~binding
      ~checkpoint
    |> take
  in
  Values.with_semantic_exn values ~name:(Binding.name binding) ~binding ~context
;;

let define_exn frame ~values ~types ~name ~location ~kind ~scheme ~members ~checkpoint =
  let binding =
    Recorder.define
      (Recorder.Frame.recorder frame)
      (Recorder.Frame.unit frame)
      ~name
      ~kind
      ~definition:(Binding.Definition.Authored location)
      ~scheme:(Some scheme)
      ~members
      ~checkpoint
    |> take
  in
  install_exn frame ~values ~types ~binding ~checkpoint
;;

let projected_exn ?members frame ~values ~types ~pattern ~projected ~kind ~checkpoint =
  let recorder = Recorder.Frame.recorder frame in
  let unit = Recorder.Frame.unit frame in
  let names = Pattern.binders pattern ~checkpoint in
  let group, roots =
    Recorder.group recorder (List.map projected ~f:snd) ~checkpoint |> take
  in
  List.fold2_exn
    names
    (List.zip_exn projected roots)
    ~init:values
    ~f:(fun values (name, span) ((projected_name, _), root) ->
      checkpoint ();
      if not (Binding.Private.equal_name name projected_name ~checkpoint)
      then invalid_arg "Semantic_scope.projected_exn: checked projection order changed";
      match
        Recorder.authored recorder unit ~role:Value_binder ~span ~checkpoint |> take
      with
      | None -> values
      | Some location ->
        define_exn
          frame
          ~values
          ~types
          ~name
          ~location
          ~kind
          ~scheme:(group, root)
          ~members
          ~checkpoint)
;;

let named_exn frame ~values ~types ~name ~kind ~role ~scheme ~members ~checkpoint =
  let recorder = Recorder.Frame.recorder frame in
  let unit = Recorder.Frame.unit frame in
  match Recorder.named recorder unit ~role name ~checkpoint |> take with
  | None -> values
  | Some location ->
    let group, roots = Recorder.group recorder [ scheme ] ~checkpoint |> take in
    define_exn
      frame
      ~values
      ~types
      ~name:(Name.name name)
      ~location
      ~kind
      ~scheme:(group, List.hd_exn roots)
      ~members
      ~checkpoint
;;

let finish_exn frame ~values ~types ~binding ~scheme ~checkpoint =
  let recorder = Recorder.Frame.recorder frame in
  let group, roots = Recorder.group recorder [ scheme ] ~checkpoint |> take in
  let binding =
    Recorder.finish_scheme recorder binding ~scheme:(group, List.hd_exn roots) ~checkpoint
    |> take
  in
  install_exn frame ~values ~types ~binding ~checkpoint
;;

let imported_exn frame ~values ~types ~binding ~checkpoint =
  install_exn frame ~values ~types ~binding ~checkpoint
;;

let members_exn frame ~values ~types ~exports ~checkpoint =
  let values =
    Inferred.Env.fold exports ~init:[] ~f:(fun ~key:name ~data:_ bindings ->
      checkpoint ();
      match Option.bind (Values.find values ~name) ~f:Values.Binding.semantic with
      | None -> bindings
      | Some binding -> (name, binding) :: bindings)
  in
  let types = Types.semantic_exports types ~checkpoint in
  Recorder.members (Recorder.Frame.recorder frame) ~values ~types ~checkpoint |> take
;;

let opened_types_exn frame types ~exports ~checkpoint =
  let recorder = Recorder.Frame.recorder frame in
  let unit = Recorder.Frame.unit frame in
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
    List.fold
      (Types.semantic_exports exports ~checkpoint)
      ~init:context
      ~f:(fun context (name, binding) ->
        checkpoint ();
        Recorder.define_context
          recorder
          unit
          context
          ~namespace:Type
          ~name
          ~binding
          ~checkpoint
        |> take)
  in
  Types.with_semantic_context types context
;;

let reference_exn frame ~values ~types name ~role ~target ~inferred ~checkpoint =
  let recorder = Recorder.Frame.recorder frame in
  let unit = Recorder.Frame.unit frame in
  match Recorder.named recorder unit ~role name ~checkpoint |> take with
  | None -> ()
  | Some location ->
    let context = context_exn frame ~values ~types ~checkpoint in
    Recorder.reference recorder unit ~context ~location ~target ~inferred ~checkpoint
    |> take
;;
