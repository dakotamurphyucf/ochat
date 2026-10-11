open Core
module Inferred = Chatml_inferred_type
module Binding = Chatml_semantic_binding
module Recorder = Chatml_semantic_recorder
module Syntax = Chatml.Chatml_type_expr
module Name = Chatml.Chatml_syntax_name
module Sites = Chatml.Chatml_syntax_sites

module Parameter = struct
  type definition =
    | Authored of Name.t
    | Implicit of Source.span

  type t =
    { name : string
    ; inferred : Inferred.typ
    ; definition : definition
    }
end

type declaration =
  | Alias of Syntax.Declaration.t
  | Specification
  | Annotation

type t =
  { frame : Recorder.Frame.t
  ; group : Recorder.Group_stage.t
  ; declaration : declaration
  ; outer_context : Recorder.Context.t
  ; mutable binding : Binding.t option
  ; mutable context : Recorder.Context.t
  ; mutable parameters : Binding.t String.Map.t
  ; mutable parameter_order : Binding.t list
  ; mutable parameters_initialized : bool
  ; mutable finished : bool
  }

let recorder t = Recorder.Frame.recorder t.frame
let unit t = Recorder.Frame.unit t.frame
let binding t = t.binding
let context t = t.context
let take = Recorder.take_exn

let require_location = function
  | Some location -> location
  | None -> take (Error Recorder.Error.Missing_sites)
;;

let create frame context declaration binding ~outer_context ~checkpoint =
  let group = Recorder.stage_group (Recorder.Frame.recorder frame) ~checkpoint |> take in
  { frame
  ; group
  ; declaration
  ; outer_context
  ; binding
  ; context
  ; parameters = String.Map.empty
  ; parameter_order = []
  ; parameters_initialized = false
  ; finished = false
  }
;;

let define frame context name ~kind ~role ~checkpoint =
  let recorder = Recorder.Frame.recorder frame in
  let unit = Recorder.Frame.unit frame in
  let location =
    Recorder.named recorder unit ~role name ~checkpoint |> take |> require_location
  in
  let binding =
    Recorder.define
      recorder
      unit
      ~name:(Name.name name)
      ~kind
      ~definition:(Binding.Definition.Authored location)
      ~scheme:None
      ~members:None
      ~checkpoint
    |> take
  in
  let namespace =
    match kind with
    | Binding.Kind.Type_alias -> Binding.Namespace.Type
    | Value | Parameter | Module -> Value
    | Type_parameter -> Type_parameter
  in
  let context =
    Recorder.define_context
      recorder
      unit
      context
      ~namespace
      ~name:(Name.name name)
      ~binding
      ~checkpoint
    |> take
  in
  binding, context
;;

let alias_exn frame ~context declaration ~checkpoint =
  let outer_context = context in
  let binding, context =
    define
      frame
      context
      declaration.Syntax.Declaration.name
      ~kind:Type_alias
      ~role:Type_binder
      ~checkpoint
  in
  create frame context (Alias declaration) (Some binding) ~outer_context ~checkpoint
;;

let specification_exn frame ~context name ~checkpoint =
  let binding, _ =
    define frame context name ~kind:Value ~role:Signature_value_binder ~checkpoint
  in
  (* The specification name itself is not a recursively visible value binding
     in its annotation; only preceding signature items and quantified parameters
     are visible there. The caller installs the completed binding afterward. *)
  create frame context Specification (Some binding) ~outer_context:context ~checkpoint
;;

let annotation_exn frame ~context ~checkpoint =
  create frame context Annotation None ~outer_context:context ~checkpoint
;;

let parameters_exn t parameters ~checkpoint =
  if t.finished || t.parameters_initialized
  then invalid_arg "Semantic_elaboration.parameters_exn: reused parameter scope";
  t.parameters_initialized <- true;
  let recorder = recorder t in
  let unit = unit t in
  let bindings =
    List.map parameters ~f:(fun (parameter : Parameter.t) ->
      checkpoint ();
      let definition =
        match parameter.definition with
        | Authored name ->
          Recorder.named recorder unit ~role:Type_parameter_binder name ~checkpoint
          |> take
          |> require_location
          |> fun location -> Binding.Definition.Authored location
        | Implicit span ->
          let location =
            Chatml_source.of_span (Recorder.source unit) span ~checkpoint
            |> Or_error.ok_exn
          in
          Binding.Definition.Implicit_specification_parameter location
      in
      let root =
        Recorder.stage_root recorder t.group parameter.inferred ~checkpoint |> take
      in
      let binding =
        match Map.find t.parameters parameter.name with
        | Some binding ->
          Recorder.finish_scheme
            recorder
            binding
            ~scheme:(Recorder.Group_stage.id t.group, root)
            ~checkpoint
          |> take
        | None ->
          Recorder.define
            recorder
            unit
            ~name:parameter.name
            ~kind:Type_parameter
            ~definition
            ~scheme:(Some (Recorder.Group_stage.id t.group, root))
            ~members:None
            ~checkpoint
          |> take
      in
      if not (Map.mem t.parameters parameter.name)
      then
        t.context
        <- Recorder.define_context
             recorder
             unit
             t.context
             ~namespace:Type_parameter
             ~name:parameter.name
             ~binding
             ~checkpoint
           |> take;
      t.parameters <- Map.set t.parameters ~key:parameter.name ~data:binding;
      binding)
  in
  t.parameter_order <- bindings
;;

let implicit_parameter_exn t ~name ~span ~checkpoint =
  if t.finished || t.parameters_initialized
  then
    invalid_arg "Semantic_elaboration.implicit_parameter_exn: completed parameter scope";
  checkpoint ();
  if not (Map.mem t.parameters name)
  then (
    let recorder = recorder t in
    let unit = unit t in
    let location =
      Chatml_source.of_span (Recorder.source unit) span ~checkpoint |> Or_error.ok_exn
    in
    let binding =
      Recorder.define
        recorder
        unit
        ~name
        ~kind:Type_parameter
        ~definition:(Binding.Definition.Implicit_specification_parameter location)
        ~scheme:None
        ~members:None
        ~checkpoint
      |> take
    in
    t.context
    <- Recorder.define_context
         recorder
         unit
         t.context
         ~namespace:Type_parameter
         ~name
         ~binding
         ~checkpoint
       |> take;
    t.parameters <- Map.set t.parameters ~key:name ~data:binding)
;;

let parameter_reference_exn t ~name ~span ~checkpoint =
  let target =
    match Map.find t.parameters name with
    | Some binding -> Recorder.Target.Binding (Binding.symbol binding)
    | None ->
      invalid_arg
        "Semantic_elaboration.parameter_reference_exn: missing checked parameter"
  in
  let location =
    Chatml_source.of_span (Recorder.source (unit t)) span ~checkpoint |> Or_error.ok_exn
  in
  Recorder.reference
    (recorder t)
    (unit t)
    ~context:t.context
    ~location
    ~target
    ~inferred:None
    ~checkpoint
  |> take
;;

let name_reference_exn t name ~role ~target ~checkpoint =
  let location =
    Recorder.named (recorder t) (unit t) ~role name ~checkpoint
    |> take
    |> require_location
  in
  Recorder.reference
    (recorder t)
    (unit t)
    ~context:t.context
    ~location
    ~target
    ~inferred:None
    ~checkpoint
  |> take
;;

let constraint_parameter_exn t name ~checkpoint =
  let target =
    match Map.find t.parameters (Name.name name) with
    | Some binding -> Recorder.Target.Binding (Binding.symbol binding)
    | None ->
      invalid_arg "Semantic_elaboration.constraint_parameter_exn: unchecked parameter"
  in
  name_reference_exn t name ~role:Constraint_parameter_reference ~target ~checkpoint
;;

let finish_exn t syntax ~inferred ~checkpoint =
  if t.finished then invalid_arg "Semantic_elaboration.finish_exn: reused scope";
  let recorder = recorder t in
  let unit = unit t in
  let root = Recorder.stage_root recorder t.group inferred ~checkpoint |> take in
  Recorder.expression_root
    recorder
    unit
    ~context:t.context
    ~span:syntax.Syntax.span
    ~root
    ~checkpoint
  |> take;
  t.binding
  <- Option.map t.binding ~f:(fun binding ->
       Recorder.finish_scheme
         recorder
         binding
         ~scheme:(Recorder.Group_stage.id t.group, root)
         ~checkpoint
       |> take);
  (match t.declaration, t.binding with
   | Alias declaration, Some binding ->
     Recorder.alias
       recorder
       unit
       ~binding
       ~parameters:t.parameter_order
       ~declaration
       ~checkpoint
     |> take
   | Annotation, None | Specification, Some _ -> ()
   | Alias _, None | Annotation, Some _ | Specification, None -> assert false);
  Recorder.commit_group recorder t.group ~checkpoint |> take;
  t.finished <- true
;;

let definition_context_exn t ~checkpoint =
  if not t.finished
  then invalid_arg "Semantic_elaboration.definition_context_exn: unfinished scope";
  match t.declaration, t.binding with
  | Annotation, None -> t.outer_context
  | (Alias _ | Specification), Some binding ->
    let namespace =
      match t.declaration with
      | Alias _ -> Binding.Namespace.Type
      | Specification -> Value
      | Annotation -> assert false
    in
    Recorder.define_context
      (recorder t)
      (unit t)
      t.outer_context
      ~namespace
      ~name:(Binding.name binding)
      ~binding
      ~checkpoint
    |> take
  | Annotation, Some _ | (Alias _ | Specification), None -> assert false
;;
