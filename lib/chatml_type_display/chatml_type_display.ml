open Core
module Frozen = Chatml_typechecker.Type_snapshot
module Equality = Chatml.Chatml_equality_requirement
module Budget = Chatml_type_display_budget
module Graph = Chatml_type_display_graph
module Limits = Budget.Limits
module Completeness = Budget.Completeness

module Kind = struct
  type t =
    | Type
    | Signature
  [@@deriving equal, sexp_of]
end

module Error = struct
  type t =
    | Foreign_root
    | Invalid_max_bytes
  [@@deriving sexp_of]
end

type t =
  { kind : Kind.t
  ; completeness : Completeness.t
  ; text : string
  }

let kind t = t.kind
let completeness t = t.completeness
let text t = t.text

module Session = struct
  open Completeness
  open Graph

  type session = Budget.session

  let create = Budget.create
  let work = Budget.work
  let emit = Budget.Writer.emit
  let escape_label = Budget.Writer.escape_label

  type task =
    | Emit of string
    | Label of string
    | Visit of Frozen.Id.t * int * bool
    | Sequence of Frozen.Id.t list * string * int
    | Row_fields of (string * Frozen.Id.t) list * Frozen.Id.t * int * bool
    | Labels of string list * bool
    | Fields of (string * Frozen.Id.t) list * int * bool
    | Variant_fields of (string * Frozen.Id.t list) list * int * bool

  let render session ?max_bytes graph ~root ~kind ~checkpoint =
    if Option.value_map max_bytes ~default:false ~f:(fun value -> value < 0)
    then Error Error.Invalid_max_bytes
    else if Option.is_none (Frozen.find graph root)
    then Error Error.Foreign_root
    else if Option.value_map max_bytes ~default:false ~f:(Int.equal 0)
    then Ok { kind; completeness = Truncated Byte_limit; text = "" }
    else (
      let writer =
        Budget.Writer.create
          session
          ~max_bytes:(Option.value max_bytes ~default:Int.max_value)
          ~checkpoint
      in
      let result =
        try
          let scan = anchors session graph root ~checkpoint in
          let rendered = Int.Hash_set.create () in
          let tasks = ref [ Visit (root, 0, false) ] in
          let push value = tasks := value :: !tasks in
          while not (List.is_empty !tasks) do
            work session ~checkpoint;
            match !tasks with
            | [] -> ()
            | task :: rest ->
              tasks := rest;
              (match task with
               | Emit value -> emit writer value
               | Label value -> escape_label writer value
               | Sequence ([], _, _) -> ()
               | Sequence (id :: tail, separator, depth) ->
                 if not (List.is_empty tail)
                 then (
                   push (Sequence (tail, separator, depth));
                   push (Emit separator));
                 push (Visit (id, depth, true))
               | Labels ([], _) -> ()
               | Labels (name :: tail, first) ->
                 push (Labels (tail, false));
                 push (Label name);
                 if not first then push (Emit ", ")
               | Row_fields ([], tail, depth, _) ->
                 push (Visit (tail, depth, true));
                 push (Emit "} over ")
               | Row_fields ((name, id) :: tail_fields, tail, depth, first) ->
                 push (Row_fields (tail_fields, tail, depth, false));
                 push (Visit (id, depth, true));
                 push (Emit ": ");
                 push (Label name);
                 if not first then push (Emit "; ")
               | Fields ([], _, _) -> ()
               | Fields ((name, id) :: tail, depth, first) ->
                 push (Fields (tail, depth, false));
                 push (Visit (id, depth, true));
                 push (Emit ": ");
                 push (Label name);
                 if not first then push (Emit "; ")
               | Variant_fields ([], _, _) -> ()
               | Variant_fields ((name, arguments) :: tail, depth, first) ->
                 push (Variant_fields (tail, depth, false));
                 (match arguments with
                  | [] -> ()
                  | _ ->
                    push (Emit ")");
                    push (Sequence (arguments, ", ", depth));
                    push (Emit "("));
                 push (Label name);
                 push (Emit "`");
                 if not first then push (Emit " | ")
               | Visit (id, depth, nested) ->
                 if depth > Budget.max_depth session then raise (Budget.Stop Depth_limit);
                 let index = Frozen.Id.index id in
                 work session ~checkpoint;
                 let value =
                   match Frozen.find graph id with
                   | None -> raise (Budget.Stop Work_limit)
                   | Some value -> value
                 in
                 let is_anchor =
                   Graph.incoming scan id > 1
                   && not (trivial_leaf session graph value ~checkpoint)
                 in
                 if is_anchor && Hash_set.mem rendered index
                 then emit writer (sprintf "#%d" index)
                 else (
                   if is_anchor
                   then (
                     Hash_set.add rendered index;
                     emit writer (sprintf "#%d=(" index);
                     push (Emit ")"));
                   let next = depth + 1 in
                   match value with
                   | Variable (Generalized { quantifier; name; equality }) ->
                     (match equality with
                      | None -> ()
                      | Some Value -> push (Emit " {eq}")
                      | Some (Row excluded) ->
                        let names =
                          Set.fold excluded ~init:[] ~f:(fun names name ->
                            work session ~checkpoint;
                            name :: names)
                          |> fun names -> Budget.reverse_list session names ~checkpoint
                        in
                        push (Emit ")}");
                        push (Labels (names, true));
                        push (Emit " {row_eq_except("));
                     work session ~checkpoint;
                     let duplicate_name =
                       Graph.quantifier_ambiguous scan name ~session ~checkpoint
                     in
                     if duplicate_name
                     then
                       push (Emit (sprintf "#%d" (Frozen.Quantifier_id.index quantifier)));
                     push (Label name);
                     push (Emit "'")
                   | Variable (Unquantified { name; equality }) ->
                     (match equality with
                      | None -> ()
                      | Some Value -> push (Emit " {eq}")
                      | Some (Row excluded) ->
                        let names =
                          Set.fold excluded ~init:[] ~f:(fun names name ->
                            work session ~checkpoint;
                            name :: names)
                          |> fun names -> Budget.reverse_list session names ~checkpoint
                        in
                        push (Emit ")}");
                        push (Labels (names, true));
                        push (Emit " {row_eq_except("));
                     push (Emit (sprintf "#%d" index));
                     push (Label name);
                     push (Emit "?")
                   | Bound_link child -> push (Visit (child, next, nested))
                   | Function (arguments, result) ->
                     if nested then push (Emit ")");
                     push (Visit (result, next, true));
                     push (Emit ") -> ");
                     push (Sequence (arguments, ", ", next));
                     push (Emit "fun(");
                     if nested then push (Emit "(")
                   | Constructor (name, arguments) ->
                     (match arguments with
                      | [] -> push (Label name)
                      | _ ->
                        push (Emit ">");
                        push (Sequence (arguments, ", ", next));
                        push (Emit "<");
                        push (Label name))
                   | Recursive (binder, body) ->
                     if nested then push (Emit ")");
                     push (Visit (body, next, true));
                     push (Emit ". ");
                     push (Emit (sprintf "#%d" (Frozen.Recursive_binder_id.index binder)));
                     push
                       (Label
                          (Frozen.recursive_binder_name graph binder
                           |> Option.value ~default:"rec"));
                     push (Emit "mu ");
                     if nested then push (Emit "(")
                   | Recursive_variable binder ->
                     push (Emit (sprintf "#%d" (Frozen.Recursive_binder_id.index binder)));
                     push
                       (Label
                          (Frozen.recursive_binder_name graph binder
                           |> Option.value ~default:"rec"));
                     push (Emit "rec:")
                   | Reference child ->
                     push (Emit ")");
                     push (Visit (child, next, true));
                     push (Emit "ref(")
                   | Record row ->
                     (match view_row session graph row ~checkpoint with
                      | None ->
                        push (Emit ")");
                        push (Visit (row, next, true));
                        push (Emit "record_raw(")
                      | Some { fields; tail } ->
                        push (Emit "}");
                        Option.iter tail ~f:(fun id ->
                          push (Visit (id, next, true));
                          push (Emit (if List.is_empty fields then ".." else "; ..")));
                        push (Fields (fields, next, true));
                        push (Emit "{"))
                   | Module row ->
                     (match view_row session graph row ~checkpoint with
                      | None ->
                        push (Emit ")");
                        push (Visit (row, next, true));
                        push (Emit "module_raw(")
                      | Some { fields; tail } ->
                        push (Emit "}");
                        Option.iter tail ~f:(fun id ->
                          push (Visit (id, next, true));
                          push (Emit (if List.is_empty fields then ".." else "; ..")));
                        push (Fields (fields, next, true));
                        push (Emit "module{"))
                   | Field_receiver (record, modul) ->
                     push (Emit ")");
                     push (Visit (modul, next, true));
                     push (Emit ", ");
                     push (Visit (record, next, true));
                     push (Emit "field_receiver(")
                   | Shape_kind shape ->
                     push (Emit ")");
                     push (Emit (Chatml.Chatml_shape_kind.name shape));
                     push (Emit "internal_shape(")
                   | Variant row ->
                     (match view_row session graph row ~checkpoint with
                      | Some { fields; tail } ->
                        (match variant_fields session graph fields ~checkpoint with
                         | Some fields ->
                           push (Emit "]");
                           Option.iter tail ~f:(fun id ->
                             push (Visit (id, next, true));
                             push (Emit (if List.is_empty fields then ".." else " | ..")));
                           push (Variant_fields (fields, next, true));
                           push (Emit "[")
                         | None ->
                           push (Emit ")");
                           push (Visit (row, next, true));
                           push (Emit "variant_raw("))
                      | None ->
                        push (Emit ")");
                        push (Visit (row, next, true));
                        push (Emit "variant_raw("))
                   | Variant_payload arguments ->
                     push (Emit ")");
                     push (Sequence (arguments, ", ", next));
                     push (Emit "internal_payload(")
                   | Tuple elements ->
                     (match elements with
                      | _ :: _ :: _ ->
                        push (Emit ")");
                        push (Sequence (elements, " * ", next));
                        push (Emit "(")
                      | _ ->
                        push (Emit ")");
                        push (Sequence (elements, ", ", next));
                        push (Emit "internal_tuple("))
                   | Array child ->
                     push (Emit " array");
                     push (Visit (child, next, true))
                   | Int -> push (Emit "int")
                   | Float -> push (Emit "float")
                   | Bool -> push (Emit "bool")
                   | String -> push (Emit "string")
                   | Row (fields, tail) ->
                     push (Row_fields (fields, tail, next, true));
                     push (Emit "row_layer{")
                   | Empty_row -> push (Emit "{}")
                   | Unit -> push (Emit "unit")))
          done;
          let text = Budget.Writer.finish writer in
          Ok { kind; completeness = Complete; text }
        with
        | Budget.Stop reason ->
          let text = Budget.Writer.truncated writer in
          Ok { kind; completeness = Truncated reason; text }
      in
      result)
  ;;
end
