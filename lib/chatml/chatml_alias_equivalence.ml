open Core
open Chatml_inferred_type

module Constructor = struct
  type t =
    | Int
    | Float
    | Bool
    | String
    | Unit
    | Empty_row
    | Array
    | Ref
    | Record
    | Module
    | Variant
    | Variant_payload
    | Tuple
    | Con of string
    | Shape_kind of Chatml.Chatml_shape_kind.t
    | Field_receiver
  [@@deriving equal]
end

type t =
  | Parameter of int
  | Function of t list * t
  | Legacy_function of argument list * result
  | Node of Constructor.t * t list
  | Row of t String.Map.t * t
  | Recursive of int * t
  | Recursive_variable of int

and argument =
  | Required of t
  | Unless_unit of t

and result =
  | Direct of t
  | Flatten of t

let legacy_function arguments result ~checkpoint =
  checkpoint ();
  match result with
  | Flatten _ -> Legacy_function (arguments, result)
  | Direct result ->
    let parameters =
      List.map arguments ~f:(fun argument ->
        checkpoint ();
        match argument with
        | Required ty -> Some ty
        | Unless_unit _ -> None)
    in
    (match Option.all parameters with
     | Some parameters -> Function (parameters, result)
     | None -> Legacy_function (arguments, Direct result))
;;

let arrow lhs ~result:rhs ~checkpoint =
  checkpoint ();
  let arguments =
    match lhs with
    | Node (Unit, []) -> []
    | Parameter _ -> [ Unless_unit lhs ]
    | Function _ | Legacy_function _ | Node _ | Row _ | Recursive _ | Recursive_variable _
      -> [ Required lhs ]
  in
  match rhs with
  | Function (parameters, result) ->
    legacy_function
      (arguments
       @ List.map parameters ~f:(fun parameter ->
         checkpoint ();
         Required parameter))
      (Direct result)
      ~checkpoint
  | Legacy_function (parameters, result) ->
    legacy_function (arguments @ parameters) result ~checkpoint
  | Parameter _ -> legacy_function arguments (Flatten rhs) ~checkpoint
  | Node _ | Row _ | Recursive _ | Recursive_variable _ ->
    legacy_function arguments (Direct rhs) ~checkpoint
;;

let recursive body ~binder ~checkpoint =
  let rec occurs ty =
    checkpoint ();
    match ty with
    | Recursive_variable other -> Int.equal binder other
    | Parameter _ -> false
    | Function (parameters, result) -> List.exists (result :: parameters) ~f:occurs
    | Legacy_function (arguments, result) ->
      let result =
        match result with
        | Direct result | Flatten result -> result
      in
      occurs result
      || List.exists arguments ~f:(function Required argument | Unless_unit argument ->
          occurs argument)
    | Node (_, arguments) -> List.exists arguments ~f:occurs
    | Row (fields, tail) -> occurs tail || Map.exists fields ~f:occurs
    | Recursive (_, body) -> occurs body
  in
  if occurs body then Recursive (binder, body) else body
;;

let of_closed_type ty ~fresh_binder ~checkpoint =
  let rec convert binders ty =
    checkpoint ();
    let open Result.Let_syntax in
    let convert = convert binders in
    let node constructor arguments =
      let%map arguments = List.map arguments ~f:convert |> Result.all in
      Node (constructor, arguments)
    in
    match ty with
    | Generic _ | Var _ ->
      Error "Symbolic alias comparison requires immutable closed types"
    | Fun (parameters, result) ->
      let%bind parameters = List.map parameters ~f:convert |> Result.all in
      let%map result = convert result in
      Function (parameters, result)
    | Mu (name, body) ->
      let binder = fresh_binder () in
      let%map body = convert_with_binder binders ~name ~binder body in
      Recursive (binder, body)
    | Rec_var name ->
      (match Map.find binders name with
       | Some binder -> Ok (Recursive_variable binder)
       | None -> Error ("Unbound symbolic recursive type variable '" ^ name))
    | Con (name, arguments) -> node (Con name) arguments
    | Ref body -> node Ref [ body ]
    | Record body -> node Record [ body ]
    | Module body -> node Module [ body ]
    | Field_receiver (kind, row) -> node Field_receiver [ kind; row ]
    | Shape_kind kind -> node (Shape_kind kind) []
    | Variant body -> node Variant [ body ]
    | Variant_payload arguments -> node Variant_payload arguments
    | Tuple arguments -> node Tuple arguments
    | Array body -> node Array [ body ]
    | Row (fields, tail) ->
      let%bind fields =
        Map.to_alist fields
        |> List.map ~f:(fun (name, ty) ->
          let%map ty = convert ty in
          name, ty)
        |> Result.all
      in
      let%map tail = convert tail in
      Row (String.Map.of_alist_exn fields, tail)
    | TInt -> node Int []
    | TFloat -> node Float []
    | Boolean -> node Bool []
    | String -> node String []
    | Empty_row -> node Empty_row []
    | Unit -> node Unit []
  and convert_with_binder binders ~name ~binder body =
    convert (Map.set binders ~key:name ~data:binder) body
  in
  convert String.Map.empty ty
;;

let equivalent left right ~checkpoint =
  let rec equal binders left right =
    checkpoint ();
    let equal = equal binders in
    let equal_list left right =
      Int.equal (List.length left) (List.length right)
      && List.for_all2_exn left right ~f:equal
    in
    match left, right with
    | Parameter left, Parameter right -> Int.equal left right
    | Function (left_parameters, left_result), Function (right_parameters, right_result)
      -> equal_list left_parameters right_parameters && equal left_result right_result
    | ( Legacy_function (left_arguments, left_result)
      , Legacy_function (right_arguments, right_result) ) ->
      Int.equal (List.length left_arguments) (List.length right_arguments)
      && List.for_all2_exn left_arguments right_arguments ~f:(fun left right ->
        checkpoint ();
        match left, right with
        | Required left, Required right | Unless_unit left, Unless_unit right ->
          equal left right
        | Required _, Unless_unit _ | Unless_unit _, Required _ -> false)
      &&
        (match left_result, right_result with
        | Direct left, Direct right | Flatten left, Flatten right -> equal left right
        | Direct _, Flatten _ | Flatten _, Direct _ -> false)
    | Node (left_constructor, left_arguments), Node (right_constructor, right_arguments)
      ->
      Constructor.equal left_constructor right_constructor
      && equal_list left_arguments right_arguments
    | Row (left_fields, left_tail), Row (right_fields, right_tail) ->
      Map.equal equal left_fields right_fields && equal left_tail right_tail
    | Recursive (left_binder, left_body), Recursive (right_binder, right_body) ->
      equal_with_binder binders ~left_binder ~right_binder left_body right_body
    | Recursive_variable left, Recursive_variable right ->
      Option.value_map (Map.find binders left) ~default:false ~f:(Int.equal right)
    | ( ( Parameter _
        | Function _
        | Legacy_function _
        | Node _
        | Row _
        | Recursive _
        | Recursive_variable _ )
      , _ ) -> false
  and equal_with_binder binders ~left_binder ~right_binder left right =
    equal (Map.set binders ~key:left_binder ~data:right_binder) left right
  in
  equal Int.Map.empty left right
;;
