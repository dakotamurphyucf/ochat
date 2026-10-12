open Core
open Chatml_inferred_type

let tv_ref_mem needle haystack = List.exists haystack ~f:(phys_equal needle)
let rec_name_mem needle haystack = List.mem haystack needle ~equal:String.equal

let has_recursive_type ?(checkpoint = fun () -> ()) (ty : typ) : bool =
  let rec aux (seen : tv ref list) ty =
    checkpoint ();
    match ty with
    | Fun (params, ret) -> List.exists params ~f:(aux seen) || aux seen ret
    | Var ({ contents = Bound t } as tv) ->
      if tv_ref_mem tv seen then true else aux (tv :: seen) t
    | Con (_n, args) -> List.exists args ~f:(aux seen)
    | Mu (_binder, _body) -> true
    | Rec_var _ -> true
    | Var { contents = Free _ }
    | Generic _ | TInt | TFloat | Boolean | String | Empty_row | Unit | Shape_kind _ ->
      false
    | Ref t | Record t | Module t | Variant t | Array t -> aux seen t
    | Field_receiver (kind, row) -> aux seen kind || aux seen row
    | Tuple ts | Variant_payload ts -> List.exists ts ~f:(aux seen)
    | Row (fields, tail) ->
      Env.exists fields (fun _field ty' -> aux seen ty') || aux seen tail
  in
  aux [] ty
;;

let check_contractive ?(checkpoint = fun () -> ()) (ty : typ) : (unit, string) result =
  let rec check_list
            (seen : tv ref list)
            ~(target : name option)
            ~(guarded : bool)
            ~(scope : name list)
            (tys : typ list)
    : (unit, string) result
    =
    match tys with
    | [] -> Ok ()
    | hd :: tl ->
      (match check_ty seen ~target ~guarded ~scope hd with
       | Ok () -> check_list seen ~target ~guarded ~scope tl
       | Error _ as err -> err)
  and check_fields
        (seen : tv ref list)
        ~(target : name option)
        ~(guarded : bool)
        ~(scope : name list)
        (fields : typ Env.t)
    : (unit, string) result
    =
    fields |> Env.bindings |> List.map ~f:snd |> check_list seen ~target ~guarded ~scope
  and check_ty
        (seen : tv ref list)
        ~(target : name option)
        ~(guarded : bool)
        ~(scope : name list)
        (ty : typ)
    : (unit, string) result
    =
    checkpoint ();
    match ty with
    | Fun (params, ret) ->
      (match check_list seen ~target ~guarded:true ~scope params with
       | Ok () -> check_ty seen ~target ~guarded:true ~scope ret
       | Error _ as err -> err)
    | Var ({ contents = Bound bound_ty } as tv) ->
      if tv_ref_mem tv seen
      then Ok ()
      else check_ty (tv :: seen) ~target ~guarded ~scope bound_ty
    | Con (_n, args) -> check_list seen ~target ~guarded ~scope args
    | Var { contents = Free _ }
    | Generic _ | TInt | TFloat | Boolean | String | Empty_row | Unit -> Ok ()
    | Shape_kind _ -> Error "Representation kind is only valid in a field receiver"
    | Mu (binder, body) ->
      let scope' = binder :: scope in
      let outer_target =
        match target with
        | Some target_name when String.equal target_name binder -> None
        | _ -> target
      in
      (match check_ty seen ~target:(Some binder) ~guarded:false ~scope:scope' body with
       | Error _ as err -> err
       | Ok () -> check_ty seen ~target:outer_target ~guarded ~scope:scope' body)
    | Rec_var binder ->
      if not (rec_name_mem binder scope)
      then Error (Printf.sprintf "Unbound recursive type variable '%s'" binder)
      else (
        match target with
        | Some target_binder when String.equal target_binder binder && not guarded ->
          Error (Printf.sprintf "Unguarded recursive type variable '%s'" binder)
        | _ -> Ok ())
    | Ref inner | Record inner | Module inner | Array inner ->
      check_ty seen ~target ~guarded:true ~scope inner
    | Variant row -> check_variant_row seen ~target ~guarded:true ~scope row
    | Variant_payload _ -> Error "Variant payload is only valid in a variant row"
    | Field_receiver (kind, row) ->
      (match check_kind seen kind with
       | Error _ as error -> error
       | Ok () -> check_ty seen ~target ~guarded:true ~scope row)
    | Tuple tys -> check_list seen ~target ~guarded:true ~scope tys
    | Row (fields, tail) ->
      (match check_fields seen ~target ~guarded ~scope fields with
       | Ok () -> check_ty seen ~target ~guarded ~scope tail
       | Error _ as err -> err)
  and check_kind seen kind =
    checkpoint ();
    match kind with
    | Shape_kind _ | Generic _ | Var { contents = Free _ } -> Ok ()
    | Var ({ contents = Bound bound } as tv) ->
      if tv_ref_mem tv seen
      then Error "Cyclic field receiver representation kind"
      else check_kind (tv :: seen) bound
    | _ -> Error "Invalid field receiver representation kind"
  and check_variant_row seen ~target ~guarded ~scope row =
    checkpoint ();
    match row with
    | Row (cases, tail) ->
      let rec check_cases = function
        | [] -> check_variant_row seen ~target ~guarded ~scope tail
        | (_, Variant_payload arguments) :: rest ->
          (match check_list seen ~target ~guarded ~scope arguments with
           | Ok () -> check_cases rest
           | Error _ as error -> error)
        | _ -> Error "Variant row fields must retain constructor payload arity"
      in
      check_cases (Env.bindings cases)
    | Var ({ contents = Bound row } as tv) ->
      if tv_ref_mem tv seen
      then Ok ()
      else check_variant_row (tv :: seen) ~target ~guarded ~scope row
    | Empty_row | Generic _ | Var { contents = Free _ } -> Ok ()
    | _ -> Error "Invalid variant row"
  in
  check_ty [] ~target:None ~guarded:false ~scope:[] ty
;;
