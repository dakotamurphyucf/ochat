open Core
module Inferred = Chatml_inferred_type
open Chatml_inferred_type
module Equality = Chatml.Chatml_equality_requirement

module Error = struct
  type t =
    | Representation_equality
    | Conflicting_equality of Equality.Conflict.t
  [@@deriving sexp_of]
end

module Role = struct
  type t =
    | Value
    | Kind
  [@@deriving equal]
end

module Cell = struct
  type t =
    { original : tv ref
    ; role : Role.t
    ; copy : tv ref
    }
end

type operation =
  | Generalise of int
  | Instantiate of
      { level : int
      ; fresh_name : unit -> name
      ; values : tv ref String.Table.t
      ; kinds : tv ref String.Table.t
      }

type work =
  | Visit of Role.t * typ * (typ -> unit)
  | Finish of (unit -> unit)

exception Rejected of Error.t

let combine_equality cell required =
  match !cell with
  | Bound _ -> assert false
  | Free info ->
    let equality =
      match info.equality with
      | None -> required
      | Some previous ->
        (match Equality.combine previous required with
         | Ok combined -> combined
         | Error conflict -> raise_notrace (Rejected (Conflicting_equality conflict)))
    in
    cell := Free { info with equality = Some equality }
;;

let transform root ~operation ~checkpoint =
  let cells = ref [] in
  let result = ref None in
  let work = ref [ Visit (Value, root, fun ty -> result := Some ty) ] in
  let find_cell original role =
    List.find !cells ~f:(fun cell ->
      checkpoint ();
      phys_equal cell.Cell.original original && Role.equal cell.role role)
  in
  let schedule children ~finish =
    let results = ref [] in
    let visits =
      List.rev_map children ~f:(fun (role, ty) ->
        checkpoint ();
        Visit (role, ty, fun ty -> results := ty :: !results))
    in
    let completion = Finish (fun () -> finish (List.rev !results)) in
    work := List.rev_append visits (completion :: !work)
  in
  let generic (role : Role.t) name equality =
    match operation with
    | Generalise _ -> Generic { name; equality }
    | Instantiate { level; fresh_name; values; kinds } ->
      let table =
        match role, equality with
        | Value, _ -> values
        | Kind, None -> kinds
        | Kind, Some _ -> raise_notrace (Rejected Representation_equality)
      in
      let cell =
        Hashtbl.find_or_add table name ~default:(fun () ->
          ref (Free { name = fresh_name (); level; equality = None }))
      in
      Option.iter equality ~f:(combine_equality cell);
      Var cell
  in
  let visit role ty deliver =
    let unary body construct =
      work
      := Visit
           ( role
           , body
           , fun body -> work := Finish (fun () -> deliver (construct body)) :: !work )
         :: !work
    in
    let many values construct =
      let children =
        List.map values ~f:(fun ty ->
          checkpoint ();
          role, ty)
      in
      schedule children ~finish:(fun values -> deliver (construct values))
    in
    match ty with
    | Var ({ contents = Bound body } as original) ->
      let memo_role =
        match operation with
        | Generalise _ -> Role.Value
        | Instantiate _ -> role
      in
      (match find_cell original memo_role with
       | Some cell -> deliver (Var cell.copy)
       | None ->
         let copy = ref (Bound Unit) in
         cells := { Cell.original; role = memo_role; copy } :: !cells;
         (* Identity exists before descent. Only invocation-owned copies mutate;
            incomplete placeholders are never exposed to a caller. *)
         work
         := Visit
              ( role
              , body
              , fun body ->
                  copy := Bound body;
                  work := Finish (fun () -> deliver body) :: !work )
            :: !work)
    | Var ({ contents = Free { name; level; equality } } as original) ->
      (match operation with
       | Generalise outer when level > outer -> deliver (Generic { name; equality })
       | Generalise _ | Instantiate _ -> deliver (Var original))
    | Generic { name; equality } -> deliver (generic role name equality)
    | Fun (parameters, result) ->
      let children =
        (role, result)
        :: List.rev_map parameters ~f:(fun ty ->
          checkpoint ();
          role, ty)
        |> List.rev
      in
      schedule children ~finish:(fun values ->
        match List.rev values with
        | result :: reversed_parameters ->
          deliver (Fun (List.rev reversed_parameters, result))
        | [] -> assert false)
    | Con (name, arguments) -> many arguments (fun arguments -> Con (name, arguments))
    | Mu (binder, body) -> unary body (fun body -> Mu (binder, body))
    | Ref body -> unary body (fun body -> Ref body)
    | Record row -> unary row (fun row -> Record row)
    | Module row -> unary row (fun row -> Module row)
    | Variant row -> unary row (fun row -> Variant row)
    | Array body -> unary body (fun body -> Array body)
    | Field_receiver (kind, row) ->
      let kind_role =
        match operation with
        | Generalise _ -> role
        | Instantiate _ -> Role.Kind
      in
      schedule
        [ kind_role, kind; Role.Value, row ]
        ~finish:(function
          | [ kind; row ] -> deliver (Field_receiver (kind, row))
          | _ -> assert false)
    | Tuple values -> many values (fun values -> Tuple values)
    | Variant_payload values -> many values (fun values -> Variant_payload values)
    | Row (fields, tail) ->
      let reversed =
        Env.fold fields ~init:[] ~f:(fun ~key ~data values ->
          checkpoint ();
          (key, data) :: values)
      in
      let fields = List.rev reversed in
      let children =
        List.rev_append
          (List.rev_map fields ~f:(fun (_, ty) ->
             checkpoint ();
             role, ty))
          [ role, tail ]
      in
      schedule children ~finish:(fun values ->
        let rec rebuild fields values result =
          match fields, values with
          | [], [ tail ] -> deliver (Row (result, tail))
          | (name, _) :: fields, value :: values ->
            checkpoint ();
            rebuild fields values (Env.add name value result)
          | [], [] | [], _ :: _ :: _ | _ :: _, [] -> assert false
        in
        rebuild fields values Env.empty)
    | Rec_var _ | Shape_kind _ | TInt | TFloat | Boolean | String | Empty_row | Unit ->
      deliver ty
  in
  let rec run () =
    match !work with
    | [] ->
      (match !result with
       | Some ty -> ty
       | None -> assert false)
    | job :: rest ->
      checkpoint ();
      work := rest;
      (match job with
       | Visit (role, ty, deliver) -> visit role ty deliver
       | Finish finish -> finish ());
      run ()
  in
  run ()
;;

let generalise ty ~level ~checkpoint =
  transform ty ~operation:(Generalise level) ~checkpoint
;;

let instantiate ty ~level ~fresh_name ~checkpoint =
  let operation =
    Instantiate
      { level
      ; fresh_name
      ; values = Hashtbl.create (module String)
      ; kinds = Hashtbl.create (module String)
      }
  in
  try Ok (transform ty ~operation ~checkpoint) with
  | Rejected error -> Error error
;;
