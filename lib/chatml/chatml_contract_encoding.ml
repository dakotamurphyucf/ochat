open Core
module Spec = Chatml_builtin_spec

let type_scheme root ~checkpoint =
  let atom s = Sexp.Atom s in
  let node tag children = Sexp.List (atom tag :: children) in
  let map xs ~f =
    List.map xs ~f:(fun x ->
      checkpoint ();
      f x)
  in
  let rec ty t =
    checkpoint ();
    match t with
    | Spec.TVar name -> node "TVar" [ atom name ]
    | TCon (name, arguments) -> node "TCon" [ atom name; Sexp.List (map arguments ~f:ty) ]
    | TInt -> atom "TInt"
    | TFloat -> atom "TFloat"
    | TBool -> atom "TBool"
    | TString -> atom "TString"
    | TUnit -> atom "TUnit"
    | TArray t -> node "TArray" [ ty t ]
    | TRef t -> node "TRef" [ ty t ]
    | TTuple ts -> node "TTuple" [ Sexp.List (map ts ~f:ty) ]
    | TRecord r -> node "TRecord" [ row r ]
    | TModule r -> node "TModule" [ row r ]
    | TField_receiver (name, r) -> node "TField_receiver" [ atom name; row r ]
    | TVariant r -> node "TVariant" [ variant r ]
    | TFun (ts, r) -> node "TFun" [ Sexp.List (map ts ~f:ty); ty r ]
    | TMu (name, t) -> node "TMu" [ atom name; ty t ]
    | TRec_var name -> node "TRec_var" [ atom name ]
  and row = function
    | Spec.TRow_empty ->
      checkpoint ();
      atom "TRow_empty"
    | TRow_var name ->
      checkpoint ();
      node "TRow_var" [ atom name ]
    | TRow_extend (fields, r) ->
      checkpoint ();
      node
        "TRow_extend"
        [ Sexp.List (map fields ~f:(fun (name, t) -> Sexp.List [ atom name; ty t ]))
        ; row r
        ]
  and variant = function
    | Spec.TVariant_row_empty ->
      checkpoint ();
      atom "TVariant_row_empty"
    | TVariant_row_var name ->
      checkpoint ();
      node "TVariant_row_var" [ atom name ]
    | TVariant_row_extend (fields, r) ->
      checkpoint ();
      node
        "TVariant_row_extend"
        [ Sexp.List
            (map fields ~f:(fun (name, ts) ->
               Sexp.List [ atom name; Sexp.List (map ts ~f:ty) ]))
        ; variant r
        ]
  in
  ty root
;;

(* Sexplib0 0.17 machine syntax: spaces separate only adjacent unquoted atoms.
   Explicit pending events make list emission stack safe. Byte work polls in
   4096-byte chunks, including long native names and escaped binary atoms. *)
let to_string ?(allocate = fun _ -> ()) root ~checkpoint =
  allocate 1024;
  let buffer = Buffer.create 256 in
  let poll index = if index mod 4096 = 0 then checkpoint () in
  let quoted text =
    let rec scan index =
      if index >= String.length text
      then false
      else (
        poll index;
        match text.[index] with
        | '"' | '(' | ')' | ';' | '\\' -> true
        | '\000' .. '\032' | '\127' .. '\255' -> true
        | '#' when index > 0 && Char.equal text.[index - 1] '|' -> true
        | '|' when index > 0 && Char.equal text.[index - 1] '#' -> true
        | _ -> scan (index + 1))
    in
    String.is_empty text || scan 0
  in
  let copy text =
    let rec chunks offset =
      if offset < String.length text
      then (
        checkpoint ();
        let len = Int.min 4096 (String.length text - offset) in
        allocate (16 * len);
        Buffer.add_substring buffer text ~pos:offset ~len;
        chunks (offset + len))
    in
    chunks 0
  in
  let escape text =
    Buffer.add_char buffer '"';
    String.iteri text ~f:(fun index c ->
      poll index;
      if index mod 4096 = 0 then allocate (16 * Int.min 4096 (String.length text - index));
      match c with
      | '"' -> Buffer.add_string buffer "\\\""
      | '\\' -> Buffer.add_string buffer "\\\\"
      | '\n' -> Buffer.add_string buffer "\\n"
      | '\r' -> Buffer.add_string buffer "\\r"
      | '\t' -> Buffer.add_string buffer "\\t"
      | '\b' -> Buffer.add_string buffer "\\b"
      | ' ' .. '~' -> Buffer.add_char buffer c
      | c ->
        let code = Char.to_int c in
        Buffer.add_char buffer '\\';
        Buffer.add_char buffer (Char.of_int_exn (48 + (code / 100)));
        Buffer.add_char buffer (Char.of_int_exn (48 + (code / 10 mod 10)));
        Buffer.add_char buffer (Char.of_int_exn (48 + (code mod 10))));
    Buffer.add_char buffer '"'
  in
  let rec emit need_space events =
    allocate 128;
    match events with
    | [] ->
      checkpoint ();
      Buffer.contents buffer
    | `Close :: rest ->
      checkpoint ();
      Buffer.add_char buffer ')';
      emit false rest
    | `Nodes [] :: rest -> emit need_space rest
    | `Nodes (node :: nodes) :: rest ->
      emit need_space (`Node node :: `Nodes nodes :: rest)
    | `Node (Sexp.Atom text) :: rest ->
      checkpoint ();
      if quoted text
      then (
        escape text;
        emit false rest)
      else (
        if need_space then Buffer.add_char buffer ' ';
        copy text;
        emit true rest)
    | `Node (Sexp.List nodes) :: rest ->
      checkpoint ();
      Buffer.add_char buffer '(';
      emit false (`Nodes nodes :: `Close :: rest)
  in
  emit false [ `Node root ]
;;
