open Core
module Spec = Chatml_builtin_spec

type t =
  { atom : string -> unit
  ; list : (unit -> unit) -> unit
  }

let atom t = t.atom
let list t = t.list

let type_scheme t root =
  let atom = atom t in
  let list = list t in
  let node tag emit =
    list (fun () ->
      atom tag;
      emit ())
  in
  let nodes values emit = list (fun () -> List.iter values ~f:emit) in
  let rec ty = function
    | Spec.TVar name -> node "TVar" (fun () -> atom name)
    | TCon (name, arguments) ->
      node "TCon" (fun () ->
        atom name;
        nodes arguments ty)
    | TInt -> atom "TInt"
    | TFloat -> atom "TFloat"
    | TBool -> atom "TBool"
    | TString -> atom "TString"
    | TUnit -> atom "TUnit"
    | TArray value -> node "TArray" (fun () -> ty value)
    | TRef value -> node "TRef" (fun () -> ty value)
    | TTuple values -> node "TTuple" (fun () -> nodes values ty)
    | TRecord value -> node "TRecord" (fun () -> row value)
    | TModule value -> node "TModule" (fun () -> row value)
    | TField_receiver (name, value) ->
      node "TField_receiver" (fun () ->
        atom name;
        row value)
    | TVariant value -> node "TVariant" (fun () -> variant value)
    | TFun (parameters, result) ->
      node "TFun" (fun () ->
        nodes parameters ty;
        ty result)
    | TMu (name, value) ->
      node "TMu" (fun () ->
        atom name;
        ty value)
    | TRec_var name -> node "TRec_var" (fun () -> atom name)
  and row = function
    | Spec.TRow_empty -> atom "TRow_empty"
    | TRow_var name -> node "TRow_var" (fun () -> atom name)
    | TRow_extend (fields, rest) ->
      node "TRow_extend" (fun () ->
        nodes fields (fun (name, value) ->
          list (fun () ->
            atom name;
            ty value));
        row rest)
  and variant = function
    | Spec.TVariant_row_empty -> atom "TVariant_row_empty"
    | TVariant_row_var name -> node "TVariant_row_var" (fun () -> atom name)
    | TVariant_row_extend (fields, rest) ->
      node "TVariant_row_extend" (fun () ->
        nodes fields (fun (name, values) ->
          list (fun () ->
            atom name;
            nodes values ty));
        variant rest)
  in
  ty root
;;

let to_sexp ~checkpoint emit =
  let reversed = ref [] in
  let atom text =
    checkpoint ();
    reversed := Sexp.Atom text :: !reversed
  in
  let list emit =
    checkpoint ();
    let parent = !reversed in
    reversed := [];
    emit ();
    let children =
      List.fold !reversed ~init:[] ~f:(fun result child ->
        checkpoint ();
        child :: result)
    in
    reversed := Sexp.List children :: parent
  in
  emit { atom; list };
  match !reversed with
  | [ root ] -> root
  | _ -> failwith "compiler identity must have one root"
;;

(* Sexplib0 0.17 machine syntax: spaces separate only adjacent unquoted atoms.
   Byte work polls in 4096-byte chunks, including long native names and escaped
   binary atoms. The emitter reads original type nodes and never builds a second
   tree for byte output. *)
let to_string ?(allocate = fun _ -> ()) ~checkpoint emit =
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
  let needs_space = ref false in
  let charge_node () =
    checkpoint ();
    allocate 256
  in
  let atom text =
    charge_node ();
    if quoted text
    then (
      escape text;
      needs_space := false)
    else (
      if !needs_space then Buffer.add_char buffer ' ';
      copy text;
      needs_space := true)
  in
  let list emit =
    charge_node ();
    Buffer.add_char buffer '(';
    needs_space := false;
    emit ();
    Buffer.add_char buffer ')';
    needs_space := false
  in
  emit { atom; list };
  checkpoint ();
  Buffer.contents buffer
;;
