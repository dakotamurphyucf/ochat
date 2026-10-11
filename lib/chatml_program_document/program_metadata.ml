open Core
module Captured = Chatml_source
module Prepared = Chatml_prepared_program
module Json = Document_schema.Json

type location =
  { source : Captured.Id.t
  ; digest : Captured.Digest.t
  ; start_byte : int
  ; end_byte : int
  }

type source =
  { id : Captured.Id.t
  ; digest : Captured.Digest.t
  ; byte_length : int
  }

type definition =
  { implementation : Captured.Id.t
  ; interface : Captured.Id.t option
  ; declared_at : location
  }

type binding =
  { name : string
  ; implementation : Captured.Id.t
  ; declared_at : location
  }

type scope =
  { source : Captured.Id.t
  ; selected_at : location option
  ; bindings : binding list
  }

type t =
  { fingerprint : Captured.Digest.t
  ; compiler_contract : Captured.Digest.t
  ; environment_name : string
  ; required_contract : Captured.Digest.t
  ; environment_declared_at : location option
  ; provenance : Prepared.Provenance.t
  ; entry : Captured.Id.t
  ; sources : source list
  ; definitions : definition list
  ; scopes : scope list
  }

let invalid path reason = Error (Document_schema.Error.Invalid_field { path; reason })

let object_ path = function
  | `Object _ -> Ok ()
  | _ -> invalid path "expected object"
;;

let required json path name =
  match Json.field json ~name with
  | Value value -> Ok value
  | Absent -> invalid (path @ [ name ]) "missing required field"
  | Null -> invalid (path @ [ name ]) "null is not admitted"
;;

let optional json path name ~f =
  match Json.field json ~name with
  | Absent -> Ok None
  | Null -> invalid (path @ [ name ]) "null is not admitted"
  | Value value -> Result.map (f (path @ [ name ]) value) ~f:Option.some
;;

let string path = function
  | `String value -> Ok value
  | _ -> invalid path "expected string"
;;

let field json path name ~f =
  Result.bind (required json path name) ~f:(f (path @ [ name ]))
;;

let int path = function
  | `Number value ->
    (match Int.of_string_opt value with
     | Some number when number >= 0 && String.equal value (Int.to_string number) ->
       Ok number
     | Some _ | None -> invalid path "expected canonical nonnegative integer")
  | _ -> invalid path "expected nonnegative integer"
;;

let hex path ~checkpoint value =
  let open Result.Let_syntax in
  let%bind text = string path value in
  if String.length text % 2 <> 0
  then invalid path "expected even-length lowercase hexadecimal bytes"
  else (
    let digit = function
      | '0' .. '9' as c -> Some (Char.to_int c - Char.to_int '0')
      | 'a' .. 'f' as c -> Some (10 + Char.to_int c - Char.to_int 'a')
      | _ -> None
    in
    (* Complete validation before allocating the decoded opaque label. *)
    let rec validate index =
      if index % 4096 = 0 then checkpoint ();
      if index = String.length text
      then Ok ()
      else (
        match digit text.[index] with
        | None -> invalid path "expected lowercase hexadecimal bytes"
        | Some _ -> validate (index + 1))
    in
    let%bind () = validate 0 in
    let bytes = Bytes.create (String.length text / 2) in
    for index = 0 to Bytes.length bytes - 1 do
      if index % 2048 = 0 then checkpoint ();
      let high = Option.value_exn (digit text.[2 * index]) in
      let low = Option.value_exn (digit text.[(2 * index) + 1]) in
      Bytes.set bytes index (Char.of_int_exn ((16 * high) + low))
    done;
    Ok (Bytes.to_string bytes))
;;

let id path ~checkpoint json =
  let open Result.Let_syntax in
  let%bind () = object_ path json in
  let%bind package_namespace = field json path "namespace_hex" ~f:(hex ~checkpoint) in
  let%bind relative_path = field json path "path" ~f:string in
  match Captured.Id.create ~package_namespace ~relative_path with
  | Ok id -> Ok id
  | Error _ -> invalid path "invalid logical source identity"
;;

let digest path json =
  let open Result.Let_syntax in
  let%bind text = string path json in
  match Captured.Digest.of_hex text with
  | Ok digest -> Ok digest
  | Error _ -> invalid path "invalid SHA-256 digest"
;;

let location path ~checkpoint json =
  let open Result.Let_syntax in
  let%bind () = object_ path json in
  let%bind source = field json path "source" ~f:(id ~checkpoint) in
  let%bind digest = field json path "digest" ~f:digest in
  let%bind start_byte = field json path "start_byte" ~f:int in
  let%bind end_byte = field json path "end_byte" ~f:int in
  if start_byte > end_byte
  then invalid path "reversed byte range"
  else Ok { source; digest; start_byte; end_byte }
;;

let list path ~checkpoint ~f = function
  | `Array values ->
    let rec loop index values result =
      checkpoint ();
      match values with
      | [] -> Ok (List.rev result)
      | value :: rest ->
        Result.bind
          (f (path @ [ Int.to_string index ]) value)
          ~f:(fun value -> loop (index + 1) rest (value :: result))
    in
    loop 0 values []
  | _ -> invalid path "expected array"
;;

let source path ~checkpoint json =
  let open Result.Let_syntax in
  let%bind () = object_ path json in
  let%bind id = field json path "id" ~f:(id ~checkpoint) in
  let%bind digest = field json path "digest" ~f:digest in
  let%bind byte_length = field json path "byte_length" ~f:int in
  Ok { id; digest; byte_length }
;;

let definition path ~checkpoint json =
  let open Result.Let_syntax in
  let%bind () = object_ path json in
  let%bind implementation = field json path "implementation" ~f:(id ~checkpoint) in
  let%bind interface = optional json path "interface" ~f:(id ~checkpoint) in
  let%bind declared_at = field json path "declared_at" ~f:(location ~checkpoint) in
  Ok { implementation; interface; declared_at }
;;

let binding path ~checkpoint json =
  let open Result.Let_syntax in
  let%bind () = object_ path json in
  let%bind name = field json path "name" ~f:string in
  let%bind implementation = field json path "implementation" ~f:(id ~checkpoint) in
  let%bind declared_at = field json path "declared_at" ~f:(location ~checkpoint) in
  Ok { name; implementation; declared_at }
;;

let scope path ~checkpoint json =
  let open Result.Let_syntax in
  let%bind () = object_ path json in
  let%bind source = field json path "source" ~f:(id ~checkpoint) in
  let%bind selected_at = optional json path "selected_at" ~f:(location ~checkpoint) in
  let%bind bindings =
    field json path "bindings" ~f:(list ~checkpoint ~f:(binding ~checkpoint))
  in
  Ok { source; selected_at; bindings }
;;

let provenance path ~checkpoint json =
  let open Result.Let_syntax in
  let%bind () = object_ path json in
  let%bind kind = field json path "kind" ~f:string in
  match kind with
  | "implicit" -> Ok Prepared.Provenance.Implicit
  | "project" ->
    let%bind declaration = field json path "declaration" ~f:(id ~checkpoint) in
    let%bind lock = optional json path "lock" ~f:(id ~checkpoint) in
    Ok (Prepared.Provenance.Project { declaration; lock })
  | _ -> invalid (path @ [ "kind" ]) "unknown provenance kind"
;;

let decode json ~checkpoint =
  let open Result.Let_syntax in
  let path = [ "payload" ] in
  checkpoint ();
  let%bind () = object_ path json in
  let%bind fingerprint = field json path "fingerprint" ~f:digest in
  let%bind compiler_contract = field json path "compiler_contract" ~f:digest in
  let%bind entry = field json path "entry" ~f:(id ~checkpoint) in
  let%bind provenance = field json path "provenance" ~f:(provenance ~checkpoint) in
  let%bind environment = required json path "environment" in
  let environment_path = path @ [ "environment" ] in
  let%bind () = object_ environment_path environment in
  let%bind environment_name =
    field environment environment_path "name_hex" ~f:(hex ~checkpoint)
  in
  if String.is_empty environment_name
  then invalid (environment_path @ [ "name_hex" ]) "empty environment name"
  else (
    let%bind required_contract =
      field environment environment_path "required_contract" ~f:digest
    in
    let%bind environment_declared_at =
      optional environment environment_path "declared_at" ~f:(location ~checkpoint)
    in
    let%bind sources =
      field json path "sources" ~f:(list ~checkpoint ~f:(source ~checkpoint))
    in
    let%bind definitions =
      field json path "definitions" ~f:(list ~checkpoint ~f:(definition ~checkpoint))
    in
    let%bind scopes =
      field json path "scopes" ~f:(list ~checkpoint ~f:(scope ~checkpoint))
    in
    Ok
      { fingerprint
      ; compiler_contract
      ; environment_name
      ; required_contract
      ; environment_declared_at
      ; provenance
      ; entry
      ; sources
      ; definitions
      ; scopes
      })
;;
