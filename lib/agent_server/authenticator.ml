open! Core

module Request_identity = struct
  type t =
    { client_address : Eio.Net.Sockaddr.stream
    ; headers : (string * string) list
    }
end

type bearer_validator =
  now:Agent_protocol.Timestamp.t
  -> token:string
  -> (Agent_protocol.Principal.t, Agent_protocol.Error.t) result

type record =
  { digest : string
  ; principal : Agent_protocol.Principal.t
  ; expires_at : Agent_protocol.Timestamp.t option
  }

type t = record list

let invalid message = Error message

let record_fields = function
  | Sexp.List fields ->
    List.map fields ~f:(function
      | Sexp.List [ Sexp.Atom name; value ] -> Ok (name, value)
      | _ -> invalid "token record fields must be (name value) pairs")
    |> Result.all
  | _ -> invalid "each token record must be a list"
;;

let field fields name =
  match
    List.filter_map fields ~f:(fun (field_name, value) ->
      Option.some_if (String.equal field_name name) value)
  with
  | [ value ] -> Ok value
  | [] -> invalid ("missing token record field: " ^ name)
  | _ -> invalid ("duplicate token record field: " ^ name)
;;

let optional_field fields name =
  match
    List.filter_map fields ~f:(fun (field_name, value) ->
      Option.some_if (String.equal field_name name) value)
  with
  | [ value ] -> Ok (Some value)
  | [] -> Ok None
  | _ -> invalid ("duplicate token record field: " ^ name)
;;

let atom name = function
  | Sexp.Atom value -> Ok value
  | _ -> invalid (name ^ " must be an atom")
;;

let decode_hex encoded =
  let nibble = function
    | '0' .. '9' as value -> Some (Char.to_int value - Char.to_int '0')
    | 'a' .. 'f' as value -> Some (Char.to_int value - Char.to_int 'a' + 10)
    | 'A' .. 'F' as value -> Some (Char.to_int value - Char.to_int 'A' + 10)
    | _ -> None
  in
  if not (Int.equal (String.length encoded) 64)
  then invalid "token_sha256 must contain exactly 64 hexadecimal characters"
  else (
    let bytes = Bytes.create 32 in
    let rec loop index =
      if index = 32
      then Ok (Bytes.unsafe_to_string ~no_mutation_while_string_reachable:bytes)
      else (
        match nibble encoded.[index * 2], nibble encoded.[(index * 2) + 1] with
        | Some high, Some low ->
          Bytes.set bytes index (Char.of_int_exn ((high lsl 4) lor low));
          loop (index + 1)
        | _ -> invalid "token_sha256 contains a non-hexadecimal character")
    in
    loop 0)
;;

let parse_scopes = function
  | Sexp.List values ->
    let open Result.Let_syntax in
    let%bind scopes =
      List.map values ~f:(fun value ->
        let%bind encoded = atom "scope" value in
        Agent_protocol.Scope.of_string encoded
        |> Result.map_error ~f:(fun error -> error.Agent_protocol.Error.message))
      |> Result.all
    in
    let set = Agent_protocol.Scope.Set.of_list scopes in
    if Set.length set = List.length scopes
    then Ok set
    else invalid "scope list contains duplicates"
  | _ -> invalid "scopes must be a list"
;;

let parse_attributes = function
  | Sexp.List values ->
    List.map values ~f:(function
      | Sexp.List [ Sexp.Atom name; Sexp.Atom value ] -> Ok (name, value)
      | _ -> invalid "attributes must contain (name value) pairs")
    |> Result.all
  | _ -> invalid "attributes must be a list"
;;

let parse_expiration = function
  | None | Some (Sexp.Atom "none") -> Ok None
  | Some value ->
    let open Result.Let_syntax in
    let%bind encoded = atom "expires_at" value in
    Agent_protocol.Timestamp.of_string encoded
    |> Result.map_error ~f:(fun error -> error.Agent_protocol.Error.message)
    |> Result.map ~f:Option.some
;;

let validate_field_names fields =
  let allowed =
    String.Set.of_list
      [ "token_sha256"; "principal_id"; "scopes"; "attributes"; "expires_at" ]
  in
  match List.find fields ~f:(fun (name, _) -> not (Set.mem allowed name)) with
  | None -> Ok ()
  | Some (name, _) -> invalid ("unknown token record field: " ^ name)
;;

let parse_principal fields =
  let open Result.Let_syntax in
  let%bind principal_id = field fields "principal_id" >>= atom "principal_id" in
  let%bind principal_id =
    Agent_protocol.Id.Principal.of_string principal_id
    |> Result.map_error ~f:(fun error -> error.Agent_protocol.Error.message)
  in
  let%bind scopes = field fields "scopes" >>= parse_scopes in
  let%bind attributes =
    optional_field fields "attributes"
    >>= function
    | None -> Ok []
    | Some value -> parse_attributes value
  in
  Agent_protocol.Principal.create
    ~id:principal_id
    ~authentication_kind:"http.static_bearer"
    ~scopes
    ~attributes
  |> Result.map_error ~f:(fun error -> error.Agent_protocol.Error.message)
;;

let parse_record sexp =
  let open Result.Let_syntax in
  let%bind fields = record_fields sexp in
  let%bind () = validate_field_names fields in
  let%bind digest = field fields "token_sha256" >>= atom "token_sha256" >>= decode_hex in
  let%bind principal = parse_principal fields in
  let%map expires_at = optional_field fields "expires_at" >>= parse_expiration in
  { digest; principal; expires_at }
;;

let parse contents =
  try
    match Sexp.of_string contents with
    | Sexp.List records when not (List.is_empty records) ->
      Result.all (List.map records ~f:parse_record)
    | Sexp.List [] -> invalid "static token file must contain at least one record"
    | _ -> invalid "static token file must be one list of token records"
  with
  | exn -> invalid ("static token file is invalid S-expression: " ^ Exn.to_string exn)
;;

let load_records ~env ~path =
  if not (Filename.is_absolute path)
  then invalid "static token file path must be absolute"
  else (
    try Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path) |> parse with
    | exn -> invalid ("unable to load static token file: " ^ Exn.to_string exn))
;;

let protocol_error message =
  Agent_protocol.Error.create Unauthenticated ~message ~retryable:false ()
;;

let load_static_file ~env ~path =
  load_records ~env ~path |> Result.map_error ~f:protocol_error
;;

let validate_static_file ~env ~path = Result.map (load_records ~env ~path) ~f:ignore

let constant_time_equal left right =
  let length = Int.max (String.length left) (String.length right) in
  let difference = ref (String.length left lxor String.length right) in
  for index = 0 to length - 1 do
    let left = if index < String.length left then Char.to_int left.[index] else 0 in
    let right = if index < String.length right then Char.to_int right.[index] else 0 in
    difference := !difference lor (left lxor right)
  done;
  Int.equal !difference 0
;;

let active ~now record =
  Option.for_all record.expires_at ~f:(fun expires_at ->
    Agent_protocol.Timestamp.compare now expires_at < 0)
;;

let authenticate_bearer t ~now ~token =
  let digest = Digestif.SHA256.digest_string token |> Digestif.SHA256.to_raw_string in
  match List.find t ~f:(fun record -> constant_time_equal digest record.digest) with
  | Some record when active ~now record -> Ok record.principal
  | Some _ -> Error (protocol_error "bearer token has expired")
  | None -> Error (protocol_error "bearer token is invalid")
;;

let client_host (identity : Request_identity.t) =
  match identity.client_address with
  | `Tcp (address, _) -> Some (Format.asprintf "%a" Eio.Net.Ipaddr.pp address)
  | `Unix _ -> None
;;

let trusted_proxy identity trusted_addresses =
  Option.exists (client_host identity) ~f:(fun address ->
    List.mem trusted_addresses address ~equal:String.equal)
;;

let header_values (identity : Request_identity.t) name =
  List.filter_map identity.headers ~f:(fun (header, value) ->
    Option.some_if (String.Caseless.equal header name) value)
;;

let asserted_header identity name =
  match header_values identity name with
  | [] -> Ok None
  | [ value ] when not (String.is_empty (String.strip value)) ->
    Ok (Some (String.strip value))
  | [ _ ] -> Error (protocol_error "trusted proxy asserted an empty identity header")
  | _ -> Error (protocol_error "trusted proxy asserted a duplicate identity header")
;;

let parse_asserted_scopes encoded =
  let open Result.Let_syntax in
  let values =
    String.split_on_chars encoded ~on:[ ','; ' '; '\t' ]
    |> List.filter ~f:(Fn.non String.is_empty)
  in
  let%bind scopes = List.map values ~f:Agent_protocol.Scope.of_string |> Result.all in
  let scopes = Agent_protocol.Scope.Set.of_list scopes in
  if List.is_empty values
  then Error (protocol_error "trusted proxy asserted no scopes")
  else if Set.length scopes <> List.length values
  then Error (protocol_error "trusted proxy asserted duplicate scopes")
  else Ok scopes
;;

let reverse_proxy_principal ~principal_header ~scopes_header identity =
  let open Result.Let_syntax in
  let%bind principal = asserted_header identity principal_header in
  let%bind scopes = asserted_header identity scopes_header in
  match principal, scopes with
  | None, None -> Ok None
  | Some _, None | None, Some _ ->
    Error (protocol_error "trusted proxy identity headers are incomplete")
  | Some principal, Some scopes ->
    let%bind id = Agent_protocol.Id.Principal.of_string principal in
    let%bind scopes = parse_asserted_scopes scopes in
    Agent_protocol.Principal.create
      ~id
      ~authentication_kind:"http.reverse_proxy"
      ~scopes
      ~attributes:
        (Option.value_map (client_host identity) ~default:[] ~f:(fun host ->
           [ "http.proxy", host ]))
    |> Result.map ~f:Option.some
;;

let authenticate_reverse_proxy
      ~trusted_addresses
      ~principal_header
      ~scopes_header
      identity
  =
  if trusted_proxy identity trusted_addresses
  then reverse_proxy_principal ~principal_header ~scopes_header identity
  else Ok None
;;
