open! Core
module D = Document_schema
module P = Agent_protocol
module X = Persistence_codec

type stored =
  { disposition : Pending_disposition.t
  ; custody : Pending_metadata_custody.t option
  }

type t =
  { carrier : stored D.Extension_carrier.t
  ; encoded : Jsonaf.t
  }

let value t = (D.Extension_carrier.value t.carrier).disposition
let sexp_of_t t = Jsonaf.sexp_of_t t.encoded
let equal left right = Jsonaf.exactly_equal left.encoded right.encoded

let shape =
  X.shape_exn
    [ "history_id", D.Shape.value
    ; "generation", D.Shape.value
    ; "pending_revision", D.Shape.value
    ; "outcome", X.fields_shape [ "kind"; "content_revision"; "reason" ]
    ; "custody", D.Shape.nullable Pending_metadata_custody.shape
    ]
;;

let validate stored =
  match stored.custody with
  | None -> Ok ()
  | Some custody ->
    if
      P.History.Id.equal
        (Pending_disposition.history_id stored.disposition)
        (Pending_metadata_custody.history_id custody)
      && Int.equal
           (Pending_disposition.generation stored.disposition)
           (Pending_metadata_custody.generation custody)
    then Ok ()
    else
      Error
        (D.Error.Invalid_field
           { path = [ "custody" ]
           ; reason = "pending custody identity or generation differs"
           })
;;

let decode ~limits json =
  let open Result.Let_syntax in
  let%bind disposition = Pending_disposition.of_json json |> X.document_result in
  let%bind custody =
    Agent_store.Document_fields.required json "custody" (function
      | `Null -> Ok None
      | value ->
        Pending_metadata_custody.of_jsonaf value ~limits |> Result.map ~f:Option.some)
  in
  let stored = { disposition; custody } in
  let%map () = validate stored in
  stored
;;

let encode ?(raw_custody = false) ~limits stored =
  let open Result.Let_syntax in
  let%bind () = validate stored in
  let%bind custody =
    match stored.custody with
    | None -> Ok `Null
    | Some custody ->
      if raw_custody
      then Pending_metadata_custody.to_jsonaf custody ~limits
      else Pending_metadata_custody.known_jsonaf custody ~limits
  in
  match Pending_disposition.to_json stored.disposition with
  | `Object fields -> Ok (`Object (fields @ [ "custody", custody ]))
  | _ ->
    Error
      (D.Error.Invalid_field
         { path = []; reason = "pending disposition must be an object" })
;;

let codec ~limits =
  match
    D.Domain_codec.create
      ~limits
      ~kind:"session.pending_disposition"
      ~version:1
      ~shape
      ~supported_semantics:[]
      ~decode:(decode ~limits)
      ~encode:(encode ~limits)
  with
  | Ok codec -> codec
  | Error error ->
    raise_s [%sexp "invalid pending disposition codec", (error : D.Error.t)]
;;

let of_jsonaf payload ~limits =
  let open Result.Let_syntax in
  let%bind document =
    D.Document.create ~limits ~kind:"session.pending_disposition" ~version:1 ~payload
  in
  let%bind carrier = D.Domain_codec.decode (codec ~limits) document in
  let projected = D.Extension_carrier.value carrier in
  let%bind stored = decode ~limits payload in
  let%bind projected_json = encode ~limits projected in
  let%bind admitted_json = encode ~limits stored in
  let%map () =
    if Jsonaf.exactly_equal projected_json admitted_json
    then validate stored
    else
      Error
        (D.Error.Invalid_field
           { path = []
           ; reason = "original custody differs from known disposition projection"
           })
  in
  { carrier = D.Extension_carrier.with_value carrier stored; encoded = payload }
;;

let to_jsonaf t ~limits =
  D.Domain_codec.encode (codec ~limits) t.carrier |> Result.map ~f:D.Document.payload
;;

let authored disposition ~limits =
  let%bind.Result payload = encode ~limits { disposition; custody = None } in
  of_jsonaf payload ~limits
;;

let adopted source ~disposition ~limits =
  let open Result.Let_syntax in
  let input = Pending_input_document.value source in
  let%bind () =
    if
      P.History.Id.equal
        (P.Pending_input.history_id input)
        (Pending_disposition.history_id disposition)
      && Int.equal
           (P.Pending_input.generation input)
           (Pending_disposition.generation disposition)
      &&
      match Pending_disposition.outcome disposition with
      | Adopted revision ->
        P.History.Content_revision.equal
          revision
          (P.Pending_input.entry input).content_revision
      | Cancelled | Retired _ -> false
    then Ok ()
    else
      Error
        (D.Error.Invalid_field
           { path = []
           ; reason = "adoption disposition differs from admitted pending source"
           })
  in
  let%bind custody = Pending_metadata_custody.of_pending source ~limits in
  let%bind payload =
    encode ~raw_custody:true ~limits { disposition; custody = Some custody }
  in
  of_jsonaf payload ~limits
;;

let retired source ~disposition ~limits =
  let open Result.Let_syntax in
  let input = Pending_input_document.value source in
  let%bind () =
    if
      P.History.Id.equal
        (P.Pending_input.history_id input)
        (Pending_disposition.history_id disposition)
      && Int.equal
           (P.Pending_input.generation input)
           (Pending_disposition.generation disposition)
      &&
      match Pending_disposition.outcome disposition with
      | Cancelled | Retired _ -> true
      | Adopted _ -> false
    then Ok ()
    else
      Error
        (D.Error.Invalid_field
           { path = []
           ; reason = "retirement disposition differs from admitted pending source"
           })
  in
  let%bind custody = Pending_metadata_custody.of_pending source ~limits in
  let%bind payload =
    encode ~raw_custody:true ~limits { disposition; custody = Some custody }
  in
  of_jsonaf payload ~limits
;;

let owner t =
  match (D.Extension_carrier.value t.carrier).custody with
  | None -> Pending_input_document.Owner.Unknown
  | Some custody -> Pending_metadata_custody.owner custody
;;

let with_value t disposition ~limits =
  let open Result.Let_syntax in
  let original = D.Extension_carrier.value t.carrier in
  let%bind () =
    if
      P.History.Id.equal
        (Pending_disposition.history_id original.disposition)
        (Pending_disposition.history_id disposition)
      && Int.equal
           (Pending_disposition.generation original.disposition)
           (Pending_disposition.generation disposition)
    then Ok ()
    else
      Error
        (D.Error.Invalid_field
           { path = []; reason = "pending disposition cannot retarget custody" })
  in
  let candidate =
    { t with
      carrier = D.Extension_carrier.with_value t.carrier { original with disposition }
    }
  in
  let%map encoded = to_jsonaf candidate ~limits in
  { candidate with encoded }
;;

let retire_canonical t ~limits =
  let original = value t in
  match Pending_disposition.outcome original with
  | Cancelled | Retired _ -> Ok t
  | Adopted _ ->
    let%bind.Result changed =
      Pending_disposition.create
        ~history_id:(Pending_disposition.history_id original)
        ~generation:(Pending_disposition.generation original)
        ~pending_revision:(Pending_disposition.pending_revision original)
        ~outcome:(Retired Canonical_history_retired)
      |> X.document_result
    in
    with_value t changed ~limits
;;

(* Native sexp replay uses the repository's bounded default admission. Production
   persistence uses the owning whole-state codec and its explicit limits. *)
let t_of_sexp sexp =
  match of_jsonaf (Jsonaf.t_of_sexp sexp) ~limits:D.Limits.default with
  | Ok value -> value
  | Error error -> raise_s [%sexp "invalid pending carrier", (error : D.Error.t)]
;;

let known_jsonaf t ~limits = encode ~limits (D.Extension_carrier.value t.carrier)
