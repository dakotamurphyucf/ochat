open Core
module Static = Chatml_static_surface

module Target = struct
  type t =
    | One_off_v1
    | Tool_v1
    | Developer_tool_v1
    | Moderator_v1
    | Delegated_moderator_v1
  [@@deriving equal, sexp]
end

module Kind = struct
  type t =
    | Standard of Target.t
    | Native
end

type t =
  { kind : Kind.t
  ; surface : Static.t
  ; required_bindings : (string * Chatml_builtin_spec.ty) list
  ; optional_bindings : (string * Chatml_builtin_spec.ty) list
  }

let for_target target ~checkpoint =
  let module X = Chatml_extension_surface in
  let surface, required_bindings =
    match target with
    | Target.One_off_v1 -> X.one_off_v1, X.one_off_entrypoints
    | Tool_v1 -> X.tool_v1, X.tool_entrypoints
    | Developer_tool_v1 -> X.developer_tool_v1, X.tool_entrypoints
    | Moderator_v1 -> X.moderator_v1, X.moderator_entrypoints
    | Delegated_moderator_v1 -> X.delegated_moderator_v1, X.moderator_entrypoints
  in
  let optional_bindings =
    match target with
    | Moderator_v1 | Delegated_moderator_v1 ->
      [ Chatml_context_rollover.entrypoint, Chatml_context_rollover.scheme ]
    | One_off_v1 | Tool_v1 | Developer_tool_v1 -> []
  in
  { kind = Kind.Standard target
  ; surface = Static.of_runtime_surface surface ~checkpoint
  ; required_bindings
  ; optional_bindings
  }
;;

let kind t = t.kind

let compiler_family t =
  match t.kind with
  | Standard _ -> "ochat.chatml.compiler.v1"
  | Native -> "ochat.chatml.compiler.native.v1"
;;

let target_name t =
  match t.kind with
  | Standard One_off_v1 -> "one_off_v1"
  | Standard Tool_v1 -> "tool_v1"
  | Standard Developer_tool_v1 -> "developer_tool_v1"
  | Standard Moderator_v1 -> "moderator_v1"
  | Standard Delegated_moderator_v1 -> "delegated_moderator_v1"
  | Native -> "native"
;;

let static_surface t = t.surface
let required_bindings t = t.required_bindings
let optional_bindings t = t.optional_bindings

let for_static_surface surface ~required_bindings =
  { kind = Kind.Native; surface; required_bindings; optional_bindings = [] }
;;

let emit_identity t writer =
  let module E = Chatml_contract_encoding in
  let atom = E.atom writer in
  let list = E.list writer in
  let binding name scheme =
    list (fun () ->
      atom name;
      E.type_scheme writer scheme)
  in
  let bindings entries =
    list (fun () ->
      List.iter entries ~f:(fun (b : Static.Binding.t) -> binding b.name b.scheme))
  in
  let entrypoints entries =
    List.iter entries ~f:(fun (name, scheme) -> binding name scheme)
  in
  let target =
    match t.kind with
    | Standard target ->
      (match Target.sexp_of_t target with
       | Atom name -> name
       | List _ -> assert false)
    | Native -> "Native"
  in
  list (fun () ->
    atom (compiler_family t);
    atom target;
    bindings (Static.globals t.surface);
    list (fun () ->
      List.iter (Static.modules t.surface) ~f:(fun (m : Static.Module.t) ->
        list (fun () ->
          atom m.name;
          bindings m.exports)));
    list (fun () ->
      List.iter
        (Static.type_aliases t.surface)
        ~f:(fun (a : Chatml_builtin_surface.builtin_type_alias) -> binding a.name a.body));
    list (fun () -> entrypoints t.required_bindings);
    match t.optional_bindings with
    | [] -> ()
    | entries ->
      list (fun () ->
        atom "Optional_bindings";
        entrypoints entries))
;;

let identity t ~checkpoint =
  Chatml_contract_encoding.to_sexp ~checkpoint (emit_identity t)
;;

module Identity_summary = struct
  type t =
    { encoded_bytes : int
    ; fingerprint : Chatml_source.Digest.t
    }

  let encoded_bytes t = t.encoded_bytes
  let fingerprint t = t.fingerprint
end

let encode_identity ?allocate t ~checkpoint =
  Chatml_contract_encoding.to_string ?allocate ~checkpoint (emit_identity t)
;;

let summarize_identity t ~checkpoint ~allocate =
  let encoded = encode_identity t ~checkpoint ~allocate in
  Identity_summary.
    { encoded_bytes = String.length encoded
    ; fingerprint = Chatml_source.Digest.of_contents encoded ~checkpoint
    }
;;

let input_bytes t ~checkpoint ~allocate =
  encode_identity t ~checkpoint ~allocate |> String.length
;;

let fingerprint t ~checkpoint =
  let encoded = encode_identity t ~checkpoint in
  Chatml_source.Digest.of_contents encoded ~checkpoint
;;

let matches_runtime_surface t surface ~checkpoint ?(allocate = fun _ -> ()) () =
  let charged_checkpoint () =
    checkpoint ();
    allocate 512
  in
  let projected = Static.of_runtime_surface surface ~checkpoint:charged_checkpoint in
  let digest contract =
    let summary = summarize_identity contract ~checkpoint ~allocate in
    Identity_summary.fingerprint summary
  in
  let expected = digest t in
  let actual = digest { t with surface = projected } in
  checkpoint ();
  Chatml_source.Digest.equal expected actual
;;

module Invocation = struct
  type contract = t
  type t = { entrypoints : string list }

  let entrypoints t = t.entrypoints
  let reject message = Error (Chatml_diagnostic.create message)

  let requirement (contract : contract) name ~checkpoint =
    List.fold contract.required_bindings ~init:None ~f:(fun found (selected, scheme) ->
      checkpoint ();
      if String.equal selected name then Some scheme else found)
    |> function
    | Some scheme -> Ok scheme
    | None -> reject "Host entrypoint was not selected at compilation"
  ;;

  let task_function = function
    | Chatml_builtin_spec.TFun (parameters, TCon ("task", [ result ])) ->
      Some (parameters, result)
    | TVar _
    | TCon _
    | TInt
    | TFloat
    | TBool
    | TString
    | TUnit
    | TArray _
    | TRef _
    | TTuple _
    | TRecord _
    | TModule _
    | TField_receiver _
    | TVariant _
    | TFun _
    | TMu _
    | TRec_var _ -> None
  ;;

  let standalone (contract : contract) ~entrypoint ~argument_count ~checkpoint =
    let open Result.Let_syntax in
    checkpoint ();
    let%bind () =
      match contract.kind with
      | Native -> Ok ()
      | Standard One_off_v1 ->
        if String.equal entrypoint "main"
        then Ok ()
        else reject "One-off host invocation requires the compiled main role"
      | Standard (Tool_v1 | Developer_tool_v1) ->
        if String.equal entrypoint "run"
        then Ok ()
        else reject "Tool host invocation requires the compiled run role"
      | Standard (Moderator_v1 | Delegated_moderator_v1) ->
        reject "Moderator compiler contracts require session invocation"
    in
    let%bind scheme = requirement contract entrypoint ~checkpoint in
    match task_function scheme with
    | None ->
      reject "Host invocation requires an explicit fixed-arity task function contract"
    | Some (parameters, _) ->
      let count =
        List.fold parameters ~init:0 ~f:(fun count _ ->
          checkpoint ();
          count + 1)
      in
      if Int.equal count argument_count
      then Ok { entrypoints = [ entrypoint ] }
      else reject "Host arguments do not match the compiled fixed function arity"
  ;;

  let matching_state state input result ~checkpoint ~allocate =
    let digest scheme =
      let encoded =
        Chatml_contract_encoding.to_string ~checkpoint ~allocate (fun writer ->
          Chatml_contract_encoding.type_scheme writer scheme)
      in
      Chatml_source.Digest.of_contents encoded ~checkpoint
    in
    let state = digest state in
    let input = digest input in
    let result = digest result in
    Chatml_source.Digest.equal state input && Chatml_source.Digest.equal state result
  ;;

  let session
        (contract : contract)
        ~initial_state
        ~on_event
        ~checkpoint
        ?(allocate = fun _ -> ())
        ()
    =
    let open Result.Let_syntax in
    checkpoint ();
    let%bind () =
      match contract.kind with
      | Standard (Moderator_v1 | Delegated_moderator_v1) ->
        if String.equal initial_state "initial_state" && String.equal on_event "on_event"
        then Ok ()
        else
          reject
            "Moderator host invocation requires the compiled initial_state/on_event roles"
      | Standard (One_off_v1 | Tool_v1 | Developer_tool_v1) ->
        reject "Standalone compiler contracts cannot instantiate a moderator session"
      | Native -> Ok ()
    in
    let%bind state = requirement contract initial_state ~checkpoint in
    let%bind event = requirement contract on_event ~checkpoint in
    match task_function event with
    | Some ([ _context; input; _event ], result) ->
      if matching_state state input result ~checkpoint ~allocate
      then Ok { entrypoints = [ initial_state; on_event ] }
      else
        reject
          "Session state contracts must agree for initial_state and on_event input/result"
    | Some (_, _) | None ->
      reject "Session on_event requires an explicit three-argument task function contract"
  ;;

  let event_state_session
        (contract : contract)
        ~initial_state
        ~on_event
        ~checkpoint
        ?(allocate = fun _ -> ())
        ()
    =
    let open Result.Let_syntax in
    checkpoint ();
    let%bind () =
      match contract.kind with
      | Native -> Ok ()
      | Standard
          ( One_off_v1
          | Tool_v1
          | Developer_tool_v1
          | Moderator_v1
          | Delegated_moderator_v1 ) ->
        reject "Event/state hooks require an authentic native compiler contract"
    in
    let%bind state = requirement contract initial_state ~checkpoint in
    let%bind event = requirement contract on_event ~checkpoint in
    match task_function event with
    | Some ([ _event; input ], result) ->
      if matching_state state input result ~checkpoint ~allocate
      then Ok { entrypoints = [ initial_state; on_event ] }
      else reject "Hook state contracts must agree for initial_state and input/result"
    | Some (_, _) | None ->
      reject "Event/state hook requires an explicit two-argument task function contract"
  ;;
end
