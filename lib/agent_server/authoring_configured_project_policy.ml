open Core
module Configuration = Authoring_project_access_configuration
module Sources = Authoring_source_host
module Contract = Chatml.Chatml_compiler_contract
module Actor = Operator_authorization
module Protocol_selection = Agent_protocol.Authoring.Context.Selection

exception Limit_exceeded

let max_member_identity_bytes = 16 * 1024 * 1024

module Error = struct
  type t =
    | Invalid_configuration
    | Source_host_closed
    | Foreign_domain
    | Root_binding_mismatch
    | Contract_limit
  [@@deriving sexp_of]
end

type grant =
  { index : int
  ; principal_id : Agent_protocol.Id.Principal.t
  ; authentication_kind : string
  }

type selection =
  { index : int
  ; project : Source.Portable_path.t
  ; request : Protocol_selection.t
  ; contract : Contract.t
  }

type t =
  { issuer : Project_source_host.Identity.t
  ; grants : grant list
  ; selections : selection list
  }

let issued_by t host =
  Project_source_host.Identity.equal t.issuer (Project_source_host.identity host)
;;

let selection_request = function
  | Configuration.Selection.Library { environment } ->
    Protocol_selection.create ~target:Protocol_selection.Library ~environment
  | Entry { name; environment } ->
    Protocol_selection.create ~target:(Protocol_selection.Entry name) ~environment
;;

let bind
      host
      ~configuration
      ~project_sources
      ~max_contract_identity_bytes
      ~max_work_steps
      ~checkpoint
  =
  let open Result.Let_syntax in
  if max_contract_identity_bytes <= 0 || max_work_steps <= 0
  then Error Error.Contract_limit
  else (
    let%bind () =
      Configuration.validate configuration ~project_sources
      |> Result.map_error ~f:(fun _ -> Error.Invalid_configuration)
    in
    let work = ref 0 in
    let remaining_identity_bytes = ref max_contract_identity_bytes in
    let poll () =
      checkpoint ();
      if !work >= max_work_steps then raise Limit_exceeded;
      Int.incr work
    in
    let within_host ~roots =
      let rec same_roots opened configured =
        match opened, configured with
        | [], [] -> true
        | (name, _) :: opened, configured_name :: configured ->
          poll ();
          String.equal name configured_name && same_roots opened configured
        | [], _ :: _ | _ :: _, [] -> false
      in
      let%bind () =
        if same_roots roots project_sources.external_roots
        then Ok ()
        else Error Error.Root_binding_mismatch
      in
      let root_index root =
        List.find_mapi roots ~f:(fun index (name, _) ->
          poll ();
          if String.equal name root then Some index else None)
      in
      let%bind grants =
        List.fold_result configuration.read_grants ~init:[] ~f:(fun acc grant ->
          poll ();
          match root_index grant.root with
          | None -> Error Error.Root_binding_mismatch
          | Some index ->
            Ok
              ({ index
               ; principal_id = grant.principal_id
               ; authentication_kind = grant.authentication_kind
               }
               :: acc))
        |> Result.map ~f:List.rev
      in
      let contracts = ref [] in
      let contract_for target =
        match
          List.find !contracts ~f:(fun (key, _) -> Contract.Target.equal key target)
        with
        | Some (_, contract) -> Ok contract
        | None ->
          poll ();
          let contract = Contract.for_target target ~checkpoint:poll in
          let remaining_member_bytes = ref max_member_identity_bytes in
          let allocate bytes =
            if
              bytes < 0
              || bytes > !remaining_identity_bytes
              || bytes > !remaining_member_bytes
            then raise Limit_exceeded;
            remaining_member_bytes := !remaining_member_bytes - bytes;
            remaining_identity_bytes := !remaining_identity_bytes - bytes
          in
          let encoded = Contract.input_bytes contract ~checkpoint:poll ~allocate in
          if encoded > max_contract_identity_bytes || encoded > max_member_identity_bytes
          then Error Error.Contract_limit
          else (
            contracts := (target, contract) :: !contracts;
            Ok contract)
      in
      let%map selections =
        List.map configuration.selections ~f:(fun selected ->
          let%bind index =
            match root_index selected.root with
            | None -> Error Error.Root_binding_mismatch
            | Some index -> Ok index
          in
          let%bind project =
            Source.Portable_path.create selected.project
            |> Result.map_error ~f:(fun _ -> Error.Invalid_configuration)
          in
          let%bind request =
            selection_request selected.request
            |> Result.map_error ~f:(fun _ -> Error.Invalid_configuration)
          in
          let%map contract = contract_for selected.target in
          { index; project; request; contract })
        |> Result.all
      in
      { issuer = Project_source_host.identity host; grants; selections }
    in
    let bounded () =
      match
        Project_source_host.with_authoring
          host
          ~f:(fun ~roots ~policy:_ ~limits:_ ~max_request_bytes:_ -> within_host ~roots)
      with
      | Ok result -> result
      | Error Project_source_host.Error.Closed -> Error Error.Source_host_closed
      | Error Foreign_domain -> Error Error.Foreign_domain
      | Error (Invalid_configuration _)
      | Error Filesystem
      | Error Authorization_header_too_large
      | Error (External_root _)
      | Error (Git _) -> Error Error.Invalid_configuration
    in
    try bounded () with
    | Limit_exceeded -> Error Error.Contract_limit)
;;

let admitted_actor t actor ~index =
  Actor.is_current actor
  &&
  let principal = Actor.principal actor in
  List.exists t.grants ~f:(fun grant ->
    Int.equal grant.index index
    && Agent_protocol.Id.Principal.equal grant.principal_id principal.id
    && String.equal grant.authentication_kind principal.authentication_kind)
;;

let read_policy t =
  Sources.Read_policy.create ~authorize:(fun actor ~root_index ->
    admitted_actor t actor ~index:root_index)
;;

let project_capture_authorized t ~original_actor ~actor capture =
  match
    Sources.Document_context.Project_capture.root_index_if_bound
      capture
      ~issuer:t.issuer
      ~original_actor
  with
  | None -> false
  | Some index -> admitted_actor t original_actor ~index && admitted_actor t actor ~index
;;

let project_contract t actor ~root ~project ~selection =
  let index = Sources.Root.index root in
  if not (admitted_actor t actor ~index)
  then None
  else
    Option.map
      (List.find t.selections ~f:(fun entry ->
         Int.equal entry.index index
         && Source.Portable_path.equal entry.project project
         && Protocol_selection.equal entry.request selection))
      ~f:(fun entry -> entry.contract)
;;
