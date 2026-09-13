open Core
module Shell = Shell_access
module Management = Agent_session.Session_management

type t =
  { tool_name : string
  ; executable : string
  ; executable_sha256 : string
  ; arguments : string list
  ; operations : string list
  ; read_roots : string list
  ; environment : string list
  ; private_paths : string list
  ; max_request_bytes : int
  ; max_response_bytes : int
  ; max_requests : int
  }
[@@deriving compare, equal, sexp]

let beneath path root =
  String.equal path root
  || String.is_prefix path ~prefix:(String.rstrip root ~drop:(Char.equal '/') ^ "/")
;;

let overlaps left right = beneath left right || beneath right left

(* Conservatively include both supported backends' implicit read roots. These
   are platform support files, not additional operator grants. Protecting a host
   credential under any of them makes this helper configuration inadmissible. *)
let implicit_roots = Shell.Backend.request_channel_implicit_read_roots
let canonical ~fs path = Shell.Path_util.canonical ~fs path

let rec canonical_destination ~fs path =
  match Eio.Path.kind ~follow:false Eio.Path.(fs / path) with
  | `Not_found ->
    let parent = Filename.dirname path in
    (match String.equal parent path with
     | true -> failwith "cannot resolve helper policy path"
     | false ->
       Filename.concat (canonical_destination ~fs parent) (Filename.basename path))
  | _ -> canonical ~fs path
;;

let capture ~fs path = path, canonical_destination ~fs path

let grant ~env ~protected_paths policy =
  let open Result.Let_syntax in
  let fs = Eio.Stdenv.fs env in
  let%bind allowed =
    Result.all
      (List.map policy.operations ~f:(fun name ->
         Management.operation_of_json (`String name)
         |> Result.map_error ~f:(fun _ -> "unknown session helper operation")))
  in
  let executable = canonical ~fs policy.executable in
  let roots = List.map policy.read_roots ~f:(canonical ~fs) in
  let protected = List.map (protected_paths @ policy.private_paths) ~f:(capture ~fs) in
  let implicit = List.map implicit_roots ~f:(canonical_destination ~fs) in
  let safe_boundary () =
    List.for_all protected ~f:(fun (source, pinned) ->
      String.equal (canonical_destination ~fs source) pinned
      && not (List.exists (roots @ implicit) ~f:(overlaps pinned)))
  in
  let%bind () =
    match roots, safe_boundary () with
    | _ :: _, true -> Ok ()
    | _ -> Error "session helper read roots overlap protected host paths or are empty"
  in
  let paths = (policy.executable, executable) :: List.zip_exn policy.read_roots roots in
  let authorize (context : Shell.Context.t) =
    let caps = context.capabilities in
    let valid =
      String.equal context.executable.canonical_path executable
      && String.equal context.executable.fingerprint.sha256 policy.executable_sha256
      && List.equal String.equal context.command.arguments policy.arguments
      && (match context.request_kind with
          | Structured -> true
          | Script_file | Raw_shell -> false)
      && List.for_all paths ~f:(fun (source, pinned) ->
        String.equal (canonical ~fs source) pinned)
      && safe_boundary ()
      && List.exists roots ~f:(beneath (canonical ~fs context.cwd))
      && List.for_all caps.read_roots ~f:(fun actual ->
        List.exists roots ~f:(beneath (canonical ~fs actual)))
      && List.is_empty caps.write_roots
      && Array.for_all context.environment ~f:(fun entry ->
        List.mem policy.environment entry ~equal:String.equal)
    in
    match valid with
    | true -> Ok ()
    | false -> Error "session helper execution differs from its operator grant"
  in
  let authorize context =
    try authorize context with
    | Eio.Cancel.Cancelled _ as exn -> raise exn
    | _ -> Error "session helper policy paths are unavailable"
  in
  let policy_revision =
    [%sexp
      ("ochat.configured-session-helper.v1" : string)
    , (policy : t)
    , (paths : (string * string) list)
    , (protected : (string * string) list)
    , (implicit : string list)]
    |> Sexp.to_string_mach
    |> Chatmd_shell_spec.Source_ref.digest
  in
  Agent_session.Session_management_channel.grant
    ~tool_name:policy.tool_name
    ~policy_revision
    ~allowed
    ~limits:
      { max_request_bytes = policy.max_request_bytes
      ; max_response_bytes = policy.max_response_bytes
      ; max_requests = policy.max_requests
      }
    ~authorize
;;

let grants ~env ~protected_paths policies =
  match
    List.find_a_dup policies ~compare:(fun a b -> String.compare a.tool_name b.tool_name)
  with
  | Some _ -> Error "duplicate session helper tool grant"
  | None ->
    (try Result.all (List.map policies ~f:(grant ~env ~protected_paths)) with
     | Eio.Cancel.Cancelled _ as exn -> raise exn
     | _ -> Error "session helper policy paths are unavailable")
;;
