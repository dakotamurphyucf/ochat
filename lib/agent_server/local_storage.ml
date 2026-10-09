open! Core

module Root = struct
  type t =
    { path : string
    ; name : string option
    }

  let create ?name ~path () =
    if String.is_empty path || not (Filename.is_absolute path)
    then
      Error (Agent_protocol.Error.invalid_request "local storage root must be absolute")
    else if String.exists path ~f:(Char.equal '\000')
    then Error (Agent_protocol.Error.invalid_request "local storage root contains NUL")
    else (
      match name with
      | Some name
        when String.is_empty name
             || String.length name > 128
             || String.exists name ~f:(fun character ->
               not
                 (Char.is_alphanum character
                  || Char.equal character '-'
                  || Char.equal character '_'
                  || Char.equal character '.')) ->
        Error
          (Agent_protocol.Error.invalid_request
             "local storage name must contain 1–128 ASCII letters, digits, '.', '_' or \
              '-'")
      | None | Some _ -> Ok { path; name })
  ;;

  let path t = t.path
  let name t = t.name
end

type t =
  | Default
  | Durable of Root.t
  | Transient

let durable_root t ~home =
  match t with
  | Transient -> Ok None
  | Durable root -> Ok (Some root)
  | Default ->
    (match home with
     | None ->
       Error
         (Agent_protocol.Error.invalid_request
            "default local storage requires HOME or an explicit durable root")
     | Some home ->
       let open Result.Let_syntax in
       let%bind _ = Root.create ~path:home () in
       let%map root = Root.create ~path:(Filename.concat home ".ochat/agent-store") () in
       Some root)
;;
