open! Core
module P = Protocol_error

module Source_mode = struct
  type t =
    | File
    | Captured
  [@@deriving equal, sexp_of]
end

type t =
  { original : Jsonaf.t
  ; prompt : string
  ; input : string
  ; is_local : bool
  ; source_mode : Source_mode.t
  ; history_compaction : bool option
  ; nested_prompt_id : string option
  }

let recipe_name = "agent_prompt_v1"
let original t = t.original
let prompt t = t.prompt
let input t = t.input
let is_local t = t.is_local
let source_mode t = t.source_mode
let history_compaction t = t.history_compaction
let nested_prompt_id t = t.nested_prompt_id

let json_object_find fields key =
  List.find_map fields ~f:(fun (name, value) ->
    if String.equal name key then Some value else None)
;;

let expect_string_field fields key =
  match json_object_find fields key with
  | Some (`String value) -> Ok value
  | Some _ -> Error (Printf.sprintf "%s: field %S must be a string" recipe_name key)
  | None -> Error (Printf.sprintf "%s: missing field %S" recipe_name key)
;;

let expect_bool_field_opt fields key =
  match json_object_find fields key with
  | None -> Ok None
  | Some `True -> Ok (Some true)
  | Some `False -> Ok (Some false)
  | Some _ -> Error (Printf.sprintf "field %S must be a bool when present" key)
;;

let expect_string_field_opt fields key =
  match json_object_find fields key with
  | None -> Ok None
  | Some (`String value) -> Ok (Some value)
  | Some _ -> Error (Printf.sprintf "field %S must be a string when present" key)
;;

let decode original =
  match original with
  | `Object fields ->
    let open Result.Let_syntax in
    let%bind prompt = expect_string_field fields "prompt" in
    let%bind input = expect_string_field fields "input" in
    let%bind is_local =
      match json_object_find fields "is_local" with
      | Some `True -> Ok true
      | Some `False -> Ok false
      | Some _ ->
        Error (Printf.sprintf "%s: field %S must be a bool" recipe_name "is_local")
      | None -> Ok false
    in
    let%bind source_mode =
      match json_object_find fields "source_mode" with
      | None | Some (`String "file") -> Ok Source_mode.File
      | Some (`String "captured") when is_local -> Ok Source_mode.Captured
      | Some (`String "captured") ->
        Error (Printf.sprintf "%s: captured source requires is_local=true" recipe_name)
      | Some (`String _) ->
        Error (Printf.sprintf "%s: unsupported source_mode" recipe_name)
      | Some _ ->
        Error (Printf.sprintf "%s: field %S must be a string" recipe_name "source_mode")
    in
    let%bind history_compaction = expect_bool_field_opt fields "history_compaction" in
    let%map nested_prompt_id = expect_string_field_opt fields "session_id" in
    { original
    ; prompt
    ; input
    ; is_local
    ; source_mode
    ; history_compaction
    ; nested_prompt_id
    }
  | _ -> Error (Printf.sprintf "%s: payload must be an object" recipe_name)
;;

let admit original ~limits =
  let open Result.Let_syntax in
  let%bind () =
    Document_schema.Json.validate ~limits original
    |> Result.map_error ~f:(function
      | Document_schema.Error.Limit_exceeded _ ->
        P.create
          P.Resource_limit
          ~message:"Model request exceeds its admitted JSON limits."
          ~retryable:false
          ()
      | _ -> P.invalid_request "Model request JSON is invalid.")
  in
  decode original |> Result.map_error ~f:P.invalid_request
;;
