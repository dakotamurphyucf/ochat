open! Core
module Capture = Chatml_source

module Root_id = struct
  type t = string [@@deriving compare, equal, sexp_of]

  let alphanumeric = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
    | _ -> false
  ;;

  let of_string value =
    if
      String.is_empty value
      || String.length value > 128
      || (not (alphanumeric value.[0]))
      || not
           (String.for_all value ~f:(fun c ->
              alphanumeric c || Char.equal c '_' || Char.equal c '-'))
    then Or_error.error_string "invalid instruction root ID"
    else Ok value
  ;;

  let to_string t = t
end

module Kind = struct
  type t =
    | Project_guidance
    | Skill
  [@@deriving equal, sexp_of]
end

module Scope = struct
  type t = Source.Portable_path.t option

  let root = None
  let of_path path = Some path
  let path t = t
end

module Id = struct
  type t =
    { root : Root_id.t
    ; member : Source.Portable_path.t
    }
  [@@deriving compare, equal, sexp_of]

  let root t = t.root
  let member t = t.member
end

module Limits = struct
  type t =
    { files : int
    ; file_bytes : int
    ; total_bytes : int
    ; work_steps : int
    }

  let create ~max_files ~max_file_bytes ~max_total_bytes ~max_work_steps =
    if
      max_files <= 0
      || max_files > 256
      || max_file_bytes <= 0
      || max_file_bytes > 1_048_576
      || max_total_bytes <= 0
      || max_total_bytes > 8_388_608
      || max_work_steps <= 0
      || max_work_steps > 1_000_000
    then Or_error.error_string "invalid instruction limits"
    else
      Ok
        { files = max_files
        ; file_bytes = max_file_bytes
        ; total_bytes = max_total_bytes
        ; work_steps = max_work_steps
        }
  ;;

  let standard =
    create
      ~max_files:128
      ~max_file_bytes:262_144
      ~max_total_bytes:4_194_304
      ~max_work_steps:1_000_000
    |> Or_error.ok_exn
  ;;

  let files t = t.files
  let file_bytes t = t.file_bytes
  let total_bytes t = t.total_bytes
  let work_steps t = t.work_steps
end

module Warning = struct
  type t =
    | Headerless_skill
    | Unknown_field of string
  [@@deriving equal, sexp_of]
end

module Metadata = struct
  type t =
    { name : string option
    ; description : string option
    ; annotations : (string * string) list
    ; warnings : Warning.t list
    }

  let name t = t.name
  let description t = t.description
  let annotations t = t.annotations
  let warnings t = t.warnings
  let empty = { name = None; description = None; annotations = []; warnings = [] }
end

module Metadata_error = struct
  type reason =
    | Unsupported_syntax
    | Duplicate_field
    | Missing_field
    | Invalid_value
    | Name_mismatch
    | Limit_exceeded
  [@@deriving equal, sexp_of]

  type t =
    { line : int
    ; field : string option
    ; reason : reason
    }
  [@@deriving sexp_of]
end

module Error = struct
  type t =
    | Invalid_identity
    | Invalid_encoding
    | Invalid_metadata of Metadata_error.t
    | Stale of
        { expected : Capture.Digest.t
        ; observed : Capture.Digest.t
        }
    | Limit_exceeded
    | Document of Document_schema.Error.t
    | Storage of Agent_store.Store_error.t
    | Invalid_capture of string
  [@@deriving sexp_of]
end

module Observed = struct
  type t =
    { id : Id.t
    ; kind : Kind.t
    ; digest : Capture.Digest.t
    ; byte_length : int
    ; metadata : Metadata.t
    }

  let id t = t.id
  let kind t = t.kind
  let digest t = t.digest
  let byte_length t = t.byte_length
  let metadata t = t.metadata
end

module Loaded = struct
  type t =
    { observed : Observed.t
    ; contents : string
    ; body_start_byte : int
    }

  let observed t = t.observed
  let contents t = t.contents
  let body_start_byte t = t.body_start_byte
end

exception Exhausted

let bounded limits ~checkpoint f =
  let work = ref 0 in
  let poll () =
    checkpoint ();
    if !work >= Limits.work_steps limits then raise Exhausted;
    Int.incr work
  in
  try f poll with
  | Exhausted -> Error Error.Limit_exceeded
;;

module Skill_slug = struct
  type t = string [@@deriving equal, sexp_of]

  let valid value =
    let character = function
      | 'a' .. 'z' | '0' .. '9' -> true
      | _ -> false
    in
    (not (String.is_empty value))
    && String.length value <= 64
    && character value.[0]
    && character value.[String.length value - 1]
    && String.for_all value ~f:(fun c -> character c || Char.equal c '-')
    && not (String.is_substring value ~substring:"--")
  ;;

  let of_string value =
    if valid value then Ok value else Or_error.error_string "invalid skill slug"
  ;;

  let to_string value = value
end
