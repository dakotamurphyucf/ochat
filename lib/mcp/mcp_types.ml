(*--------------------------------------------------------------------------
  Model Context Protocol – Core JSON-RPC & Tool Types

  This module defines a *minimal* subset of the MCP 2025-03-26 schema that
  we need for the **client-side Phase-1 implementation** (stdio transport).

  – JSON-RPC request / response envelopes
  – Tool metadata returned by `tools/list`
  – Tool call result wrapper

  We purposefully keep the surface small – more record fields can easily be
  added in later milestones when they are required by the runtime.  All
  types derive automatic (de)serialisers via `ppx_jsonaf_conv`.

  IMPORTANT STYLE NOTE
  --------------------
  Following the guideline in the user request we use *snake_case* for OCaml
  record fields **and** supply an explicit [@key …] attribute whenever the
  MCP spec uses a different field name (typically camelCase).
---------------------------------------------------------------------------*)
open Core
module Jsonaf = Jsonaf_ext
open Jsonaf.Export

module Jsonrpc = struct
  (*--------------------------------------------------------------------
    JSON-RPC identifiers are either strings or 32-bit integers.
  --------------------------------------------------------------------*)
  module Id = struct
    type t =
      | String of string
      | Int of int
    [@@deriving bin_io, equal, compare, sexp]

    (* We use a custom [jsonaf] conversion to ensure that the ID is
       serialised as a bare JSON string or number, depending on its type.
       This is necessary to comply with the JSON-RPC 2.0 specification. *)

    (* Manual Jsonaf conversions to satisfy external JSON-RPC consumers: a
       string ID is serialised as a bare JSON string; an int ID as a JSON
       number (encoded via its decimal representation). *)

    let jsonaf_of_t = function
      | String s -> `String s
      | Int i -> `Number (Int.to_string i)
    ;;

    let t_of_jsonaf json =
      match json with
      | `String s -> String s
      | `Number num_str ->
        (try Int (int_of_string num_str) with
         | _ -> failwith "Invalid Id.Int")
      | _ -> failwith "Invalid Id.t"
    ;;

    let of_int i = Int i
    let of_string s = String s
    let ( = ) = equal
  end

  (*-------------------------- Error objects -------------------------*)
  type error_obj =
    { code : int
    ; message : string
    ; data : Jsonaf.t option [@default None] [@jsonaf_drop_if Option.is_none]
    }
  [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]

  (*-------------------------- Requests ------------------------------*)
  type request =
    { jsonrpc : string [@key "jsonrpc"]
    ; id : Id.t
    ; method_ : string [@key "method"]
    ; params : Jsonaf.t option [@default None] [@jsonaf_drop_if Option.is_none]
    }
  [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]

  let make_request ?params ~id ~method_ () : request =
    { jsonrpc = "2.0"; id; method_; params }
  ;;

  (*-------------------------- Responses -----------------------------*)
  type response =
    { jsonrpc : string [@key "jsonrpc"]
    ; id : Id.t
    ; result : Jsonaf.t option [@jsonaf.option]
    ; error : error_obj option [@jsonaf.option]
    }
  [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]

  let ok ~id result : response =
    { jsonrpc = "2.0"; id; result = Some result; error = None }
  ;;

  let error ~id ~code ~message ?data () : response =
    { jsonrpc = "2.0"; id; result = None; error = Some { code; message; data } }
  ;;

  (*-------------------------- Notifications -------------------------*)
  type notification =
    { jsonrpc : string [@key "jsonrpc"]
    ; method_ : string [@key "method"]
    ; params : Jsonaf.t option [@default None] [@jsonaf_drop_if Option.is_none]
    }
  [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]

  let notify ?params ~method_ () : notification = { jsonrpc = "2.0"; method_; params }
end

(*--------------------------------------------------------------------
  Capability negotiation – we only model what we currently require
  (`tools` capability).  Missing fields are kept as options so that JSON
  round-tripping still works if the server sends extra information.
--------------------------------------------------------------------*)
module Capability = struct
  type list_changed = bool [@@deriving jsonaf, sexp, bin_io]

  type tools_capability =
    { list_changed : list_changed option
          [@key "listChanged"] [@default None] [@jsonaf_drop_if Option.is_none]
    }
  [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]

  type prompts_capability =
    { list_changed : list_changed option
          [@key "listChanged"] [@default None] [@jsonaf_drop_if Option.is_none]
    }
  [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]

  type resources_capability =
    { subscribe : bool option [@default None] [@jsonaf_drop_if Option.is_none]
    ; list_changed : list_changed option
          [@key "listChanged"] [@default None] [@jsonaf_drop_if Option.is_none]
    }
  [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]

  type t =
    { tools : tools_capability option [@default None] [@jsonaf_drop_if Option.is_none]
    ; prompts : prompts_capability option [@default None] [@jsonaf_drop_if Option.is_none]
    ; resources : resources_capability option
          [@default None] [@jsonaf_drop_if Option.is_none]
    }
  [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]
end

(*--------------------------------------------------------------------
  Tool metadata as returned by `tools/list`
--------------------------------------------------------------------*)
module Tool = struct
  type t =
    { name : string
    ; description : string option [@default None] [@jsonaf_drop_if Option.is_none]
    ; input_schema : Jsonaf.t [@key "inputSchema"]
    }
  [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]
end

(*--------------------------------------------------------------------
  Response payload for `tools/list`
--------------------------------------------------------------------*)
module Tools_list_result = struct
  type t =
    { tools : Tool.t list
    ; next_cursor : string option
          [@key "nextCursor"] [@default None] [@jsonaf_drop_if Option.is_none]
    }
  [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]
end

(*--------------------------------------------------------------------
  Tool-call result wrapper (simplified – just text or raw Json body)
--------------------------------------------------------------------*)
module Tool_result = struct
  type content =
    | Text of string
    | Json of Jsonaf.t
    | Rich of Jsonaf.t
  [@@deriving sexp, bin_io]

  let jsonaf_of_content = function
    | Text s -> `String s
    | Json j -> j
    | Rich j -> j
  ;;

  let content_of_jsonaf = function
    | `String s -> Text s
    | `Object _ as j -> Rich j
    | other -> Json other
  ;;

  type t =
    { content : content list
    ; is_error : bool [@key "isError"] [@default false]
    }
  [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]
end

(*--------------------------------------------------------------------
  Resource metadata & contents – very small subset so that the server can
  produce valid `resources/list` and `resources/read` payloads.  We only
  include the fields that are actually used by the current implementation.
--------------------------------------------------------------------*)

module Resource = struct
  type t =
    { uri : string
    ; name : string
    ; description : string option [@default None] [@jsonaf_drop_if Option.is_none]
    ; mime_type : string option
          [@key "mimeType"] [@default None] [@jsonaf_drop_if Option.is_none]
    ; size : int option [@default None] [@jsonaf_drop_if Option.is_none]
    }
  [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]

  module Contents_text = struct
    type t =
      { uri : string
      ; mime_type : string option
            [@key "mimeType"] [@default None] [@jsonaf_drop_if Option.is_none]
      ; text : string
      }
    [@@deriving jsonaf, sexp, bin_io]
  end

  (* The more structured wrapper types used in the official MCP schema are
     omitted for now – the current server implementation hand-constructs the
     JSON objects directly.  They can be added back once higher-level
     resource helpers are required by the client code. *)
end

(*--------------------------------------------------------------------
  End of mcp_types.ml
--------------------------------------------------------------------*)
