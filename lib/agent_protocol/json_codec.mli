(** Strict JSON decoding helpers for stable protocol codecs. *)

type fields

(** [fields json] returns object fields and rejects duplicate names. *)
val fields : Jsonaf.t -> (fields, Error.t) result

(** [required fields name] returns required field [name]. *)
val required : fields -> string -> (Jsonaf.t, Error.t) result

(** [required_as fields name decode] decodes required field [name] with [decode]. *)
val required_as
  :  fields
  -> string
  -> (Jsonaf.t -> ('a, Error.t) result)
  -> ('a, Error.t) result

(** [optional fields name] returns optional field [name]. *)
val optional : fields -> string -> Jsonaf.t option

(** [optional_as fields name decode] decodes optional field [name] with [decode]. *)
val optional_as
  :  fields
  -> string
  -> (Jsonaf.t -> ('a, Error.t) result)
  -> ('a option, Error.t) result

(** [to_alist fields] returns fields in their decoded order. *)
val to_alist : fields -> (string * Jsonaf.t) list

(** [string json] decodes a JSON string. *)
val string : Jsonaf.t -> (string, Error.t) result

(** [bool json] decodes a JSON boolean. *)
val bool : Jsonaf.t -> (bool, Error.t) result

(** [list decode json] decodes a JSON array with [decode]. *)
val list : (Jsonaf.t -> ('a, Error.t) result) -> Jsonaf.t -> ('a list, Error.t) result

(** [bounded_int ~min ~max json] decodes an integer in [[min, max]]. *)
val bounded_int : min:int -> max:int -> Jsonaf.t -> (int, Error.t) result

(** [bounded_int64 ~min ~max json] decodes an integer in [[min, max]]. *)
val bounded_int64
  :  min:Core.Int64.t
  -> max:Core.Int64.t
  -> Jsonaf.t
  -> (Core.Int64.t, Error.t) result

(** [enum ~name values json] decodes a closed string enum. *)
val enum : name:string -> (string * 'a) list -> Jsonaf.t -> ('a, Error.t) result

(** [validate_required_features ~supported ~required] rejects unknown required features. *)
val validate_required_features
  :  supported:Core.String.Set.t
  -> required:string list
  -> (unit, Error.t) result

(** [validate_limits ~max_depth ~max_bytes json] enforces structural depth and
    encoded-size limits. *)
val validate_limits : max_depth:int -> max_bytes:int -> Jsonaf.t -> (unit, Error.t) result

(** [canonical json] recursively sorts object fields and rejects duplicates. *)
val canonical : Jsonaf.t -> (Jsonaf.t, Error.t) result

(** [canonical_string json] returns the stable encoding of [canonical json]. *)
val canonical_string : Jsonaf.t -> (string, Error.t) result
