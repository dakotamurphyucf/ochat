open Core

module Owner = struct
  type t = { seal : string }

  let create () = { seal = String.init 1 ~f:(fun _ -> '\000') }
end

module Id = struct
  type t =
    { owner : Owner.t
    ; index : int
    }

  let equal left right =
    phys_equal left.owner right.owner && Int.equal left.index right.index
  ;;

  let sexp_of_t t = [%sexp (t.index : int)]
end

module Role = struct
  type t =
    | Value_reference
    | Value_binder
    | Module_binder
    | Module_reference
    | Type_binder
    | Type_parameter_binder
    | Type_reference
    | Signature_value_binder
    | Field_label
    | Variant_tag
    | Constraint_name
    | Constraint_parameter_reference
  [@@deriving equal, sexp_of]
end

module Limits = struct
  type t =
    { max_sites : int
    ; max_name_bytes : int
    }

  let create ~max_sites ~max_name_bytes =
    if max_sites <= 0 || max_sites > Sys.max_array_length || max_name_bytes <= 0
    then Or_error.error_string "Syntax site limits must be positive and array-admissible"
    else Ok { max_sites; max_name_bytes }
  ;;
end

module Error = struct
  type t =
    | Site_limit
    | Name_byte_limit
    | Accounting_overflow
    | Invalid_location
  [@@deriving equal, sexp_of]
end

module Site = struct
  type t =
    { id : Id.t
    ; name : string
    ; role : Role.t
    ; location : Chatml_source.Location.t
    }

  let id t = t.id
  let name t = t.name
  let role t = t.role
  let location t = t.location
end

module Span_key = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp]
  end

  include T
  include Comparator.Make (T)
end

module At_span = struct
  type t =
    | Missing
    | Unique of Site.t
    | Ambiguous
end

type t =
  { by_span : Site.t list Map.M(Span_key).t
  ; owner : Owner.t
  ; source : Chatml_source.t
  ; sites : Site.t list
  ; index : Site.t option array
  ; name_bytes : int
  }

let source t = t.source
let sites t = t.sites
let site_count t = Array.length t.index
let name_bytes t = t.name_bytes

let find t (id : Id.t) =
  if phys_equal t.owner id.owner && id.index >= 0 && id.index < Array.length t.index
  then t.index.(id.index)
  else None
;;

let at_span t ~role ~checkpoint (span : Source.span) =
  checkpoint ();
  let rows = Map.find t.by_span (span.left.offset, span.right.offset) in
  match rows with
  | None -> At_span.Missing
  | Some rows ->
    List.fold rows ~init:At_span.Missing ~f:(fun found row ->
      checkpoint ();
      if Role.equal row.Site.role role
      then (
        match found with
        | Missing -> Unique row
        | Unique _ | Ambiguous -> Ambiguous)
      else found)
;;

module type Sink = sig
  val enabled : bool
  val add : Role.t -> name:string -> span:Source.span -> Id.t
  val completed_program_statement : end_byte:int -> unit
  val completed_program_separator : end_byte:int -> unit
end

module Disabled = struct
  let enabled = false
  let add _ ~name:_ ~span:_ = assert false
  let completed_program_statement ~end_byte:_ = ()
  let completed_program_separator ~end_byte:_ = ()
end

let collect source ~limits ~checkpoint ~f =
  Chatml_checkpoint.protect checkpoint ~f:(fun checkpoint ->
    let exception Rejected of Error.t in
    let owner = Owner.create () in
    let count = ref 0 in
    let name_bytes = ref 0 in
    let reversed = ref [] in
    let module Sink = struct
      let enabled = true
      let completed_program_statement ~end_byte:_ = ()
      let completed_program_separator ~end_byte:_ = ()

      let add role ~name ~span =
        checkpoint ();
        if !count = Int.max_value then raise (Rejected Accounting_overflow);
        if !count >= limits.Limits.max_sites then raise (Rejected Site_limit);
        let bytes = String.length name in
        if bytes > Int.max_value - !name_bytes then raise (Rejected Accounting_overflow);
        if bytes > limits.max_name_bytes - !name_bytes
        then raise (Rejected Name_byte_limit);
        let location =
          match Chatml_source.of_span source span ~checkpoint with
          | Ok location -> location
          | Error _ -> raise (Rejected Invalid_location)
        in
        let id = { Id.owner; index = !count } in
        let row = { Site.id; name; role; location } in
        reversed := row :: !reversed;
        incr count;
        name_bytes := !name_bytes + bytes;
        id
      ;;
    end
    in
    try
      let value = f (module Sink : Sink) in
      checkpoint ();
      let index = Array.create ~len:!count None in
      let rec publish acc by_span = function
        | [] -> acc, by_span
        | row :: rest ->
          checkpoint ();
          index.(row.Site.id.index) <- Some row;
          let span = Chatml_source.Location.span row.Site.location in
          let key = span.left.offset, span.right.offset in
          let by_span =
            Map.update by_span key ~f:(function
              | None -> [ row ]
              | Some rows -> row :: rows)
          in
          publish (row :: acc) by_span rest
      in
      let sites, by_span = publish [] (Map.empty (module Span_key)) !reversed in
      checkpoint ();
      Ok (value, { by_span; owner; source; sites; index; name_bytes = !name_bytes })
    with
    | Rejected error -> Error error)
;;
