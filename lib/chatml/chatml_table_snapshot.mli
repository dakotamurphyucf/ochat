open Chatml_lang

(** Validate the structural string-keyed table and copy its entries in their
    current insertion order. Reject malformed entries and duplicate keys with
    [Failure]. The outer array and entry records are fresh; payloads are shared.
    Expected O(n) time and O(n) space. The builtin owner reserves allocation. *)
val to_array : value -> value array
