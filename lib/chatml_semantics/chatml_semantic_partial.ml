open Core
module Identity = Chatml_semantic_identity
module Captured = Chatml_source
module Prepared = Chatml_prepared_program
module Sites = Chatml.Chatml_syntax_sites
module Budget = Chatml_semantic_budget
module Invalidity = Chatml_checked_graph.Invalidity
module Syntax = Chatml_checked_graph.Parsed

type t =
  { identity : Identity.t
  ; invalidity : Invalidity.t
  ; sources : Captured.t list
  ; parsed : Syntax.t list
  ; diagnostics : Chatml_diagnostic.t list
  }

let identity t = t.identity
let invalidity t = t.invalidity
let sources t = t.sources
let parsed t = t.parsed
let diagnostics t = t.diagnostics

let create
      input
      ~identity
      ~invalidity
      ~parsed
      ~(diagnostic : Chatml_diagnostic.t)
      ~budget
      ~checkpoint
  =
  List.iter parsed ~f:(fun syntax ->
    checkpoint ();
    let sites = Syntax.sites syntax in
    Budget.admit budget ~facts:1 ~edges:1 ~labels:0;
    List.iter (Sites.sites sites) ~f:(fun site ->
      checkpoint ();
      Budget.admit budget ~facts:1 ~edges:1 ~labels:(String.length (Sites.Site.name site))));
  Budget.admit budget ~facts:1 ~edges:1 ~labels:(String.length diagnostic.message);
  List.iter diagnostic.related ~f:(fun related ->
    checkpoint ();
    Budget.admit
      budget
      ~facts:1
      ~edges:1
      ~labels:(String.length (Chatml_diagnostic.Related.message related)));
  checkpoint ();
  { identity
  ; invalidity
  ; sources = Prepared.sources input
  ; parsed
  ; diagnostics = [ diagnostic ]
  }
;;
