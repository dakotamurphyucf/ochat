open Core

let current = ref None

let get () =
  let generation = Highlight_registry.generation () in
  match !current with
  | Some (current_generation, engine) when Int.equal current_generation generation ->
    engine
  | None | Some _ ->
    let engine =
      Highlight_tm_engine.create ~theme:Highlight_theme.github_dark
      |> Highlight_tm_engine.with_registry ~registry:(Highlight_registry.get ())
    in
    current := Some (generation, engine);
    engine
;;
