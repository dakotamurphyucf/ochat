open Core

let engine : Highlight_tm_engine.t Lazy.t =
  lazy
    (let e = Highlight_tm_engine.create ~theme:Highlight_theme.github_dark in
     let reg = Highlight_registry.get () in
     Highlight_tm_engine.with_registry e ~registry:reg)
;;

let get () = Lazy.force engine
