open! Core

val analyze
  :  Hook_worker.t
  -> Shell_access.Context.t
  -> (Shell_access.Analyzer.result, Hook_worker.error) result
