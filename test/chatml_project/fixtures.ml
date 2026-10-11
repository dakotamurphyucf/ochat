open Core
module M = Selection_model

type t =
  { label : string
  ; json : string
  ; model : M.t
  }

(* Handwritten wire declarations, paired with independently authored semantics.
   This tests pure selection, not discovery, dependency loading or execution. *)
let selection =
  [ { label = "implicit own default"
    ; json =
        {|{"schema_version":1,"namespace":"implicit","entries":{"main":{"source":"main.chatml","target":"one_off_v1"}}}|}
    ; model =
        { environments = [ "default" ]
        ; package_default = Some "default"
        ; entry_environment = None
        ; files = []
        }
    }
  ; { label = "request over entry over explicit default"
    ; json =
        {|{"schema_version":1,"namespace":"explicit","environments":{"default":{},"entry":{},"app":{}},"default_environment":"app","entries":{"main":{"source":"main.chatml","target":"one_off_v1","environment":"entry"}}}|}
    ; model =
        { environments = [ "default"; "entry"; "app" ]
        ; package_default = Some "app"
        ; entry_environment = Some "entry"
        ; files = []
        }
    }
  ; { label = "entry over literal default"
    ; json =
        {|{"schema_version":1,"namespace":"literal","environments":{"default":{},"entry":{},"app":{}},"entries":{"main":{"source":"main.chatml","target":"one_off_v1","environment":"entry"}}}|}
    ; model =
        { environments = [ "default"; "entry"; "app" ]
        ; package_default = Some "default"
        ; entry_environment = Some "entry"
        ; files = []
        }
    }
  ; { label = "explicit default without entry selector"
    ; json =
        {|{"schema_version":1,"namespace":"app","environments":{"default":{},"entry":{},"app":{}},"default_environment":"app","entries":{"main":{"source":"main.chatml","target":"one_off_v1"}}}|}
    ; model =
        { environments = [ "default"; "entry"; "app" ]
        ; package_default = Some "app"
        ; entry_environment = None
        ; files = []
        }
    }
  ; { label = "entry without package default"
    ; json =
        {|{"schema_version":1,"namespace":"no-default","environments":{"entry":{},"app":{}},"entries":{"main":{"source":"main.chatml","target":"one_off_v1","environment":"entry"}}}|}
    ; model =
        { environments = [ "entry"; "app" ]
        ; package_default = None
        ; entry_environment = Some "entry"
        ; files = []
        }
    }
  ; { label = "empty environments are not an implicit default"
    ; json =
        {|{"schema_version":1,"namespace":"empty","environments":{},"entries":{"main":{"source":"main.chatml","target":"one_off_v1"}}}|}
    ; model =
        { environments = []
        ; package_default = None
        ; entry_environment = None
        ; files = []
        }
    }
  ]
;;

let replacement =
  {|{
  "schema_version": 1,
  "namespace": "app",
  "environments": {
    "app": {"modules":{"Root":"self.Root"},"requires":{"compiler":"root-family","targets":["root-target"]}},
    "isolated": {"modules":{"Only":"self.Other"},"requires":{"compiler":"other-family","targets":["other-target"]}},
    "none": {"modules":{}},
    "catalog": {}
  },
  "default_environment": "app",
  "file_environments": {"src/a.chatml":"isolated","src/a.chatmli":"none","src/all.chatml":"catalog"},
  "entries": {"main":{"source":"main.chatml","target":"one_off_v1"}}
}|}
;;
