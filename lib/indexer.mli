open Eio

val index
  :  sw:Switch.t
  -> dir:Fs.dir_ty Path.t
  -> dm:Domain_manager.ty Resource.t
  -> net:_ Net.t
  -> vector_db_folder:string
  -> folder_to_index:string
  -> unit
