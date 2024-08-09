open Proj2_types
val print_stringlistx : string list -> unit
val print_stringlistp : string list -> unit
val print_set : SymbolSet.t -> unit
val print_mapping : string -> SymbolSet.t -> unit
val print_smap : SymbolSet.t SMap.t -> unit
val print_apset : (string * string list) * SymbolSet.t -> unit
val print_predictset : ((string * string list) * SymbolSet.t) list -> unit
val printSymbolSet : Proj2_types.SymbolSet.t -> unit
val printSymbolMap : Proj2_types.SymbolSet.t Proj2_types.SMap.t -> unit
val printPredictSet :
  ((string * string list) * Proj2_types.SymbolSet.t) list -> unit
val printTree : Proj2_types.parseTree -> int -> unit

