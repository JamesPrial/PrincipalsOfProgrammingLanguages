open Proj2_types
val getStartSymbol : grammar -> string
val getNonterminals : grammar -> string list
val getInitFirstSets : grammar -> symbolMap
val getInitFollowSets : grammar -> symbolMap
val computeFirstSet : symbolMap -> string list -> SymbolSet.t
val recurseFirstSets :
  grammar ->
  symbolMap -> (symbolMap -> string list -> SymbolSet.t) -> symbolMap
val getFirstSets :
  grammar ->
  symbolMap -> (symbolMap -> string list -> SymbolSet.t) -> symbolMap
val updateFollowSet :
  symbolMap -> symbolMap -> string -> string list -> symbolMap
val recurseFollowSets :
  grammar ->
  symbolMap ->
  symbolMap ->
  (symbolMap -> symbolMap -> string -> string list -> symbolMap) -> symbolMap
val getFollowSets :
  grammar ->
  symbolMap ->
  symbolMap ->
  (symbolMap -> symbolMap -> string -> string list -> symbolMap) -> symbolMap
val getPredictSets :
  grammar ->
  symbolMap ->
  symbolMap ->
  (symbolMap -> string list -> SymbolSet.t) ->
  ((string * string list) * SymbolSet.t) list
val tryDerive :
  grammar -> string list -> bool
val tryDeriveTree :
  grammar -> string list -> parseTree
val genParser :
  grammar -> string list -> bool
val genTreeParser :
  grammar -> string list -> parseTree
