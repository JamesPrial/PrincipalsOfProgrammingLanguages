type grammar = string * (string * string list) list;;

module SymbolSet = Set.Make(String);;
module SMap = Map.Make(String);;
type symbolMap = SymbolSet.t SMap.t;;

type parseTree =
| Terminal of string
| Nonterminal of string * parseTree list;;
