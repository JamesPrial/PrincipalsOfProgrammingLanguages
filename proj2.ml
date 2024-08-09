open Proj2_types;;

let getStartSymbol (g : grammar) : string =
  (* YOUR CODE GOES HERE *)
"";;

let getNonterminals (g : grammar) : string list =
  (* YOUR CODE GOES HERE *)
[];;

let getInitFirstSets (g : grammar) : symbolMap =
  (* YOUR CODE GOES HERE *)
SMap.empty;;

let getInitFollowSets (g : grammar) : symbolMap =
  (* YOUR CODE GOES HERE *)
SMap.empty;;

let rec computeFirstSet (first : symbolMap) (symbolSeq : string list) : SymbolSet.t =
  (* YOUR CODE GOES HERE *)
SymbolSet.empty;;

let recurseFirstSets (g : grammar) (first : symbolMap) firstFunc : symbolMap =
  (* YOUR CODE GOES HERE *)
SMap.empty;;

let rec getFirstSets (g : grammar) (first : symbolMap) firstFunc : symbolMap =
  (* YOUR CODE GOES HERE *)
SMap.empty;;

let rec updateFollowSet (first : symbolMap) (follow : symbolMap) (nt : string) (symbolSeq : string list) : symbolMap =
  (* YOUR CODE GOES HERE *)
SMap.empty;;

let recurseFollowSets (g : grammar) (first : symbolMap) (follow : symbolMap) followFunc : symbolMap =
  (* YOUR CODE GOES HERE *)
SMap.empty;;

let rec getFollowSets (g : grammar) (first : symbolMap) (follow : symbolMap) followFunc : symbolMap =
  (* YOUR CODE GOES HERE *)
SMap.empty;;

let rec getPredictSets (g : grammar) (first : symbolMap) (follow : symbolMap) firstFunc : ((string * string list) * SymbolSet.t) list =
  (* YOUR CODE GOES HERE *)
[];;

let tryDerive (g : grammar) (inputStr : string list) : bool =
  (* YOUR CODE GOES HERE *)
false;;

let tryDeriveTree (g : grammar) (inputStr : string list) : parseTree =
  (* YOUR CODE GOES HERE *)
Terminal "empty";;

let genParser g = tryDerive g;;
let genTreeParser g = tryDeriveTree g;;
