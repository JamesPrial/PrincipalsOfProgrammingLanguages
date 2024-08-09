open Proj2_types;;

let getStartSymbol (g : grammar) : string =
	match g with
	(s, _) -> s
;;

let rec extractNonTerms l: string list = 
match l with
[] -> ([]: string list)
|h::t -> match h with
(n, _) -> n::(extractNonTerms t)
;;

let getNonterminals (g : grammar) : string list =
  match g with
  (_, l) -> extractNonTerms l
;;

let rec recInit l: symbolMap =
	match l with
	[] -> SMap.empty
	|h::t -> SMap.add h (SymbolSet.empty) (recInit t)
;;

let getInitFirstSets (g : grammar) : symbolMap =
  let l = getNonterminals g in
  recInit l
;;

let getInitFollowSets (g : grammar) : symbolMap =
    let l = getNonterminals g in
    SMap.add (getStartSymbol g) (SymbolSet.singleton "eof") (recInit l)
;;

let rec computeFirstSet (first : symbolMap) (symbolSeq : string list) : SymbolSet.t =
	match symbolSeq with
		[] -> SymbolSet.singleton "eps"
		| h::t -> if (SMap.mem h first) then
			let sym = (SMap.find h first) in
		    if (SymbolSet.mem "eps" sym) then (SymbolSet.union (SymbolSet.remove "eps" sym) (computeFirstSet first t)) else sym
		else (SymbolSet.singleton h)
;;

let foldFunc (a : symbolMap) (rule: string * string list)  firstFunc : symbolMap =
	match rule with
		(symbol, str) -> let firstSet = (firstFunc a str) in
			if (SMap.mem symbol a) then (SMap.add symbol (SymbolSet.union (SMap.find symbol a) (firstSet)) a) else (SMap.add symbol firstSet a)
;;

let rec fold f a firstFunc l =
	match l with
	[] -> a
	| h::t -> fold f (f a h firstFunc) firstFunc t;
;;

let recurseFirstSets (g : grammar) (first : symbolMap) firstFunc : symbolMap =
	match g with
		(_, l)-> fold foldFunc first firstFunc l
;;

let rec getFirstSets (g : grammar) (first : symbolMap) firstFunc : symbolMap =
	let (updated : symbolMap) = (recurseFirstSets g first firstFunc) in
		if (SMap.equal (SymbolSet.equal) first updated) then first else (getFirstSets g updated firstFunc)
;;
let rec eps first follow nt symbolSeq acc =
	match symbolSeq with
	[] -> (SymbolSet.union acc (SMap.find nt follow))
	|h::t -> if(SMap.mem h first) then (if (SymbolSet.mem "eps" (SMap.find h first)) then eps first follow nt t (SymbolSet.union acc (SymbolSet.remove "eps" (SMap.find h first) ) ) else (SymbolSet.union acc (SMap.find h first)) ) else (SymbolSet.add h acc)
;;
let rec updateFollowSet (first : symbolMap) (follow : symbolMap) (nt : string) (symbolSeq : string list) : symbolMap =
	match symbolSeq with
		[] -> follow
		| h::t -> if (SMap.mem h first) then (updateFollowSet first (SMap.add h (SymbolSet.union (SMap.find h follow) (eps first follow nt t (SMap.find h follow)) ) follow) nt t) else (updateFollowSet first follow nt t)
;;

let rec foldFollow f first (a: symbolMap) l =
	match l with
	[] -> a
	| h::t -> match h with
		(nt, seq) -> foldFollow f first (f first a nt seq) t

let recurseFollowSets (g : grammar) (first : symbolMap) (follow : symbolMap) followFunc : symbolMap =
	match g with
	(_, l) -> foldFollow followFunc first follow l
;;

let rec getFollowSets (g : grammar) (first : symbolMap) (follow : symbolMap) followFunc : symbolMap =
  let (updated: symbolMap) = (recurseFollowSets g first follow followFunc) in
  	if (SMap.equal (SymbolSet.equal) follow updated) then follow else (getFollowSets g first updated followFunc)
;;

let rec predict rules (first : symbolMap) (follow : symbolMap) firstFunc : ((string * string list) * SymbolSet.t) list =
	match rules with
		[] -> []
		| h::t -> match h with
			(nt, seq) -> let firstSet = (firstFunc first seq) in
				if (SymbolSet.mem "eps" firstSet) then (h, (SymbolSet.union (SymbolSet.remove "eps" firstSet) (SMap.find nt follow) ) )::(predict t first follow firstFunc) else (h, firstSet)::(predict t first follow firstFunc)
;;

let rec getPredictSets (g : grammar) (first : symbolMap) (follow : symbolMap) firstFunc : ((string * string list) * SymbolSet.t) list =
	match g with
		(_, l) -> predict l first follow firstFunc
;;

let rec foldRules (predictSets: ((string * string list) * SymbolSet.t) list) (nt: string) (find:string): ((string * string list) * SymbolSet.t) = 
	match predictSets with
		[] -> (("eps", []), SymbolSet.empty);
		| h::t -> match h with
		((nonTerm, seq),symSet) -> if(nt = nonTerm) then (if (SymbolSet.mem find symSet) then ((nonTerm, seq), symSet) else (foldRules t nt find) ) else (foldRules t nt find)
;;

let rec parser (inputStr: string list) (parsing: string list) (firstSets :symbolMap) (predictSets: ((string * string list) * SymbolSet.t) list): ((string list) * (string list)) =
	match parsing with 
		[] -> (inputStr, parsing)
		|parseHead::[]->( match inputStr with 
			[] -> if (SMap.mem parseHead firstSets) then (match (foldRules predictSets parseHead "eof") with
				((nt,seq), sy) -> if (nt = "eps") then (inputStr, parsing) else ([],[])
			) else (parsing, inputStr) 
			|inputHead::inputTail-> (**) if (parseHead = inputHead) then (parser inputTail [] firstSets predictSets) else (if (SMap.mem parseHead firstSets) then
					(match (foldRules predictSets parseHead inputHead) with ((nt, seq), symSet) -> if (nt = "eps") then (["eps"],["eps"]) else(

						match (parser (inputStr) (seq) firstSets predictSets) with
							([],[]) ->([], [])
							|(["eps"], ["eps"]) -> (["eps"],["eps"])
							|(inp, pars) -> parser inp pars firstSets predictSets 
					)) else ((["eps"], ["eps"]))
				)  
			(**))
		|parseHead::parseTail -> match inputStr with
			[] ->(inputStr, parsing)
			|inputHead::inputTail -> if (parseHead = inputHead) then (parser inputTail parseTail firstSets predictSets) else (if (SMap.mem parseHead firstSets) then
					(match (foldRules predictSets parseHead inputHead) with ((nt, seq), symSet) -> if (nt = "eps") then (["eps"],["eps"]) else(

						match (parser (inputStr) (seq@parseTail) firstSets predictSets) with
							([],[]) ->([], [])
							|(["eps"], ["eps"]) -> (["eps"],["eps"])
							|(inp, pars) -> parser inp pars firstSets predictSets
					)) else ((["eps"], ["eps"]))
				) 
;;

let rec strMatch (str1: string list) (str2: string list) : bool =
	match str1 with
		[] -> (match str2 with
			[] -> true
			|h2::t2 -> false)
		|h1::t1 ->	(match str2 with
			[] -> false
			|h2::t2 -> if(h1 = h2) then (strMatch t1 t2) else false
		)
;;


let tryDerive (g : grammar) (inputStr : string list) : bool =
	let firstSets = (getFirstSets g (getInitFirstSets g) computeFirstSet) in
	let followSets = (getFollowSets g firstSets (getInitFollowSets g) updateFollowSet) in
	let predictSets = (getPredictSets g firstSets followSets computeFirstSet) in
	match g with
		(start, rules) -> match (parser inputStr [start] firstSets predictSets) with
			([],[]) -> true
			|(["eps"], ["eps"]) -> false
			|(a,b) ->  false
			
			
;;

let tryDeriveTree (g : grammar) (inputStr : string list) : parseTree =
  (* YOUR CODE GOES HERE *)
Terminal "empty";;

let genParser g = tryDerive g;;
let genTreeParser g = tryDeriveTree g;;
