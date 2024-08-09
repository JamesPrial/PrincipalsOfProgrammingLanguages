open Proj2_types


let printSymbolSet set =
  Proj2_types.SymbolSet.iter (fun x -> print_endline ("Element: " ^ x)) set;;

let printSymbolMap map =
  Proj2_types.SMap.iter (fun x y -> (
    print_endline ("Key: " ^ x);
    Proj2_types.SymbolSet.iter (fun x ->
      print_endline ("  Value: " ^ x)
    ) y
  )) map;;

let printPredictSet predict =
  List.iter (fun x -> match x with
    ((nt, rhs), pset) ->
      print_endline ("Rule: " ^ nt ^ " -> " ^ (String.concat " " rhs));
      Proj2_types.SymbolSet.iter (fun str -> print_endline ("  Predict: " ^ str)) pset
  ) predict;;

let rec printTree (tree : Proj2_types.parseTree) indent =
  print_string (String.make indent ' ');
  match tree with
    Proj2_types.Terminal sym -> print_endline sym
  | Proj2_types.Nonterminal (sym, children) ->
      print_endline (sym ^ " {");
      List.iter (fun tree -> printTree tree (indent + 2)) children;
      print_endline ((String.make indent ' ') ^ "}");;

(* print string list *)
let rec print_stringlistp s = List.iter print_string s ;;

(* another way to print string list *)
let rec print_stringlistx s = match s with 
         [ ] -> print_string (" ") 
    |  h::t -> if t = [] then print_string h 
               else let _ = print_string ( h ^ ", " ) in
                    print_stringlistx t ;;

(* yet another print symbol set *)
let print_set s = print_stringlistx (SymbolSet.elements  s) ;; (* print SymbolSet *)

(* print two elements in the mapping symx->setsy *)
(* used for print_smap *)
let print_mapping symx setsy = 
  let _= print_string( " " ^ symx ^ ": ") in
    let _= print_set setsy in
     print_string ("\n") ;;

(* yet another print smap*)
let print_smap s = SMap.iter print_mapping s;;

(* used for print predict sets *)
let print_apset pset_elem = match pset_elem with
   ((x,y), s) -> 
      let _ = print_string ( " " ^ x ^ "->") in
      let _ = List.iter print_string y in
		  let _ = print_string ( ", { " ) in
      let _= print_set s in
        print_string ( " }\n" ) ;;

(* yet another print predict set *)
let print_predictset pset  = 
       List.iter print_apset pset ;;

let rec printTreeGraphvizInternal (tree : Proj2_types.parseTree) currentNodeNumber nextNumber = match tree with
  Proj2_types.Terminal sym -> print_endline ("node" ^ (string_of_int currentNodeNumber) ^ " [label=\"" ^ (Str.global_replace (Str.regexp "[\"]") "\\\"" sym) ^ "\"];");
  nextNumber
| Proj2_types.Nonterminal (sym, children) ->
    print_endline ("node" ^ (string_of_int currentNodeNumber) ^ " [label=\"" ^ (Str.global_replace (Str.regexp "[\"]") "\\\"" sym) ^ "\"];");
    if (List.length children) > 0 then (
      List.iteri (fun pos subtree -> print_endline ("node" ^ (string_of_int currentNodeNumber) ^ " -> node" ^ (string_of_int (nextNumber + pos)) ^ ";")) children;
      let (_, final_next) = List.fold_left (fun (curr,next) subtree -> (curr + 1,printTreeGraphvizInternal subtree curr next)) (nextNumber, nextNumber + (List.length children)) children in
      final_next
    ) else (
      print_endline ("node" ^ (string_of_int currentNodeNumber) ^ " -> node" ^ (string_of_int nextNumber) ^ ";");
      print_endline ("node" ^ (string_of_int nextNumber) ^ " [label=\"epsilon\"];");
      nextNumber + 1
    );;

let printTreeGraphviz (tree : Proj2_types.parseTree) =
  print_endline "digraph G {";
  ignore (printTreeGraphvizInternal tree 0 1);
  print_endline "}";;
