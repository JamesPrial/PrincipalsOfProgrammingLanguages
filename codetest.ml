#load "proj2_types.cmo"
#load "proj2.cmo"
#load "proj2_driver.cmo"

open Proj2_types
open Proj2
open Proj2_driver


#use "testcases_simple/testcase1.ml" (* load test case file *);;


(*convert the test string list into symbol map *)
let rec convert2smap (x: (string*string list) list)= 
    match x with
      [ ] -> SMap.empty 
     | (s, slist)::t -> SMap.add s ( SymbolSet.of_list slist) (convert2smap t)
;;


(*convert the test string list into predict set *)
let rec convert2pset (x: ((string * string list) * string list) list ) = 
     match x with 
    [ ] -> [ ]
   | ( (s, slist), slist2) :: t -> ( (s, slist), SymbolSet.of_list slist2 ) :: convert2pset t  
;; 


(*compare symbol set*)
let compareSymbolSet x y = 
   SymbolSet.equal x y ;;
      
   
(*compare smap *)
let compareSMap x y =
   SMap.equal ( fun x y -> SymbolSet.equal x  y ) x y ;;


(*compare one item in predict set*)
let comparePSet_elem ((x1, x2), x3) ((y1, y2), y3) : bool =
        x1 = y1 && x2 = y2 && (compareSymbolSet x3 y3) ;;


(*compare two predict sets*)
let rec comparePSet x y: bool =
     match x with [] -> (if y = [] then true else false)
               | h::t -> 
            ( match y with [] -> false 
                | y1::y2 -> ( (comparePSet_elem h y1) && (comparePSet t y2)) );;


(*---------------compare two parse trees-------------  *)
(* this part is for testing the extra credit component *)
(* DO NOT panic if your test fails here.               *)
(* You can comment it out if you want to.              *)
let rec compare_tree tree1 tree2 = match tree1 with
  Proj2_types.Terminal sym1 -> (match tree2 with Proj2_types.Terminal sym2 -> String.equal sym1 sym2 | _ -> false)
| Proj2_types.Nonterminal (sym1,lst1) -> (match tree2 with
    Proj2_types.Nonterminal (sym2,lst2) -> (
      if (not (String.equal sym1 sym2)) then false
      else if (List.length lst1) != (List.length lst2) then false
      else compare_tree_list lst1 lst2
    )
  | _ -> false
  )
and compare_tree_list lst1 lst2 = match lst1 with
  [] -> true
| e1 :: rest1 -> (match lst2 with
    [] -> raise (Invalid_argument "This should not happen")
  | e2 :: rest2 -> compare_tree e1 e2 && compare_tree_list rest1 rest2
  );;


let ret_3sets g = 
(* return first set, follow set, and predict sets*)
  let ifs = getInitFirstSets g in  
  let fs = getFirstSets g ifs computeFirstSet in
  let ifl = getInitFollowSets g in
  let ls = getFollowSets g fs ifl updateFollowSet in 
  let pset = getPredictSets g fs ls computeFirstSet in
     (fs, ls, pset) ;; 
      

(* convert reference input strings into first, follow, and predict set *)
let fs_ref = convert2smap fs_refstring ;;
let ls_ref = convert2smap ls_refstring ;; 
let pset_ref = convert2pset pset_refstring ;;


let (fs, ls, pset) = ret_3sets g;;

let fscomp = compareSMap fs fs_ref;; 
let flcomp = compareSMap ls ls_ref;; 
let pdsetcomp = comparePSet pset_ref pset;;

print_endline "=========FIRST Sets============";;
print_smap fs ;;

if fscomp then print_string ("FIRST set passed\n===============================\n")  
          else print_string ("FIRST set failed\n===============================\n") ;;


print_endline "=========FOLLOW Sets===========";;
print_smap ls ;;

if flcomp then print_string ("FOLLOW set passed\n==============================\n")  
          else print_string ("FOLLOW set failed\n==============================\n") ;;

print_endline "========PREDICT Sets===========";;
print_predictset pset ;;


if pdsetcomp then print_string ("PREDICT set passed\n==============================\n")  
          else print_string ("PREDICT set failed\n==============================\n") ;;


let parser = genParser g;;

let check_correctinput instr = 
 let ci1 = parser instr in
   let _ = print_string "Parsing test for correct string: " in
      let _ = print_stringlistp instr in
             if ci1 then print_string ",  passed\n" 
                    else print_string ",  failed\n" ;;

check_correctinput correct_inputstring1 ;;
check_correctinput correct_inputstring2 ;;
check_correctinput correct_inputstring3 ;;
check_correctinput correct_inputstring4 ;;
check_correctinput correct_inputstring5 ;;


let check_incorrectinput instr = 
 let ici1 = parser instr in
   let _ = print_string "Parsing test for incorrect string: " in
      let _ = print_stringlistp instr in
             if not ici1 then print_string ",  passed\n" 
                    else print_string ",  failed\n" ;;

check_incorrectinput incorrect_inputstring1;;
check_incorrectinput incorrect_inputstring2;;
check_incorrectinput incorrect_inputstring3;;
check_incorrectinput incorrect_inputstring4;;
check_incorrectinput incorrect_inputstring5;;


print_string "==============================\n";;


let treeParser = genTreeParser g;;

let tree1 = treeParser correct_inputstring1 ;;
let tree2 = treeParser correct_inputstring2 ;;
let tree3 = treeParser correct_inputstring3 ;;
let tree4 = treeParser correct_inputstring4 ;;
let tree5 = treeParser correct_inputstring5 ;;

let check_tree tree1 tree2 instr = 
let r = compare_tree tree1 tree2 in
   let _ = print_string "Parse tree for string: " in
      let _ = print_stringlistp instr in
             if  r then print_string ",  passed\n" 
                    else print_string ",  failed\n" ;;

check_tree tree1 correct_tree1 correct_inputstring1 ;;
check_tree tree2 correct_tree2 correct_inputstring2 ;;
check_tree tree3 correct_tree3 correct_inputstring3 ;;
check_tree tree4 correct_tree4 correct_inputstring4 ;;
check_tree tree5 correct_tree5 correct_inputstring5 ;;





