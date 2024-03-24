(* helper functions for opearting on z3 models*)

module PA = Smtlib_utils.V_2_6.Ast
open Context
open Z3
open Containers

open R6reduce_axioms
open Z3_utils

exception UnsupportedQuery of string

module StrTbl = CCHashtbl.Make (CCString)


(* A version of generate disequalities & acyclicality_axioms_term that won't put things in Ctx.t.axioms, and instead returns them*)

let generate_disequalities_return term (term_ty : PA.ty) parents guards =
  if (List.is_empty parents) then []
  else
    let parent_term, parent_ty = fst (List.nth parents 0) in
    if (ty_equal parent_ty term_ty) then (
      (* print_string ("We are adding with the guards: "); let _ = List.map (fun (guard : PA.term) -> stmt_printer [PA.Stmt_assert guard]) guards in *)
      let stmt =
        PA.Stmt_assert
          (PA.Imply (PA.And guards, (PA.Not (PA.Eq (term, parent_term)))))
      in
      [stmt])
    else []

let generate_acyclicality_axioms_term_return term term_ty min_depth max_depth selector_list =
  let rec loop stack axioms =
    match stack with
    | [] -> axioms
    | (term, term_ty, selection_depth, selector_list, parents, guards) :: rest
      -> (
        let new_axioms = 
                (if
                  List.length selector_list = 0
                  && selection_depth - max_depth <= min_depth
                then generate_disequalities_return term term_ty parents guards
                else []) in
        (*NOTE: I did not have the if here before and this created multiple of
          the same axioms*)
        if selection_depth = 0 then (loop [@tailcall]) rest (new_axioms @ axioms)
        else
          match selector_list with
          | (selector_name, selector_ty, cstor_name) :: selectors' -> (
            match selector_ty with
            | PA.Ty_app (adt_name, _) -> (
              match StrTbl.find_opt Ctx.t.adt_with_selectors adt_name with
              | Some new_selectors ->
                  let new_parents =
                    if List.length parents = 0 then
                      [((term, term_ty), "is-" ^ cstor_name)]
                    else parents
                  in
                  (loop [@tailcall])
                    ( ( PA.App (selector_name, [term])
                      , selector_ty
                      , selection_depth - 1
                      , new_selectors
                      , new_parents
                      , PA.App ("is-" ^ cstor_name, [term]) :: guards )
                    :: ( term
                       , term_ty
                       , selection_depth
                       , selectors'
                       , parents
                       , guards )
                    :: rest ) (new_axioms @ axioms)
              | None ->
                  (loop [@tailcall])
                    ( ( term
                      , term_ty
                      , selection_depth
                      , selectors'
                      , parents
                      , guards )
                    :: rest ) (new_axioms @ axioms))
            | _ ->
                (loop [@tailcall])
                  ( (term, term_ty, selection_depth, selectors', parents, guards)
                  :: rest ) (new_axioms @ axioms))
          | _ -> (loop [@tailcall]) rest (new_axioms @ axioms))
  in
  loop [(term, term_ty, max_depth, selector_list, [], [])] []


(*we have to use some sort of stack rip to avoid tail recursision since the depths can branch*)
let find_one_cycle original_term original_term_ty max_depth model selector_list ctx func_decls sorts  =
  let z3_original_term = convert_smt_to_z3_term ctx func_decls sorts original_term in 
  let original_selector_list = selector_list in
  let rec loop stack =
    match stack with
      | [] -> [], false
      | (term, term_ty, selection_depth, selector_list, guards) :: rest
        -> (
        (* print_string ("Checking "); stmt_printer [PA.Stmt_assert term]; print_endline ("at a depth " ^ (string_of_int selection_depth));
        print_string "Printing the selector list: ";
        let _ = List.map (fun (a,b, c) -> print_string a; Context.alt_ty_printer b; print_string c) selector_list in *)
        let z3_term = convert_smt_to_z3_term ctx func_decls sorts term in
        let z3_term_ty = convert_smt_to_z3_ty ctx sorts term_ty in
        let z3_selector_list = List.map (fun (selector_name, selector_ty, _) -> selector_ty, (Z3.FuncDecl.mk_func_decl_s ctx selector_name [z3_term_ty] (convert_smt_to_z3_ty ctx sorts selector_ty), selector_name)) selector_list in
        let one_applications = List.filter_map 
                                  (fun selector -> (*print_endline ("the ty for selector " ^ (snd (snd selector)) ^ " is "); Context.alt_ty_printer (fst selector);
                                                   print_endline ("which is being compared to: "); Context.alt_ty_printer term_ty;
                                                   print_endline ("the ty for original term is "); Context.alt_ty_printer (original_term_ty);
                                                   print_endline ("which is being compared to: "); Context.alt_ty_printer (Ctx.get_selector_output_ty (snd (snd selector)));*)
                                                  (* if ((ty_equal (fst selector) term_ty) && (ty_equal original_term_ty (Ctx.get_selector_output_ty (snd (snd selector))))) *)
                                                  if (ty_equal (fst selector) original_term_ty)
                                                   then Some (Z3.Expr.mk_app ctx (fst (snd selector)) [z3_term], PA.App ((snd (snd selector)), [term]))
                                                   else None) z3_selector_list in 
        (*I don't know if Z3.Expr.equal will work in the way that I want it to*)
        let new_guard = 
          begin match selector_list with 
            | (_, _, cstor_name_) :: _ -> Some (Z3.Expr.mk_app ctx (FuncDecl.mk_func_decl_s ctx ("is-" ^ cstor_name_) [(convert_smt_to_z3_ty ctx sorts (Ctx.get_cstor_type cstor_name_))] (Z3.Boolean.mk_sort ctx)) [(convert_smt_to_z3_term ctx func_decls sorts term)])
            | [] -> None
          end in        
          let equality_to_check = List.fold_left  
                                  (fun acc one_application -> 
                                    (* print_endline ("We are comparing" ^ (Expr.to_string one_application) ^ " to " ^ (Expr.to_string z3_original_term)); *)
                                    acc || 
                                    let and_expr = (((Z3.Boolean.mk_eq ctx (fst one_application) z3_original_term) :: (match new_guard with Some n_g -> [n_g] | None -> []) @ guards)) in
                                    let expr = (Z3.Boolean.mk_and ctx and_expr) in
                                    (* print_string "Printing the and: ";
                                    let _ = List.map (fun e -> print_string (Expr.to_string e ^ ";")) and_expr in*)
                                    (* print_endline ("Printing the Expression" ^ (Expr.to_string expr));  *)
                                    let model_evaluation = (Z3.Model.eval model expr false) in (*TODO: add one more guard to this line*)
                                    begin match model_evaluation with
                                      | Some value -> 
                                          if (Z3.Boolean.is_true value) then ((*print_string ("evaluated true: " ^ (Z3.Expr.to_string expr));*) true)
                                          else (
                                            if (Z3.Boolean.is_false value) then ((*print_string ("evaluated false: " ^ (Z3.Expr.to_string expr));*) false)
                                            else ((*Note: if the model evaluates to something that is neither true nor false, 
                                                          then we assume that there are no constraints on that specific selector.
                                                          Thus, we can assume that there is a model without any cycles on this selector.contents
                                                          This should be sound, but is a little risky because I won't get warned of other bugs *)
                                                  (* print_string ("In the neither case for" ^ (Z3.Expr.to_string value)); *)
                                                  false)
                                                  (* print_endline "The model is:";
                                                  print_endline ("\n _______________________________________  \n Model: \n" ^ Z3.Model.to_string model);
                                                  raise (UnsupportedQuery ("Model evaluates to something that is not true or fals for the expression: " ^ (Z3.Expr.to_string value) ^ "and it is a boolean: " ^ (string_of_bool (Z3.Boolean.is_bool value))))) *)
                                          )
                                      | None -> raise (UnsupportedQuery ("Model cannot evaluate and equality for the expression: " ^ (Z3.Expr.to_string expr)))
                                    end)
                                  false one_applications in
        if (equality_to_check) then (
          (* print_string "Printing the one applications:";  *)
          (* let _ = List.map (fun expr -> print_string (Z3.Expr.to_string (fst expr))) one_applications in  *)
          (* print_string "we get a cycle on the term: "; stmt_printer [PA.Stmt_assert term]; print_string "while compared to: "; stmt_printer [PA.Stmt_assert original_term]; print_string " and the depth is "; print_int max_depth; *)
          (* print_string "This is the model: "; print_string (Z3.Model.to_string model); *)
          (* print_string ("the guards we used where"); let _ =  List.map (fun (e: Expr.expr) -> print_string (Expr.to_string e)) guards in *)
          (*TODO: right now this just adds in duplicate axioms if we already instatiated up to a smaller depth*)
          let new_axioms = generate_acyclicality_axioms_term_return original_term original_term_ty 0 (max_depth - selection_depth + 2) original_selector_list, true in (*TODO: rewrite how the acyclicality axioms are added in: should not be based off of selector_list*)
          (* print_string "adding the axioms: "; stmt_printer (fst new_axioms); *)
          (* print_endline ("with the bool: " ^ (string_of_bool (snd new_axioms))); *)
          new_axioms
        ) else (
          if selection_depth = 0 then (loop [@tailcall]) rest
          else
            match selector_list with
            | (selector_name, selector_ty, cstor_name) :: selectors' -> (
              match selector_ty with
              | PA.Ty_app (adt_name, _) -> (
                match StrTbl.find_opt Ctx.t.adt_with_selectors adt_name with
                | Some new_selectors ->
                    (loop [@tailcall])
                      ( ( PA.App (selector_name, [term])
                        , selector_ty
                        , selection_depth - 1
                        , new_selectors
                        ,  (Z3.Expr.mk_app ctx (FuncDecl.mk_func_decl_s ctx ("is-" ^ cstor_name) [(convert_smt_to_z3_ty ctx sorts (Ctx.get_cstor_type cstor_name))] (Z3.Boolean.mk_sort ctx)) [(convert_smt_to_z3_term ctx func_decls sorts term)]) :: guards)
                      :: ( term
                        , term_ty
                        , selection_depth
                        , selectors'
                        , guards)
                      :: rest )
                | None ->
                    (loop [@tailcall])
                      ( ( term
                        , term_ty
                        , selection_depth
                        , selectors'
                        , guards)
                      :: rest ) )
              | _ ->
                  (loop [@tailcall])
                    ( (term, term_ty, selection_depth, selectors', guards)
                    :: rest ) )
            | _ -> (loop [@tailcall]) rest ))
  in
  loop [(original_term, original_term_ty, max_depth, selector_list, [])]


let find_cycles_in_model_adt terms_with_ty max_depth (selector_list : (string * PA.ty * string) list) model ctx func_decls sorts =
  let rec aux terms_with_ty max_depth (selector_list : (string * PA.ty * string) list) =
    match terms_with_ty with
    | value :: rest ->
        let new_axioms, found_cycle = find_one_cycle (fst value) (snd value) max_depth model selector_list ctx func_decls sorts
        in
        if (not found_cycle) then aux rest max_depth selector_list
        else ((*print_string "found loop"; stmt_printer new_axioms; *)
              new_axioms, true)
    | _ -> [], false
  in
  aux terms_with_ty max_depth selector_list 

let find_cycles_in_model keys model ctx func_decls sorts =
  let rec aux keys =
    match keys with
    | key :: rest ->
        let values, ty = StrTbl.find Ctx.t.adt_with_depth_variables key in
        let values_len = Ctx.get_vertex_weight key in
        (* print_endline (" For " ^ key ^ " we have a depth of " ^ (string_of_int values_len)); *)
        let stop = values_len in
        let constructor_list = StrTbl.find Ctx.t.adts key in
        let selector_list = generate_selector_list constructor_list in
        let _ = create_selector_values (StrTbl.keys_list Ctx.t.adts) in
        let new_axioms, found_cycle =
          find_cycles_in_model_adt
            (List.map (fun x -> (PA.Const (fst x), ty)) values)
            stop selector_list model ctx func_decls sorts
        in
        if found_cycle then new_axioms, true else (aux rest)
    | _ -> [], false
  in
  aux keys
(* 
(* more general function that finds if there a cycle of a specific adt type*)
let find_cycles_in_model_adt adt_constants selectors (model : Z3.Model.model) ctx depth = 
  let adt_z3_constants = List.map (fun x -> Z3.FuncDecl.mk_const_decl_s ctx (fst x) (snd x)) adt_constants in 
  let z3_selectors = List.map (fun x -> Z3.FuncDecl.mk_func_decl_s ctx (fst x) (fst (snd x)) (snd (snd x))) selectors in
   *)