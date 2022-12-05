(* Sets Implementation for CMSC 330 Project 3
 * Last updated: 8 March 2022
 *
 * Refer to SETS.md for documentation and do not modify this file.
 *)

 let rec elem x a =
  match a with
  | h::t -> (h = x) || (elem x t)
  | [] -> false

let rec insert x a =
  if not (elem x a) then x::a else a

let insert_all xs a =
  List.fold_right insert xs a

let rec subset a b =
  match a with
  | h::t -> (elem h b) && (subset t b)
  | [] -> true

let rec eq a b = (subset a b) && (subset b a)

let rec remove x a =
  match a with
  | h::t -> if h = x then t else h::(remove x t)
  | [] -> []

let rec diff a b =  
  match b with
  | [] -> a
  | h::t -> diff (remove h a) t

(* Wrapper for old P3 *)
let rec minus a b = diff a b

let rec union a b =
  match a with
  | h::t -> insert h (union t b)
  | [] ->
    (match b with
     | h::t -> insert h (union [] t)
     | [] -> [])

let rec intersection a b =
  match a with
  | h::t -> if elem h b then insert h (intersection t b) else (intersection t b)
  | [] -> []

let rec product a b =
  let rec product_help x b =
    match b with
    | h::t -> insert (x, h) (product_help x t)
    | [] -> [] in
  match a with
  | h::t -> union (product_help h b) (product t b)
  | [] -> []

let rec cat x a =
  match a with
  | [] -> []
  | h::t -> (x,h)::(cat x t)

  type ('q, 's) transition = 'q * 's option * 'q

  type ('q, 's) nfa_t = {
    sigma: 's list;
    qs: 'q list;
    q0: 'q;
    fs: 'q list;
    delta: ('q, 's) transition list;
  }
  
  (***********)
  (* Utility *)
  (***********)
  
  (* explode converts a string to a character list *)
  let explode (s: string) : char list =
    let rec exp i l =
      if i < 0 then l else exp (i - 1) (s.[i] :: l)
    in
    exp (String.length s - 1) []
  
  (****************)
  (* Part 1: NFAs *)
  (****************)
  
  let rec move_rec (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =
    match nfa.delta with
    | [] -> []
    | (a,b,c)::delta_rest -> 
      (match qs with
      | [] -> []
      | state::qs_rest -> (if a=state then
        (if b=s then [c] else [])
        else [])@(move_rec nfa qs_rest s))@(move_rec 
          {
          sigma = nfa.sigma; 
          qs = nfa.qs;
          q0 = nfa.q0;
          fs = nfa.fs;
          delta = delta_rest} qs s)
      (*(match b with
      | None -> 
      | Some trans -> )*)
  
  let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =
    insert_all (move_rec nfa qs s) []
  
  let rec e_closure_rec (nfa: ('q,'s) nfa_t) (qs: 'q list) (checked: 'q list) : 'q list =
    match qs with
    | [] -> checked
    | check::rest -> 
      let list = (move nfa [check] None) in
      e_closure_rec nfa (insert_all (diff list checked) rest) (insert check checked)
      (*(e_closure_rec nfa (diff (move_rec nfa [q] None) ([q]@checked)) (insert_all (move_rec nfa [q] None) (checked)))
  *)
  let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
    insert_all (e_closure_rec nfa qs []) []
  
  let rec accept_helper (nfa: ('q,char) nfa_t) (chars: char list) (curr_states: 'q list) : bool =
    match curr_states with
    | [] -> false
    | a::curr_states_rest -> ((accept_helper nfa chars curr_states_rest)||
    (match chars with 
    | [] -> elem a nfa.fs 
    | b::chars_rest -> accept_helper nfa chars_rest (e_closure nfa (move nfa [a] (Some b)))))
  
  let accept (nfa: ('q,char) nfa_t) (s: string) : bool =
    (accept_helper nfa (explode s) (e_closure nfa [nfa.q0]))
  
  (*******************************)
  (* Part 2: Subset Construction *)
  (*******************************)
  
  let rec new_states_rec (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
    match nfa.sigma with
    | [] -> []
    | a::rest -> (e_closure nfa (move nfa qs (Some a)))::(new_states_rec 
    {
      sigma = rest; 
      qs = nfa.qs;
      q0 = nfa.q0;
      fs = nfa.fs;
      delta = nfa.delta} qs)
      
  let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
    new_states_rec nfa qs
  
  let rec new_trans_rec (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
    match nfa.sigma with
    | [] -> []
    | a::rest -> (qs, (Some a), (e_closure nfa (move nfa qs (Some a))))::(new_trans_rec 
    {
      sigma = rest; 
      qs = nfa.qs;
      q0 = nfa.q0;
      fs = nfa.fs;
      delta = nfa.delta} qs) 
  
  let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
    new_trans_rec nfa qs
  
  let rec new_finals_rec (nfa: ('q,'s) nfa_t) (qs: 'q list) =
    match qs with
    | [] -> false
    | a::rest -> (elem a nfa.fs)||(new_finals_rec nfa rest)
  
  let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
    if new_finals_rec nfa qs then [qs] else []
  
  let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
      (work: 'q list list) : ('q list, 's) nfa_t =
    failwith "unimplemented"
  
  let rec marked (marked_states: 'q list list) (state: 'q list) = 
    match marked_states with
    | [] -> false
    | lst::rest -> if eq lst state then true else marked rest state
  
  let rec for_each_sigma (nfa: ('q,'s) nfa_t) (state: 'q list) (nfa_a: ('q list, 's) nfa_t)
      (sigma_list: ('s list)) (check: 'q list list) : ('q list, 's) nfa_t * 'q list list =
    match sigma_list with
    | [] -> (nfa_a, check)
    | s::rest -> if (e_closure nfa (move nfa state (Some s))) = [] then 
      for_each_sigma nfa state nfa_a rest ((e_closure nfa (move nfa state (Some s)))::check)
    else
      for_each_sigma nfa state ({
      sigma = nfa_a.sigma;
      qs = nfa_a.qs;
      q0 = nfa_a.q0;
      fs = nfa_a.fs;
      delta = (nfa_a.delta)@[(state,(Some s),(e_closure nfa (move nfa state (Some s))))]}
      ) rest ((e_closure nfa (move nfa state (Some s)))::check)
  
  let rec nfa_to_dfa_helper (nfa: ('q,'s) nfa_t) (states_to_check: 'q list list) 
      (marked_states: 'q list list) (nfa_a: ('q list, 's) nfa_t) : ('q list, 's) nfa_t =
    match states_to_check with
    | [] -> {sigma = nfa_a.sigma; qs = marked_states; 
              q0 = nfa_a.q0; fs = nfa_a.fs; delta = nfa_a.delta}
    | check_lst::rest -> 
      if check_lst = [] then nfa_to_dfa_helper nfa rest marked_states nfa_a else
      if marked marked_states check_lst then
        nfa_to_dfa_helper nfa rest marked_states nfa_a
      else
        let (ret_nfa, ret_checks) = for_each_sigma nfa check_lst nfa_a nfa.sigma [] in
          nfa_to_dfa_helper nfa (rest@ret_checks) (check_lst::marked_states) ret_nfa
  
  let rec make_finals (nfa: ('q list,'s) nfa_t) (fs: 'q list) (qs: 'q list list) : ('q list, 's) nfa_t =
    match fs with
    | [] -> nfa
    | f::f_rest -> (
      match qs with
      | [] -> make_finals nfa f_rest nfa.qs
      | q::q_rest -> if elem f q then 
        make_finals ({sigma = nfa.sigma;
      qs = nfa.qs;
      q0 = nfa.q0;
      fs = nfa.fs@[q];
      delta = nfa.delta}) fs q_rest else
        make_finals nfa fs q_rest
    )
  
  
  let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
    let nfa_finals = nfa_to_dfa_helper nfa [(e_closure nfa [nfa.q0])] [] {sigma = nfa.sigma; 
      qs = [];
      q0 = (e_closure nfa [nfa.q0]);
      fs = [];
      delta = []}
    in
      make_finals nfa_finals nfa.fs nfa_finals.qs

      type regexp_t =
      | Empty_String
      | Char of char
      | Union of regexp_t * regexp_t
      | Concat of regexp_t * regexp_t
      | Star of regexp_t
    
    (***********)
    (* Utility *)
    (***********)
    
    let fresh =
      let cntr = ref 0 in
      fun () ->
        cntr := !cntr + 1 ;
        !cntr
    
    (*******************************)
    (* Part 3: Regular Expressions *)
    (*******************************)
    
    let rec elem x a =
      match a with
      | h::t -> (h = x) || (elem x t)
      | [] -> false
    
    let rec insert x a =
      if not (elem x a) then x::a else a
    
    let rec union a b =
      match a with
      | h::t -> insert h (union t b)
      | [] ->
        (match b with
         | h::t -> insert h (union [] t)
         | [] -> [])
    
    let rec regexp_to_nfa_rec (regexp: regexp_t) : (int, char) nfa_t =
      match regexp with
      | Empty_String -> let state1 = fresh () in
        let state2 = fresh () in
          {sigma = [];
          qs = [state1;state2];
          q0 = state1;
          fs = [state2];
          delta = [(state1, None, state2)];
          }
      | Char(c) ->let state1 = fresh () in
        let state2 = fresh () in
          {sigma = [c];
          qs = [state1;state2];
          q0 = state1;
          fs = [state2];
          delta = [(state1, Some c, state2)];
          }
      | Union(reg1, reg2) -> let nfa1 = regexp_to_nfa_rec reg1 in
        let nfa2 = regexp_to_nfa_rec reg2 in
        let state1 = fresh () in
        let state2 = fresh () in
          {sigma = (union nfa1.sigma nfa2.sigma);
          qs = insert state2 (insert state1 (union nfa1.qs nfa2.qs));
          q0 = state1;
          fs = [state2];
          delta = insert (state1, None, nfa2.q0) (insert (state1, None, nfa1.q0) (insert ((match nfa2.fs with 
          | [] -> failwith "nfa1 has no final state"
          | f::rest -> f), None, state2) (insert ((match nfa1.fs with 
          | [] -> failwith "nfa2 has no final state"
          | f::rest -> f), None, state2) (union nfa1.delta nfa2.delta))))
          }
      | Concat(reg1, reg2) -> let nfa1 = regexp_to_nfa_rec reg1 in
        let nfa2 = regexp_to_nfa_rec reg2 in
          {sigma = (union nfa1.sigma nfa2.sigma);
          qs = (union nfa1.qs nfa2.qs);
          q0 = nfa1.q0;
          fs = nfa2.fs;
          delta = insert ((match nfa1.fs with 
                          | [] -> failwith "nfa1 has no final state"
                          | f::rest -> f), None, nfa2.q0) (union nfa1.delta nfa2.delta)
          }
      | Star(reg) -> let nfa = regexp_to_nfa_rec reg in
        let state1 = fresh () in
        let state2 = fresh () in
          {sigma = nfa.sigma;
          qs = insert state2 (insert state1 nfa.qs);
          q0 = state1;
          fs = [state2];
          delta = insert (state1, None, state2) (insert (state2, None, state1) 
          (insert (state1, None, nfa.q0) (insert ((match nfa.fs with 
          | [] -> failwith "nfa has no final state"
          | f::rest -> f), None, state2) (nfa.delta))))
          }
    
    let regexp_to_nfa (regexp: regexp_t) : (int, char) nfa_t =
      regexp_to_nfa_rec regexp

