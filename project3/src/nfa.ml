open List
open Sets

(*********)
(* Types *)
(*********)

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

