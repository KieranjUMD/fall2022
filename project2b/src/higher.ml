open Funs

(********************************)
(* Part 1: High Order Functions *)
(********************************)

let contains_elem lst e = fold (fun a x -> 
  if (x = e) then
    true
  else
    a) false lst

let is_present lst x = map (fun elem -> 
  if (x = elem) then
    1
  else
    0) lst

let count_occ lst target = fold (fun a x ->
  if (target = x) then
    a+1
  else
    a) 0 lst

let uniq lst = fold (fun a x ->
  if ((contains_elem a x) = false) then
    x::a
  else
    a) [] lst

let assoc_list lst = fold (fun a x ->
  (x,count_occ lst x)::a) [] (uniq lst)

let ap fns args = fold (fun a x ->
  a@(fold (fun b y ->
  b@[(x y)]) [] args)) [] fns
