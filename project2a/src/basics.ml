(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup tup = 
  match tup with
  | (a,b,c) -> (c,b,a)

let is_odd x = abs(x) mod 2 = 1

let area x y = 
  match x with
  | (a,b) -> match y with
             | (c,d) -> abs(c-a)*abs(d-b)

let volume x y = 
  match x with
  | (a,b,c) -> match y with
               | (d,e,f) -> abs(d-a)*abs(e-b)*abs(f-c)

(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec fibonacci n = if n = 0 then 
  0
else if n = 1 then
  1
else
  fibonacci (n-1) + fibonacci (n-2)

let rec pow x y = if y < 1 then 1 else x*pow x (y-1)

let rec log x y = if y < x then 0 else 1+log x (y/x)

let rec gcf x y = if y = 0 then
  x
else if x mod y = 0 then y else gcf y (x mod y);;

let rec is_prime_count x count = 
  if float_of_int (count) <= sqrt (float_of_int x) then
    if x mod count = 0 then false else is_prime_count x (count+1)
  else true

let rec is_prime x = if x < 2 then false else is_prime_count x 2 

(*****************)
(* Part 3: Lists *)
(*****************)

let rec get idx lst = 
  match lst with
  | [] -> failwith "Out of bounds"
  | (v::rest) -> if idx = 0 then v else get (idx-1) rest

let rec size lst = 
  match lst with
  | [] -> 0
  | (v::rest) -> 1 + size rest

let larger lst1 lst2 = if size lst1 > size lst2 then
  lst1 
else if size lst1 = size lst2 then
  []
else
  lst2

let rec rec_reverse lst =
  match lst with
  | [] -> []
  | (v::rest) -> ((rec_reverse rest)@[v])

let reverse lst = rec_reverse lst

let rec combine lst1 lst2 = 
  match lst1 with
  | [] -> (match lst2 with
    | [] -> []
    | (v2::rest2) -> v2::(combine lst1 rest2))
  | (v1::rest1) -> v1::(combine rest1 lst2)

let rec merge lst1 lst2 = 
  match lst1 with
  | [] -> lst2
  | (v1::rest1) -> (match lst2 with
    | [] -> lst1
    | (v2::rest2) -> if v1>v2 then 
      v2::(merge lst1 rest2)
    else
      v1::(merge rest1 lst2))

let rec rotate shift lst = if shift = 0 then
  lst
else
  match lst with
  | [] -> []
  | (v::rest) -> rotate (shift-1) (rest@[v])

let rec is_palindrome lst = (lst = reverse lst)