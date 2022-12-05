open Funs

(*************************************)
(* Part 2: Three-Way Search Tree *)
(*************************************)

type int_tree =
  | IntLeaf
  | IntNode of int * int option * int_tree * int_tree * int_tree 

let empty_int_tree = IntLeaf

let rec int_insert x t = 
  match t with
  | IntLeaf -> IntNode (x, None, IntLeaf, IntLeaf, IntLeaf)
  | IntNode(a, None, c, d, e) ->
    (if (x<a) then
      IntNode(x, Some a, IntLeaf, IntLeaf, IntLeaf)
    else if (x=a) then
      t
    else
      IntNode(a, Some x, IntLeaf, IntLeaf, IntLeaf)
    )
  | IntNode(a, Some b, c, d, e) ->
    (if (x<a) then
      IntNode(a, Some b, int_insert x c, d, e)
    else if (x=a) then
      t
    else if (x<b) then
      IntNode(a, Some b, c, int_insert x d, e)
    else if (x=b) then
      t
    else 
      IntNode(a, Some b, c, d, int_insert x e))

let rec int_mem x t = 
  match t with
  | IntLeaf -> false
  | IntNode(a, None, c, d, e) ->
    (if (a=x) then true else false)
  | IntNode(a, Some b, c, d, e) ->
    (if (x<a) then
      int_mem x c
    else if (x=a) then
      true
    else if (x<b) then
      int_mem x d
    else if (x=b) then
      true
    else 
      int_mem x e)


let rec int_size t = 
  match t with
  | IntLeaf -> 0
  | IntNode(a, None, c, d, e) -> 1
  | IntNode(a, Some b, c, d, e) -> 
    2 + (int_size c) + (int_size d) + (int_size e)

let rec int_max t =
  match t with
  | IntLeaf -> failwith "Invalid_argument"
  | IntNode(a, None, c, d, e) -> a
  | IntNode(a, Some b, c, d, e) -> 
    (if (e=IntLeaf) then b else (int_max e))


(*******************************)
(* Part 3: Three-Way Search Tree-Based Map *)
(*******************************)

type 'a tree_map =
  | MapLeaf
  | MapNode of (int * 'a) * (int * 'a) option * 'a tree_map * 'a tree_map * 'a tree_map

let empty_tree_map = MapLeaf

let rec map_put k v t = 
  match t with
  | MapLeaf -> MapNode ((k,v), None, MapLeaf, MapLeaf, MapLeaf)
  | MapNode((a,f), None, c, d, e) ->
    (if (k<a) then
      MapNode((k,v), Some (a,f), MapLeaf, MapLeaf, MapLeaf)
    else if (k=a) then
      failwith "Invalid_argument"
    else
      MapNode((a,f), Some (k,v), MapLeaf, MapLeaf, MapLeaf)
    )
  | MapNode((a,f), Some (b,g), c, d, e) ->
    (if (k<a) then
      MapNode((a,f), Some (b,g), map_put k v c, d, e)
    else if (k=a) then
      failwith "Invalid_argument"
    else if (k<b) then
      MapNode((a,f), Some (b,g), c, map_put k v d, e)
    else if (k=b) then
      failwith "Invalid_argument"
    else 
      MapNode((a,f), Some (b,g), c, d, map_put k v e))

let rec map_contains k t = 
  match t with
  | MapLeaf -> false
  | MapNode((a,f), None, c, d, e) ->
    (if (a=k) then true else false)
  | MapNode((a,f), Some (b,g), c, d, e) ->
    (if (k<a) then
      map_contains k c
    else if (k=a) then
      true
    else if (k<b) then
      map_contains k d
    else if (k=b) then
      true
    else 
      map_contains k e)

let rec map_get k t =
  match t with
  | MapLeaf -> failwith "Invalid_argument"
  | MapNode((a,f), None, c, d, e) ->
    (if (a=k) then f else (failwith "Invalid_argument"))
  | MapNode((a,f), Some (b,g), c, d, e) ->
    (if (k<a) then
      map_get k c
    else if (k=a) then
      f
    else if (k<b) then
      map_get k d
    else if (k=b) then
      g
    else 
      map_get k e)

(***************************)
(* Part 4: Variable Lookup *)
(***************************)

(* Modified tree from part 3 to store key value pairs in each scope *)
type tree_scope =
  | ScopeLeaf
  | ScopeNode of (string * int) * (string * int) option * tree_scope * tree_scope * tree_scope

let empty_tree_scope = ScopeLeaf

let rec scope_put k v t = 
  match t with
  | ScopeLeaf -> ScopeNode ((k,v), None, ScopeLeaf, ScopeLeaf, ScopeLeaf)
  | ScopeNode((a,f), None, c, d, e) ->
    (if (k<a) then
      ScopeNode((k,v), Some (a,f), ScopeLeaf, ScopeLeaf, ScopeLeaf)
    else if (k=a) then
      failwith "Duplicate variable binding in scope!"
    else
      ScopeNode((a,f), Some (k,v), ScopeLeaf, ScopeLeaf, ScopeLeaf)
    )
  | ScopeNode((a,f), Some (b,g), c, d, e) ->
    (if (k<a) then
      ScopeNode((a,f), Some (b,g), scope_put k v c, d, e)
    else if (k=a) then
      failwith "Duplicate variable binding in scope!"
    else if (k<b) then
      ScopeNode((a,f), Some (b,g), c, scope_put k v d, e)
    else if (k=b) then
      failwith "Duplicate variable binding in scope!"
    else 
      ScopeNode((a,f), Some (b,g), c, d, scope_put k v e))


(* Modify the next line to your intended type *)
type lookup_table = 
  | TopScope
  | InnerScope of lookup_table * tree_scope

let empty_table : lookup_table = TopScope

let push_scope (table : lookup_table) : lookup_table = 
  InnerScope(table, empty_tree_scope)
  
let pop_scope (table : lookup_table) : lookup_table =
  match table with 
  | TopScope -> failwith "No scopes remain!"
  | InnerScope(a,b) -> a

let add_var name value (table : lookup_table) : lookup_table =
  match table with
  | TopScope -> failwith "There are no scopes to add a variable to!"
  | InnerScope(a,b) -> InnerScope(a, scope_put name value b)

let rec lookup name (table : lookup_table) =
  match table with
  | TopScope -> failwith "Variable not found!"
  | InnerScope(s,t) -> 
    (match t with
    | ScopeLeaf -> lookup name s
    | ScopeNode((a,f), None, c, d, e) ->
      (if (a=name) then f else (lookup name s))
    | ScopeNode((a,f), Some (b,g), c, d, e) ->
      (if (name<a) then
        lookup name (InnerScope(s,c))
      else if (name=a) then
        f
      else if (name<b) then
        lookup name (InnerScope(s,d))
      else if (name=b) then
        g
      else 
        lookup name (InnerScope(s,e))))
