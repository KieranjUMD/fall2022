open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else update t x v
        
(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e = 
  match e with 
  | Value(Int(x)) -> Int x
  | Value(Bool(x)) -> Bool x
  | Value(String(str)) -> String str
  | Value(Closure(envir, str, expr1)) -> Closure (envir, str, expr1)
  | ID(str) -> lookup env str
  | Fun (str, expr1) -> Closure (env, str, expr1)
  | Not (expr1) -> (
    match (eval_expr env expr1) with
    | Bool(x) -> Bool (not x)
    | _ -> raise (TypeError "Not applied to a non-boolean")
    )
  | Binop(oper, expr1, expr2) -> (
    match oper with
    | Add -> (
      match (eval_expr env expr1) with
      | Int(x) -> (
        match (eval_expr env expr2) with
        | Int(y) -> Int (x + y)
        | _ -> raise (TypeError "Add applied to a non-int")
      )
      | _ -> raise (TypeError "Add applied to a non-int")
    )
    | Sub -> (
      match (eval_expr env expr1) with
      | Int(x) -> (
        match (eval_expr env expr2) with
        | Int(y) -> Int(x - y)
        | _ -> raise (TypeError "Sub applied to a non-int")
       )
       | _ -> raise (TypeError "Sub applied to a non-int")
     )
    | Mult -> (
      match (eval_expr env expr1) with
      | Int(x) -> (
        match (eval_expr env expr2) with
         | Int(y) -> Int(x * y)
         | _ -> raise (TypeError "Mult applied to a non-int")
       )
       | _ -> raise (TypeError "Mult applied to a non-int")
     )
    | Div -> (
      match (eval_expr env expr1) with
      | Int(x) -> (
         match (eval_expr env expr2) with
         | Int(y) -> if y = 0 then (raise DivByZeroError) else Int(x / y)
        | _ -> raise (TypeError "Div applied to a non-int")
      )
      | _ -> raise (TypeError "Div applied to a non-int")
     )
    | Concat -> (
      match (eval_expr env expr1) with
      | String(str1) -> (
        match (eval_expr env expr2) with
         | String(str2) -> String(str1 ^ str2)
         | _ -> raise (TypeError "Concat applied to a non-String")
       )
       | _ -> raise (TypeError "Concat applied to a non-String")
     )
    | Greater -> (
      match (eval_expr env expr1) with
      | Int(x) -> (
        match (eval_expr env expr2) with
         | Int(y) -> Bool(x > y)
         | _ -> raise (TypeError "Greater applied to a non-int")
       )
       | _ -> raise (TypeError "Greater applied to a non-int")
     )
    | Less -> (
      match (eval_expr env expr1) with
      | Int(x) -> (
        match (eval_expr env expr2) with
         | Int(y) -> Bool(x < y)
         | _ -> raise (TypeError "Less applied to a non-int")
       )
       | _ -> raise (TypeError "Less applied to a non-int")
     )
    | GreaterEqual -> (
      match (eval_expr env expr1) with
      | Int(x) -> (
        match (eval_expr env expr2) with
         | Int(y) -> Bool(x >= y)
         | _ -> raise (TypeError "GreaterEqual applied to a non-int")
       )
       | _ -> raise (TypeError "GreaterEqual applied to a non-int")
     )
    | LessEqual -> (
      match (eval_expr env expr1) with
      | Int(x) -> (
        match (eval_expr env expr2) with
         | Int(y) -> Bool(x <= y)
         | _ -> raise (TypeError "LessEqual applied to a non-int")
       )
       | _ -> raise (TypeError "LessEqual applied to a non-int")
     )
    | Equal -> (
      match (eval_expr env expr1) with
      | Int(x) -> (
        match (eval_expr env expr2) with
         | Int(y) -> Bool(x = y)
         | _ -> raise (TypeError "Equal applied to an int and a non-int")
       )
       | Bool(x) -> (
        match (eval_expr env expr2) with
         | Bool(y) -> Bool(x = y)
         | _ -> raise (TypeError "Equal applied to a bool and a non-bool")
       )
       | String(x) -> (
        match (eval_expr env expr2) with
         | String(y) -> Bool(x = y)
         | _ -> raise (TypeError "Equal applied to a string and a non-string")
       )
       | _ -> raise (TypeError "Equal can only be applied to ints, bools, and strings")
     )
    | NotEqual -> (
      match (eval_expr env expr1) with
      | Int(x) -> (
        match (eval_expr env expr2) with
         | Int(y) -> Bool(x <> y)
         | _ -> raise (TypeError "NotEqual applied to an int and a non-int")
       )
       | Bool(x) -> (
        match (eval_expr env expr2) with
         | Bool(y) -> Bool(x <> y)
         | _ -> raise (TypeError "NotEqual applied to a bool and a non-bool")
       )
       | String(x) -> (
        match (eval_expr env expr2) with
         | String(y) -> Bool(x <> y)
         | _ -> raise (TypeError "NotEqual applied to a string and a non-string")
       )
       | _ -> raise (TypeError "NotEqual can only be applied to ints, bools, and strings")
     )
    | Or -> (
      match (eval_expr env expr1) with
      | Bool(x) -> (
        match (eval_expr env expr2) with
         | Bool(y) -> Bool(x || y)
         | _ -> raise (TypeError "Or applied to a non-bool")
       )
       | _ -> raise (TypeError "Or applied to a non-bool")
     )
    | And -> (
      match (eval_expr env expr1) with
      | Bool(x) -> (
        match (eval_expr env expr2) with
         | Bool(y) -> Bool(x && y)
         | _ -> raise (TypeError "And applied to a non-bool")
       )
       | _ -> raise (TypeError "And applied to a non-bool")
     )
  )
  | If(expr1, expr2, expr3) -> (
    match (eval_expr env expr1) with
    | Bool(x) -> (if x then (eval_expr env expr2) else (eval_expr env expr3))
    | _ -> raise (TypeError "Or applied to a non-bool")
   )
  | FunctionCall(expr1, expr2) -> (
    match (eval_expr env expr1) with
    | Closure(envir, str, expr1) -> (
      match (eval_expr env expr2) with 
      | Int(x) -> eval_expr (extend env str (Int(x))) expr1
      | Bool(x) -> eval_expr (extend env str (Bool(x))) expr1
      | String(x) -> eval_expr (extend env str (String(x))) expr1
      | Closure(a, b, c) -> eval_expr (extend env str (Closure(a, b, c))) expr1
    )
    | Int(x) -> eval_expr env expr2
    | String(x) -> eval_expr env expr2
    | Bool(x) -> eval_expr env expr2
    | _ -> raise (TypeError "Closure should be first part of FunctionCall")
  )
  | Let(func_name, let_rec, expr1, expr2) -> (
    if let_rec then (
          match (eval_expr (extend_tmp env func_name) expr1) with
          | Closure(envir, var, expr) -> eval_expr (extend envir func_name (Closure(envir, var, expr))) expr2
          | _ -> failwith "something went wrong"
      ) else (
      eval_expr (extend env func_name (eval_expr env expr1)) expr2
    )
  )

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = 
  match m with
  | Def(var, expr1) -> let new_env = (extend_tmp env var) in (
    let new_val = eval_expr new_env expr1 in (
      (extend env var new_val, Some (new_val))
    )
  )
  | Expr(expr1) -> (env, Some (eval_expr env expr1))
  | NoOp -> ([], None)