open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* PASTE YOUR PARSERS FROM P4A HERE *)

let rec find_in (toks: token list) (n: int) (read_toks: token list) = 
  match toks with
  | [] -> raise (InvalidInputException "parsing error: unmatched let")
  | h::rest -> (match h with
                | Tok_In -> (if n = 0 then (read_toks, rest) 
                  else find_in rest (n-1) (read_toks@[Tok_In]) )
                | Tok_Let -> find_in rest (n+1) (read_toks@[Tok_Let])
                | _ -> find_in rest n (read_toks@[h])
                )

let rec find_RParen (toks: token list) (n: int) (read_toks: token list) = 
  match toks with
  | [] -> raise (InvalidInputException "parsing error: unmatched (")
  | h::rest -> (match h with
                | Tok_RParen -> (if n = 0 then (read_toks, rest) 
                  else find_RParen rest (n-1) (read_toks@[Tok_RParen]) )
                | Tok_LParen -> find_RParen rest (n+1) (read_toks@[Tok_LParen])
                | _ -> find_RParen rest n (read_toks@[h]))

let rec find_Arrow (toks: token list) (n: int) = 
  match toks with
  | [] -> raise (InvalidInputException "parsing error: unmatched fun")
  | h::rest -> (match h with
                | Tok_Arrow -> (if n = 0 then rest 
                  else find_Arrow rest (n-1) )
                | Tok_Fun -> find_Arrow rest (n+1) 
                | _ -> find_Arrow rest n
                )

let rec move_forward (toks: token list) (n: int) =
  if n <= 0 then toks else
    (match toks with
    | [] -> raise (InvalidInputException "parsing error: too short of expression")
    | h::rest -> move_forward rest (n-1)
      )

let rec find_then (toks: token list) (n: int) (m: int) (read_toks: token list) =
  match toks with
  | [] -> raise (InvalidInputException "parsing error: unmatched if")
  | h::rest -> (match h with
                | Tok_Then -> (if n = 0 then (if m = 0 then (read_toks, rest) else raise (InvalidInputException "incorrect num of else")) 
                  else find_then rest (n-1) m (read_toks@[Tok_Then]) )
                | Tok_Else -> (if m = 0 then raise (InvalidInputException "incorrect num of else")
                else find_then rest n (m-1) (read_toks@[Tok_Else]))
                | Tok_If -> find_then rest (n+1) (m+1) (read_toks@[Tok_If])
                | _ -> find_then rest n m (read_toks@[h]))

let rec find_else (toks: token list) (n: int) (m: int) (read_toks: token list) =
  match toks with
  | [] -> raise (InvalidInputException "parsing error: unmatched if")
  | h::rest -> (match h with
                | Tok_Else -> (if n = 0 then (if m = 0 then (read_toks, rest) else raise (InvalidInputException "incorrect num of then")) 
                  else find_else rest (n-1) m (read_toks@[Tok_Else]) )
                | Tok_Then-> (if m = 0 then raise (InvalidInputException "incorrect num of then")
                else find_then rest n (m-1) (read_toks@[Tok_Then]))
                | Tok_If -> find_else rest (n+1) (m+1) (read_toks@[Tok_If])
                | _ -> find_else rest n m (read_toks@[h]))

let rec parse_expr toks =  
  match toks with
  | [] -> raise (InvalidInputException "parsing error")
  | h::rest -> (
    match h with
    | Tok_RParen ->    raise (InvalidInputException "unmatched )")
    | Tok_LParen -> let (expr1, expr2) = (find_RParen rest 0 []) in (
      match (lookahead expr2) with 
      | Some(Tok_ID(str)) -> let (lst,expr) = parse_expr expr1 in  (lst, FunctionCall(expr, ID str))
      | Some(Tok_String(str)) -> let (lst,expr) = parse_expr expr1 in  (lst, FunctionCall(expr, Value (String str)))
      | Some(Tok_Int(x)) -> let (lst,expr) = parse_expr expr1 in  (lst, FunctionCall(expr, Value (Int x)))
      | Some(Tok_Bool(x)) -> let (lst,expr) = parse_expr expr1 in  (lst, FunctionCall(expr, Value (Bool x)))
      | Some(Tok_LParen) -> let (lst,expr) = parse_expr expr1 in (lst, FunctionCall(expr, (let(lst,expr) = parse_expr expr2 in expr)))
      | _ -> (expr2, (let (lst,expr) = parse_expr expr1 in expr))
    )
    | Tok_Equal ->    raise (InvalidInputException "invalid =")
    | Tok_NotEqual ->    raise (InvalidInputException "invalid <>")
    | Tok_Greater ->    raise (InvalidInputException "invalid >")
    | Tok_Less ->    raise (InvalidInputException "invalid <")
    | Tok_GreaterEqual ->    raise (InvalidInputException "invalid >=")
    | Tok_LessEqual ->    raise (InvalidInputException "invalid <=")
    | Tok_Or ->    raise (InvalidInputException "||")
    | Tok_And ->    raise (InvalidInputException "&&")
    | Tok_Not -> let (lst,expr) = parse_expr rest in (lst, Not(expr))
    | Tok_If -> let (lst1, lst4) = (find_then rest 0 0 []) in (
      let (lst2, lst3) = (find_else lst4 0 0 []) in (
        let (lst, expr1) = parse_expr lst1 in (
          let (lst, expr2) = parse_expr lst2 in (
            let (lst, expr3) = parse_expr lst3 in (
              (lst, If(expr1,expr2,expr3))
            )
          )
        )
      )
    )
    | Tok_Then ->    raise (InvalidInputException "invalid then")
    | Tok_Else ->    raise (InvalidInputException "invalid else")
    | Tok_Add ->    raise (InvalidInputException "invalid +")
    | Tok_Sub ->    raise (InvalidInputException "invalid -")
    | Tok_Mult ->    raise (InvalidInputException "invalid *")
    | Tok_Div ->    raise (InvalidInputException "invalid /")
    | Tok_Concat ->    raise (InvalidInputException "invalid ^")
    | Tok_Let -> (
      match (lookahead rest) with
      | Some(Tok_ID(str)) -> (let (expr1, expr2) = (find_in (move_forward rest 2) 0 []) in
        let (lst,expr20) = (parse_expr expr2) in 
       (lst, Let(str, false, (let (lst,expr) = (parse_expr expr1) in expr), expr20)))
      | Some(Tok_Rec) -> (let (expr1, expr2) = (find_in (move_forward rest 3) 0 []) in
         let (lst,expr20) = (parse_expr expr2) in
        (match (lookahead_many rest 1) with
         | Some(Tok_ID(str)) -> ([], Let(str, true, (let (lst,expr) = (parse_expr expr1) in expr), expr20))
         | _ -> raise (InvalidInputException "parsing error: ID expected after \"let rec\"")
         ))
      | _ -> raise (InvalidInputException "parsing error: ID or \"rec\" expected after \"let\"")
      )
    | Tok_Rec ->    raise (InvalidInputException "rec in wrong location")
    | Tok_In ->    raise (InvalidInputException "unmatched in")
    | Tok_Def ->    raise (InvalidInputException "not toplevel")
    | Tok_Fun -> (
      match (lookahead rest) with
      | Some(Tok_ID(str)) -> (
        match (lookahead_many rest 1) with
        | Some(Tok_Arrow) -> (
          let (lst,expr) = parse_expr (find_Arrow rest 0) in (lst,Fun(str,expr))
          )
        | _ -> raise (InvalidInputException "parsing error: -> expected after fun \"ID\" ")
        )
      | _ -> raise (InvalidInputException "parsing error: variable expected after \"fun\"")
      )
    | Tok_Arrow ->    raise (InvalidInputException "unmatched ->")
    | Tok_Int(x) -> if lookahead toks = None then (rest, Value(Int x)) 
      else (
        match (move_forward toks 1) with
        | [] -> ([], Value(Int x)) 
        | g::t -> (
          match g with
          | Tok_Add -> let (lst,expr) = parse_expr (move_forward toks 2) in (lst, Binop(Add, Value(Int x), expr))
          | Tok_Sub -> let (lst,expr) = parse_expr (move_forward toks 2) in (lst, Binop(Sub, Value(Int x), expr))
          | Tok_Mult -> let (lst,expr) = parse_expr (move_forward toks 2) in (lst, Binop(Mult, Value(Int x), expr))
          | Tok_Div -> let (lst,expr) = parse_expr (move_forward toks 2) in (lst, Binop(Div, Value(Int x), expr))
          | Tok_Concat -> raise (InvalidInputException "Incompatible operand for integer")
          | Tok_Greater -> let (lst,expr) = parse_expr (move_forward toks 2) in (lst, Binop(Greater, Value(Int x), expr))
          | Tok_Less -> let (lst,expr) = parse_expr (move_forward toks 2) in (lst, Binop(Less, Value(Int x), expr))
          | Tok_GreaterEqual -> let (lst,expr) = parse_expr (move_forward toks 2) in (lst, Binop(GreaterEqual, Value(Int x), expr))
          | Tok_LessEqual -> let (lst,expr) = parse_expr (move_forward toks 2) in (lst, Binop(LessEqual, Value(Int x), expr))
          | Tok_Equal -> let (lst,expr) = parse_expr (move_forward toks 2) in (lst, Binop(Equal, Value(Int x), expr))
          | Tok_NotEqual -> let (lst,expr) = parse_expr (move_forward toks 2) in (lst, Binop(NotEqual, Value(Int x), expr))
          | Tok_Or -> raise (InvalidInputException "Incompatible operand for integer")
          | Tok_And -> raise (InvalidInputException "Incompatible operand for integer")
          | Tok_DoubleSemi -> raise (InvalidInputException "Not an operand")
          | _ -> raise (InvalidInputException "Not an operand")
          )
      )
    | Tok_Bool(x) -> if lookahead toks = None then (rest, Value(Bool x)) 
    else (
      match (move_forward toks 1) with
      | [] -> ([], Value(Bool x)) 
      | g::t -> (
        match g with
        | Tok_Add -> raise (InvalidInputException "Incompatible operand for boolean")
        | Tok_Sub -> raise (InvalidInputException "Incompatible operand for boolean")
        | Tok_Mult -> raise (InvalidInputException "Incompatible operand for boolean")
        | Tok_Div -> raise (InvalidInputException "Incompatible operand for boolean")
        | Tok_Concat -> raise (InvalidInputException "Incompatible operand for boolean")
        | Tok_Greater -> let (lst,expr) = parse_expr (move_forward toks 2) in (lst, Binop(Greater, Value(Bool x), expr))
        | Tok_Less -> let (lst,expr) = parse_expr (move_forward toks 2) in (lst, Binop(Less, Value(Bool x), expr))
        | Tok_GreaterEqual -> let (lst,expr) = parse_expr (move_forward toks 2) in (lst, Binop(GreaterEqual, Value(Bool x), expr))
        | Tok_LessEqual -> let (lst,expr) = parse_expr (move_forward toks 2) in (lst, Binop(LessEqual, Value(Bool x), expr))
        | Tok_Equal -> let (lst,expr) = parse_expr (move_forward toks 2) in (lst, Binop(Equal, Value(Bool x), expr))
        | Tok_NotEqual -> let (lst,expr) = parse_expr (move_forward toks 2) in (lst, Binop(NotEqual, Value(Bool x), expr))
        | Tok_Or -> let (lst,expr) = parse_expr (move_forward toks 2) in (lst, Binop(Or, Value(Bool x), expr))
        | Tok_And -> let (lst,expr) = parse_expr (move_forward toks 2) in (lst, Binop(And, Value(Bool x), expr))
        | Tok_DoubleSemi -> raise (InvalidInputException "Not an operand")
        | _ -> raise (InvalidInputException "Not an operand")
        )
    )
    | Tok_String(str) -> if lookahead toks = None then (rest, Value(String str)) 
    else (
      match (move_forward toks 1) with
      | [] -> ([], Value(String str)) 
      | g::t -> (
        match g with
        | Tok_Add -> raise (InvalidInputException "Incompatible operand for string")
        | Tok_Sub -> raise (InvalidInputException "Incompatible operand for string")
        | Tok_Mult -> raise (InvalidInputException "Incompatible operand for string")
        | Tok_Div -> raise (InvalidInputException "Incompatible operand for string")
        | Tok_Concat -> let (lst,expr) = parse_expr (move_forward toks 2) in (lst, Binop(Concat, Value(String str), expr))
        | Tok_Greater -> let (lst,expr) = parse_expr (move_forward toks 2) in (lst, Binop(Greater, Value(String str), expr))
        | Tok_Less -> let (lst,expr) = parse_expr (move_forward toks 2) in (lst, Binop(Less, Value(String str), expr))
        | Tok_GreaterEqual -> let (lst,expr) = parse_expr (move_forward toks 2) in (lst, Binop(GreaterEqual, Value(String str), expr))
        | Tok_LessEqual -> let (lst,expr) = parse_expr (move_forward toks 2) in (lst, Binop(LessEqual, Value(String str), expr))
        | Tok_Equal -> let (lst,expr) = parse_expr (move_forward toks 2) in (lst, Binop(Equal, Value(String str), expr))
        | Tok_NotEqual -> let (lst,expr) = parse_expr (move_forward toks 2) in (lst, Binop(NotEqual, Value(String str), expr))
        | Tok_Or -> raise (InvalidInputException "Incompatible operand for string")
        | Tok_And -> raise (InvalidInputException "Incompatible operand for string")
        | Tok_DoubleSemi -> raise (InvalidInputException "Not an operand")
        | _ -> raise (InvalidInputException "Not an operand")
        )
    )
    | Tok_ID(str) -> if lookahead toks = None then (rest, ID str) 
    else (
      match (move_forward toks 1) with
      | [] -> ([], ID str) 
      | g::t -> (
        match g with
        | Tok_Add -> let (lst,expr) = parse_expr (move_forward toks 2) in (lst, Binop(Add, ID str, expr))
        | Tok_Sub -> let (lst,expr) = parse_expr (move_forward toks 2) in (lst, Binop(Sub, ID str, expr))
        | Tok_Mult -> let (lst,expr) = parse_expr (move_forward toks 2) in (lst, Binop(Mult, ID str, expr))
        | Tok_Div -> let (lst,expr) = parse_expr (move_forward toks 2) in (lst, Binop(Div, ID str, expr))
        | Tok_Concat -> let (lst,expr) = parse_expr (move_forward toks 2) in (lst, Binop(Concat, ID str, expr))
        | Tok_Greater -> let (lst,expr) = parse_expr (move_forward toks 2) in (lst, Binop(Greater, ID str, expr))
        | Tok_Less -> let (lst,expr) = parse_expr (move_forward toks 2) in (lst, Binop(Less, ID str, expr))
        | Tok_GreaterEqual -> let (lst,expr) = parse_expr (move_forward toks 2) in (lst, Binop(GreaterEqual, ID str, expr))
        | Tok_LessEqual -> let (lst,expr) = parse_expr (move_forward toks 2) in (lst, Binop(LessEqual, ID str, expr))
        | Tok_Equal -> let (lst,expr) = parse_expr (move_forward toks 2) in (lst, Binop(Equal, ID str, expr))
        | Tok_NotEqual -> let (lst,expr) = parse_expr (move_forward toks 2) in (lst, Binop(NotEqual, ID str, expr))
        | Tok_Or -> let (lst,expr) = parse_expr (move_forward toks 2) in (lst, Binop(Or, ID str, expr))
        | Tok_And -> let (lst,expr) = parse_expr (move_forward toks 2) in (lst, Binop(And, ID str, expr))
        | Tok_ID(str1) -> (t,FunctionCall(ID str, ID str1))
        | Tok_Int(x) -> (t,FunctionCall(ID str, Value (Int x)))
        | Tok_String(str1) -> (t,FunctionCall(ID str, Value (String str1)))
        | Tok_Bool(x) -> (t,FunctionCall(ID str, Value (Bool x)))
        | Tok_LParen -> let (lst, expr) = parse_expr (move_forward toks 1) in (lst, FunctionCall(ID str, expr))
        | Tok_DoubleSemi -> raise (InvalidInputException "Not an operand")
        | _ -> raise (InvalidInputException "Not an operand")
        )
    )
    | Tok_DoubleSemi -> raise (InvalidInputException "Unnecessary ;;")
    )

(* Part 3: Parsing mutop *)

let rec find_doubleSemi (toks: token list) (read_toks: token list) = 
  match toks with
  | [] -> raise (InvalidInputException "parsing error: no ;;")
  | h::rest -> (match h with
                | Tok_DoubleSemi -> (read_toks, rest)
                | _ -> find_doubleSemi rest (read_toks@[h]))

let rec parse_mutop toks = 
  let (expr1, expr2) = find_doubleSemi toks [] in
    match toks with
    | h::rest -> (
      match h with 
      | Tok_DoubleSemi -> (expr2, NoOp)
      | Tok_Def -> (
        match (lookahead rest) with
        | Some(Tok_ID(str)) -> (
          match (lookahead_many rest 1) with
          | Some(Tok_Equal) -> let (lst,expr) = parse_expr (move_forward expr1 3) in (expr2, Def(str, expr))
          | _ -> raise (InvalidInputException "= expected after Def \"ID\"")
          )
        | _ -> raise (InvalidInputException "token ID expected after Def")
      )
      | _ -> (expr2, let (lst,expr) = parse_expr expr1 in (Expr expr))
    )
    |[] -> raise (InvalidInputException "parsing error: no ;;")