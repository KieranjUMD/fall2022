open TokenTypes

(* PASTE YOUR LEXER FROM P4A HERE *)

open String
open Str

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let without text matched = 
  sub text (length matched) (length text - length matched)


let rec mklst input = 
  let re_LParen = Str.regexp "(" in   
  let re_RParen = Str.regexp ")" in   
  let re_Equal = Str.regexp "=" in   
  let re_NotEqual = Str.regexp "<>" in   
  let re_Greater = Str.regexp ">" in   
  let re_Less = Str.regexp "<" in   
  let re_GreaterEqual = Str.regexp ">=" in   
  let re_LessEqual = Str.regexp "<=" in   
  let re_Or = Str.regexp "||" in 
  let re_And = Str.regexp "\\&\\&" in (*needs \*)   
  let re_Not = Str.regexp "not " in   
  let re_If = Str.regexp "if " in   
  let re_Then = Str.regexp "then " in 
  let re_Else = Str.regexp "else " in 
  let re_Add = Str.regexp "\\+" in (*needs \*)
  let re_Sub = Str.regexp "-" in    
  let re_Mult = Str.regexp "*" in 
  let re_Div = Str.regexp "/" in 
  let re_Concat = Str.regexp "\\^" in  (*needs \*)
  let re_Let = Str.regexp "let " in 
  let re_Rec = Str.regexp "rec " in 
  let re_In = Str.regexp "in " in 
  let re_Def = Str.regexp "def " in 
  let re_Fun = Str.regexp "fun " in 
  let re_Arrow = Str.regexp "->" in       
  let re_num = Str.regexp "[0-9]+" in         
  let re_neg_num = Str.regexp "(-[0-9]+)" in  
  let re_Bool_true = Str.regexp "true " in 
  let re_Bool_false = Str.regexp "false " in 
  let re_String = Str.regexp "\"[^\"]*\"" in 
  let re_ID = Str.regexp "[a-zA-Z][a-zA-Z0-9]*" in 
  let re_DoubleSemi = Str.regexp ";;" in    
    let text = (concat "" [(trim input); " "]) in
      if text = " " then [] else 
        if (string_match re_num text 0) then
          let matched = matched_string text in
            Tok_Int(int_of_string matched)::mklst(without text matched)
        else if (string_match re_neg_num text 0) then
          let matched = matched_string text in
            Tok_Int(int_of_string (sub matched 1 ((length matched)-2)))::mklst(without text matched)
        else if (string_match re_DoubleSemi text 0) then
          let matched = matched_string text in
            Tok_DoubleSemi::mklst(without text matched)
        else if (string_match re_Arrow text 0) then
          let matched = matched_string text in
            Tok_Arrow::mklst(without text matched)
        else if (string_match re_Sub text 0) then
          let matched = matched_string text in
            Tok_Sub::mklst(without text matched)
        else if (string_match re_NotEqual text 0) then
          let matched = matched_string text in
            Tok_NotEqual::mklst(without text matched)
        else if (string_match re_LessEqual text 0) then
          let matched = matched_string text in
            Tok_LessEqual::mklst(without text matched)
        else if (string_match re_GreaterEqual text 0) then
          let matched = matched_string text in
            Tok_GreaterEqual::mklst(without text matched)
        else if (string_match re_Equal text 0) then
          let matched = matched_string text in
            Tok_Equal::mklst(without text matched)
        else if (string_match re_Greater text 0) then
          let matched = matched_string text in
            Tok_Greater::mklst(without text matched)
        else if (string_match re_Less text 0) then
          let matched = matched_string text in
            Tok_Less::mklst(without text matched)
        else if (string_match re_RParen text 0) then
          let matched = matched_string text in
            Tok_RParen::mklst(without text matched)
        else if (string_match re_LParen text 0) then
          let matched = matched_string text in
            Tok_LParen::mklst(without text matched)
        else if (string_match re_Or text 0) then
          let matched = matched_string text in
            Tok_Or::mklst(without text matched)
        else if (string_match re_And text 0) then
          let matched = matched_string text in
            Tok_And::mklst(without text matched)
        else if (string_match re_Not text 0) then
          let matched = matched_string text in
            Tok_Not::mklst(without text matched)
        else if (string_match re_If text 0) then
          let matched = matched_string text in
            Tok_If::mklst(without text matched)
        else if (string_match re_Then text 0) then
          let matched = matched_string text in
            Tok_Then::mklst(without text matched)
        else if (string_match re_Else text 0) then
          let matched = matched_string text in
            Tok_Else::mklst(without text matched)
        else if (string_match re_Add text 0) then
          let matched = matched_string text in
            Tok_Add::mklst(without text matched)
        else if (string_match re_Mult text 0) then
          let matched = matched_string text in
            Tok_Mult::mklst(without text matched)
        else if (string_match re_Div text 0) then
          let matched = matched_string text in
            Tok_Div::mklst(without text matched)
        else if (string_match re_Concat text 0) then
          let matched = matched_string text in
            Tok_Concat::mklst(without text matched)
        else if (string_match re_Let text 0) then
          let matched = matched_string text in
            Tok_Let::mklst(without text matched)
        else if (string_match re_Rec text 0) then
          let matched = matched_string text in
            Tok_Rec::mklst(without text matched)
        else if (string_match re_In text 0) then
          let matched = matched_string text in
            Tok_In::mklst(without text matched)
        else if (string_match re_Def text 0) then
          let matched = matched_string text in
            Tok_Def::mklst(without text matched)
        else if (string_match re_Fun text 0) then
          let matched = matched_string text in
            Tok_Fun::mklst(without text matched)
        else if (string_match re_Bool_true text 0) then
          let matched = matched_string text in
            Tok_Bool(true)::mklst(without text matched)
        else if (string_match re_Bool_false text 0) then
          let matched = matched_string text in
            Tok_Bool(false)::mklst(without text matched)
        else if (string_match re_String text 0) then
          let matched = matched_string text in
            Tok_String(sub text 1 ((length matched)-2))::mklst(without text matched)
        else if (string_match re_ID text 0) then
          let matched = matched_string text in
            Tok_ID(matched)::mklst(without text matched)
        else failwith "lexing error"

let tokenize input = mklst input