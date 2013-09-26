open Lcombinators.GenericParsing
open Lcombinators.CharParsing

(* the datatype for tokens -- you will need to augment these *)
type token = 
    INT of int 
  | PLUS | MINUS | STAR | SLASH
  | NOT
  | LPAREN | RPAREN
  | EQUAL | NEQ | GTE | LTE | GT | LT
  | WHITESPACE | COMMENT 
  | SEMI
  | RETURN
  | EOF

(* removes WHITESPACE and COMMENT tokens from a token list *)
let remove_whitespace (ts: token list) : token list =
  let p = fun a t -> match t with (WHITESPACE | COMMENT) -> a | _ -> t::a in
  List.rev (List.fold_left p [] ts)

(* the tokenize function -- should convert a list of characters to a list of 
 * Fish tokens using the combinators. *)
let rec tokenize(cs:char list) : token list = 
  let ws_parser = const_map WHITESPACE white in
  let comment_parser = const_map COMMENT comment in
  let int_parser = map (fun i -> INT i) integer in
  let plus_parser = const_map PLUS (c '+') in
  let minus_parser = const_map MINUS (c '-')  in
  let star_parser = const_map STAR (c '*')  in
  let slash_parser = const_map SLASH (c '/')  in
  let lparen_parser = const_map LPAREN (c '(') in
  let rparen_parser = const_map RPAREN (c ')') in
  let equal_parser = const_map EQUAL (str "==") in 
  let neq_parser = const_map NEQ (str "!=") in
  let gte_parser =  const_map GTE (str ">=") in
  let lte_parser = const_map LTE (str "<=") in
  let gt_parser = const_map GT (c '>') in
  let lt_parser = const_map LT (c '<') in
  let not_parser = const_map NOT (c '!') in
  let return_parser = const_map RETURN (str "return") in
  let semi_parser = const_map SEMI (c ';') in
  let all_tokens = [int_parser; ws_parser; comment_parser; 
    plus_parser; minus_parser; star_parser; slash_parser;
    lparen_parser; rparen_parser; equal_parser; neq_parser;
    gte_parser; lte_parser; gt_parser; lt_parser; not_parser; 
    return_parser; semi_parser] in
  let eof_parser = map (fun _ -> EOF) eof in
  let p = seq (star (alts all_tokens), eof_parser) in
  match run (p cs) with
   | Some (tokens, EOF) -> remove_whitespace tokens
   | _ -> failwith "lex error"
