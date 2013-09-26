open Lcombinators.GenericParsing
open Lcombinators.CharParsing

(* the datatype for tokens -- you will need to augment these *)
type token = 
    INT of int 
  | VAR of string
  | EOF
  | RETURN | IF | ELSE | WHILE | FOR
  | PLUS | MINUS | STAR | SLASH
  | LPAREN | RPAREN | LBRACE | RBRACE
  | EQUAL | NEQ | LT | LTE | GT | GTE
  | NOT | AND | OR
  | ASSIGN | SEMI
  | WHITESPACE | COMMENT 


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
  let id_parser = map (fun i -> VAR i) identifier in

  let str_pairs = 
  [  (str "return", RETURN); (str "if", IF); (str "else", ELSE);
  (str "while", WHILE); (str "for", FOR); (str "==", EQUAL); (str "!=", NEQ);
  (str ">=", GTE); (str "<=", LTE); (str "&&", AND); (str "||", OR); ] in
  let c_pairs = 
  [ (c '+', PLUS); (c '-', MINUS); (c '*', STAR); (c '/', SLASH);
  (c '(', LPAREN); (c ')', RPAREN); (c '{', LBRACE); (c '}', RBRACE);
  (c '>', GT); (c '<', LT); (c '!', NOT); (c '=', ASSIGN); (c ';', SEMI); ] in

  let str_parser = 
    List.map (function (code, t) -> const_map t code) str_pairs in
  let c_parser = 
    List.map (function (code, t) -> const_map t code) c_pairs in

  let all_tokens = [ws_parser; comment_parser; 
  int_parser; (alts c_parser); (alts str_parser); id_parser] in
  let eof_parser = map (fun _ -> EOF) eof in
  let p = seq (star (alts all_tokens), eof_parser) in
  match run (p cs) with
   | Some (tokens, EOF) -> remove_whitespace tokens
   | _ -> failwith "lex error"
