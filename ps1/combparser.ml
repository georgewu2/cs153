(* This file should be extended to implement the Fish parser using the 
 * parsing combinator library, and the combinator-based lexer. *)
open Lcombinators.GenericParsing
open Comblexer
open Ast

let dummy_pos : pos = 0

let rec make_exp_parser (():unit) : (token, exp) parser =
  let int_parser = satisfy_opt (function INT i -> Some (Int i, dummy_pos) | _ -> None) in
  let sub_parser = seq (satisfy (fun t -> t == LPAREN), 
    lazy_seq (lazy (make_exp_parser ()), lazy (satisfy (fun t -> t == RPAREN)))) in 
  let sub_exp_parser = map (fun (_, (e, _)) -> e) sub_parser in
  let first_parser = alt (int_parser, sub_exp_parser) in
  let a_binop_parser = satisfy_opt (function 
    STAR -> Some Times | SLASH -> Some Div | _ -> None) in
  let b_exp_parser = seq (first_parser, lazy_seq (lazy a_binop_parser, lazy first_parser)) in
  let b_binop_parser = satisfy_opt (function
    PLUS -> Some Plus | MINUS -> Some Minus | _ -> None) in
  let c_exp_parser = seq (b_exp_parser, lazy_seq (lazy b_binop_parser, lazy b_exp_parser)) in
  let rest_parser = seq (first_parser, c_exp_parser) in
  let binop_parser = map (fun (e1, (op, e2)) -> (Binop (e1, op, e2), dummy_pos)) rest_parser in
  alts [binop_parser; first_parser]
(*and make_plus_minus (():unit) : (token, (binop * exp)) parser = 
  let plus_minus_exp_parser = alt (seq (make_times_div (), lazy_seq(lazy (satisfy (fun t -> t == PLUS)), 
    lazy (make_plus_minus ()))), make_times_div ()) in
  lazy_seq(lazy plus_minus_exp_parser, lazy (make_exp_parser()))
and make_times_div (():unit) : (token, (binop * exp)) parser = 
  let times_div_exp_parser = alt (seq (first_parser, lazy_seq(lazy (satisfy (fun t -> t == STAR)),
    lazy (times_div_exp_parser ()))), first_parser) in
  lazy_seq(lazy times_div_exp_parser, lazy (make_exp_parser ()))*)
(*and make_binop_rest (():unit) : (token, (binop * exp)) parser =*)

  (*let binop_comp_parser = satisfy_opt (function 
    EQUAL -> Some Eq | NEQ -> Some Neq | GTE -> Some Gte | LTE -> Some Lte | GT -> Some Gt |
    LT -> Some Lt | _ -> None) in
  lazy_seq (lazy binop_comp_parser, lazy (make_exp_parser ()))

    let binop_op_parser = satisfy_opt (function 
    STAR -> Some Times | SLASH -> Some Div | _ -> None) in
  let binop_plus_parser = satisfy_opt (function
    PLUS -> Some Plus | MINUS -> Some Minus | _ -> None) in*)

let rec make_stmt_parser (():unit) : (token, stmt) parser =
  let return_parser = seq (satisfy (fun t -> t == RETURN), lazy_seq (lazy (make_exp_parser ()), 
    lazy (satisfy (fun t -> t == SEMI)))) in
  let return_stmt_parser = map (fun (_, (e, _)) -> ((Return e), dummy_pos)) return_parser in
  let exp_stmt_parser = map (fun e -> (Exp e, dummy_pos)) (make_exp_parser ()) in
  alts [return_stmt_parser; exp_stmt_parser]

let parse(ts:token list) : program = 
  let program_parser = make_stmt_parser () in
  match run (program_parser ts) with
   | Some stmt -> stmt
   | None -> failwith "parse error"

let parse_string s =
  let cs = Explode.explode s in
  let tokens = tokenize cs in
  let ast = parse tokens in
  ast

let test = parse_string "return 4 * (1 == 2) + 2 * (2 == 2) + 1 * (3 == 2);"
let test2 = parse_string "return 2 + 3 * 4;"
let test3 = parse_string "return 2 * 3 + 4;"