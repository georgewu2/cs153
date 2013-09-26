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
  let rest_parser = seq (first_parser, make_dexp_rest ()) in
  let binop_parser = map (fun (e1, (op, e2)) -> (Binop (e1, op, e2), dummy_pos)) rest_parser in
  alts [binop_parser; first_parser]

and make_aexp_parser (():unit) : (token, exp) parser = 
  let rest_parser = make_aexp_rest () in
  let aexp_parser = map (fun (op, e2) -> (Binop ((Int 0, dummy_pos), op, e2), dummy_pos)) rest_parser in
  alts [aexp_parser; make_exp_parser ()]
and make_aexp_rest (():unit) : (token, (binop * exp)) parser =
  let unary_parser = satisfy_opt (function 
     MINUS -> Some Minus | _ -> None) in
  lazy_seq (lazy unary_parser, lazy (make_aexp_parser ()))

and make_not_exp_parser (():unit) : (token, exp) parser = 
  let rest_parser = make_not_exp_rest () in
  let not_exp_parser = map (fun (_, e) -> (Not (e), dummy_pos)) rest_parser in
  alts [not_exp_parser; make_aexp_parser ()]
and make_not_exp_rest (():unit) : (token, (token * exp)) parser =
  let unary_parser = satisfy (fun t -> t == NOT) in
  lazy_seq (lazy unary_parser, lazy (make_not_exp_parser ()))

and make_bexp_parser (():unit) : (token, exp) parser = 
  let rest_parser = seq (make_not_exp_parser (), make_bexp_rest ()) in
  let bexp_parser = map (fun (e1, (op, e2)) -> (Binop (e1, op, e2), dummy_pos)) rest_parser in
  alts [bexp_parser; make_not_exp_parser ()]
and make_bexp_rest (():unit) : (token, (binop * exp)) parser =
  let op_parser = satisfy_opt (function 
    STAR -> Some Times | SLASH -> Some Div | _ -> None) in
  lazy_seq (lazy op_parser, lazy (make_bexp_parser ()))

and make_cexp_parser (():unit) : (token, exp) parser = 
  let rest_parser = seq (make_bexp_parser (), make_cexp_rest ()) in
  let cexp_parser = map (fun (e1, (op, e2)) -> (Binop (e1, op, e2), dummy_pos)) rest_parser in
  alts [cexp_parser; make_bexp_parser ()]
and make_cexp_rest (():unit) : (token, (binop * exp)) parser = 
  let op_parser = satisfy_opt (function
    PLUS -> Some Plus | MINUS -> Some Minus | _ -> None) in
  lazy_seq (lazy op_parser, lazy (make_cexp_parser ()))

and make_dexp_parser (():unit) : (token, exp) parser = 
  let rest_parser = seq (make_cexp_parser (), make_dexp_rest ()) in
  let dexp_parser = map (fun (e1, (op, e2)) -> (Binop (e1, op, e2), dummy_pos)) rest_parser in
  alts [dexp_parser; make_cexp_parser ()]
and make_dexp_rest (():unit) : (token, (binop * exp)) parser = 
  let op_parser = satisfy_opt (function
    EQUAL -> Some Eq | NEQ -> Some Neq | GTE -> Some Gte | LTE -> Some Lte
    | GT -> Some Gt| LT -> Some Lt | _ -> None) in
  lazy_seq (lazy op_parser, lazy (make_dexp_parser ()))


(*and make_binop_rest (():unit) : (token, (binop * exp)) parser =
  let binop_op_parser = satisfy_opt (function 
    PLUS -> Some Plus | MINUS -> Some Minus | STAR -> Some Times | SLASH -> Some Div | _ -> None) in
  lazy_seq (lazy binop_op_parser, lazy (make_exp_parser ()))*)


let rec make_stmt_parser (():unit) : (token, stmt) parser =
  let return_parser = seq (satisfy (fun t -> t == RETURN), lazy_seq (lazy (make_dexp_parser ()), 
    lazy (satisfy (fun t -> t == SEMI)))) in
  let return_stmt_parser = map (fun (_, (e, _)) -> ((Return e), dummy_pos)) return_parser in
  let exp_stmt_parser = map (fun e -> (Exp e, dummy_pos)) (make_dexp_parser ()) in
  alts [return_stmt_parser; exp_stmt_parser]

let parse(ts:token list) : program = 
  let program_parser = make_stmt_parser () in
  match run (program_parser ts) with
   | Some stmt -> stmt
   | None -> failwith "parse error"

(*
let parse_string s =
  let cs = Explode.explode s in
  let tokens = tokenize cs in
  let ast = parse tokens in
  ast

let test = parse_string "return 4 * (1 == 2) + 2 * (2 == 2) + 1 * (3 == 2);"
let test2 = parse_string "return 2 + 3 * 4;"
let test3 = parse_string "return 2 * 3 + 4;"*)