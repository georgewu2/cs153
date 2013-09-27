(* This file should be extended to implement the Fish parser using the 
 * parsing combinator library, and the combinator-based lexer. *)
open Lcombinators.GenericParsing
open Comblexer
open Ast

let dummy_pos : pos = 0

let rec make_exp_parser (():unit) : (token, exp) parser =
  let int_parser = satisfy_opt (function INT i -> Some (Int i, dummy_pos) | _ -> None) in
  let var_parser = satisfy_opt (function VAR i -> Some (Var i, dummy_pos) | _ -> None) in
  let sub_parser = seq (satisfy (fun t -> t == LPAREN), 
    lazy_seq (lazy (make_gexp_parser ()), lazy (satisfy (fun t -> t == RPAREN)))) in 
  let sub_exp_parser = map (fun (_, (e, _)) -> e) sub_parser in
  let first_parser = alts [int_parser; var_parser; sub_exp_parser] in
  first_parser

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

and make_eexp_parser (():unit) : (token, exp) parser = 
  let rest_parser = seq (make_dexp_parser (), make_eexp_rest ()) in
  let eexp_parser = map (fun (e1, (op, e2)) -> (And (e1, e2), dummy_pos)) rest_parser in
  alts [eexp_parser; make_dexp_parser ()]
and make_eexp_rest (():unit) : (token, (token * exp)) parser = 
  let op_parser = satisfy (fun t -> t == AND) in
  lazy_seq (lazy op_parser, lazy (make_eexp_parser ()))

and make_fexp_parser (():unit) : (token, exp) parser = 
  let rest_parser = seq (make_eexp_parser (), make_fexp_rest ()) in
  let fexp_parser = map (fun (e1, (op, e2)) -> (Or (e1, e2), dummy_pos)) rest_parser in
  alts [fexp_parser; make_eexp_parser ()]
and make_fexp_rest (():unit) : (token, (token * exp)) parser = 
  let op_parser = satisfy (fun t -> t == OR) in
  lazy_seq (lazy op_parser, lazy (make_fexp_parser ()))

and make_gexp_parser (():unit) : (token, exp) parser = 
  let var_parser = satisfy_opt (function VAR i -> Some i | _ -> None) in
  let rest_parser = seq (var_parser, make_gexp_rest ()) in
  let gexp_parser = map (fun (v, (_, e)) -> (Assign (v, e), dummy_pos)) rest_parser in
  alts [gexp_parser; make_fexp_parser ()]
and make_gexp_rest (():unit) : (token, (token * exp)) parser = 
  let op_parser = satisfy (fun t -> t == ASSIGN) in
  lazy_seq (lazy op_parser, lazy (make_gexp_parser ()))

let rec make_stmts_parser (():unit) : (token, stmt) parser = 
  let seq_parser = lazy_seq(lazy (make_stmt_parser ()), lazy (make_stmts_parser ())) in
  let seq_stmt_parser = map (fun (a,b) -> ((Seq (a,b), dummy_pos))) seq_parser in
  let empty_parser = always (Ast.skip, dummy_pos) in 
  alts [seq_stmt_parser; empty_parser]
and make_stmt_parser (():unit) : (token, stmt) parser =
  let sub_parser = seq (satisfy (fun t -> t == LBRACE), 
    lazy_seq (lazy (make_stmts_parser ()), lazy (satisfy (fun t -> t == RBRACE)))) in 
  let sub_stmt_parser = map (fun (_, (s, _)) -> s) sub_parser in
  let return_parser = seq (satisfy (fun t -> t == RETURN), lazy_seq (lazy (make_gexp_parser ()), 
    lazy (satisfy (fun t -> t == SEMI)))) in
  let return_stmt_parser = map (fun (_, (e, _)) -> ((Return e), dummy_pos)) return_parser in
  let exp_parser = seq (make_gexp_parser (), satisfy (fun t -> t == SEMI)) in
  let exp_stmt_parser = map (fun (e, _) -> (Exp e, dummy_pos)) exp_parser  in
  alts [sub_stmt_parser; return_stmt_parser; exp_stmt_parser; (make_if_else_parser ()); 
    (make_if_parser ()); (make_while_parser ()); (make_for_parser ())]
and make_while_parser ((): unit) : (token, stmt) parser = 
  let while_parser = satisfy (fun t -> t == WHILE) in
  let all_parser = seq(while_parser, lazy_seq(lazy (make_gexp_parser ()), lazy (make_stmt_parser ()))) in
  map (fun (_, (e, s)) -> ((While (e,s)), dummy_pos)) all_parser
and make_for_parser ((): unit) : (token, stmt) parser = 
  let rest_parser = lazy_seq (lazy (satisfy (fun t -> t == RPAREN)), lazy (make_stmt_parser ())) in
  let rest2_parser = lazy_seq (lazy (make_gexp_parser ()), lazy (rest_parser)) in
  let rest3_parser = lazy_seq (lazy (satisfy (fun t -> t == SEMI)), lazy (rest2_parser)) in
  let rest4_parser = lazy_seq (lazy (make_gexp_parser ()), lazy (rest3_parser)) in
  let rest5_parser = lazy_seq (lazy (satisfy (fun t -> t == SEMI)), lazy (rest4_parser)) in
  let rest6_parser = lazy_seq (lazy (make_gexp_parser ()), lazy (rest5_parser)) in
  let for_parser = seq (satisfy (fun t -> t == FOR), 
    lazy_seq(lazy (satisfy (fun t -> t == LPAREN)), lazy rest6_parser)) in
  map (fun (_, (_, (e1, (_, (e2, (_, (e3, (_, s)))))))) -> 
    (For (e1, e2, e3, s), dummy_pos)) for_parser
and make_if_else_parser (():unit) : (token, stmt) parser = 
  let if_parser = satisfy (fun t -> t == IF) in
  let else_parser = lazy_seq(lazy (satisfy (fun t -> t == ELSE)), lazy(make_stmt_parser ())) in
  let rest_parser = lazy_seq(lazy (make_cstmt_parser ()), lazy else_parser) in
  let rest2_parser = lazy_seq(lazy (make_gexp_parser ()), lazy rest_parser) in
  let if_else_parser = seq (if_parser, rest2_parser) in
  map (fun (_, (e, (s1,(_, s2)))) -> ((If (e, s1, s2)), dummy_pos)) if_else_parser
and make_if_parser (():unit) : (token, stmt) parser = 
  let if_parser = satisfy (fun t -> t == IF) in
  let rest_parser = seq(if_parser, lazy_seq(lazy (make_gexp_parser ()), lazy (make_stmt_parser ()))) in
  map (fun (_, (e, s1)) -> If(e, s1, (Ast.skip, dummy_pos)), dummy_pos) rest_parser
(* Equivalents for c statements*)
and make_cstmt_parser (():unit) : (token, stmt) parser =
  let sub_parser = seq (satisfy (fun t -> t == LBRACE), 
    lazy_seq (lazy (make_stmts_parser ()), lazy (satisfy (fun t -> t == RBRACE)))) in 
  let sub_stmt_parser = map (fun (_, (s, _)) -> s) sub_parser in
  let return_parser = seq (satisfy (fun t -> t == RETURN), lazy_seq (lazy (make_gexp_parser ()), 
    lazy (satisfy (fun t -> t == SEMI)))) in
  let return_stmt_parser = map (fun (_, (e, _)) -> ((Return e), dummy_pos)) return_parser in
  let exp_parser = seq (make_gexp_parser (), satisfy (fun t -> t == SEMI)) in
  let exp_stmt_parser = map (fun (e, _) -> (Exp e, dummy_pos)) exp_parser  in
  alts [sub_stmt_parser; return_stmt_parser; exp_stmt_parser; (make_if_else_parser ()); 
    (make_while_parser ()); (make_for_parser ())]

let parse(ts:token list) : program = 
  let program_parser = make_stmts_parser () in
  match run (program_parser ts) with
   | Some stmt -> stmt
   | None -> failwith "parse error"
