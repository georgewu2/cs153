(* TODO:  your job is to map ScishAst expressions to CishAst functions. 
   The file sample_input.scish shows a sample Scish expression and the
   file sample_output.cish shows the output I get from my compiler.
   You will want to do your own test cases...
 *)
open Scish_ast
open Cish_ast

exception Unimplemented

(* tuple on a dummp pos *)
let (~@) x = (x,0)
let result = "result"

(* generate fresh labels *) 
let counter = ref 0
let new_int() = (counter := (!counter) + 1; !counter)
let new_temp() = "t" ^ (string_of_int (new_int()))

let (+$) (v,e) s = (Let(v, e, s), 0)
let (++) s1 s2 = (Seq(s1,s2),0)
let e2s rexp = (Exp(rexp,0),0)

let def_var v body tail = 
    (v,(Int 0,0)) +$ (body ++ (e2s (Assign(v, (Var result,0))) ) ++ tail ) 

let move_temp body t = 
    

    (Let(t, (Int 0,0), (Seq( body, (Exp(Assign(t, (Var result,0)),0),0) ),0)), 0)

let rec compile_exp (e:Scish_ast.exp) : Cish_ast.stmt = 
	match e with
	| Scish_ast.Int i -> (Exp(Assign(result, ((Int i), 0)), 0), 0)
	(*| Var x -> (* Update var in env *)*)
	| PrimApp (op, e_list) -> compile_primop op e_list
	| _ -> ~@Cish_ast.skip
	(*
	| Lambda (v, e) -> 
	| App (e1, e2) ->
	| If (e1, e2, e3) ->*)

and compile_primop (op:Scish_ast.primop) (e_list: Scish_ast.exp list) : Cish_ast.stmt = 
	match op, e_list with

	| Scish_ast.Plus, a::b::[] -> 
		let t1 = new_temp() in
		let t2 = new_temp() in
        let epi = (Exp(Assign(result, (Binop((Var t1, 0), Plus, (Var t2, 0)), 0)),0),0) in
        let body2 = def_var t2 (compile_exp b) epi in
        let body = def_var t1 (compile_exp a) body2  in
        body


        (*
	| Minus, a::b::[] -> Binop(a, Minus, b)
	| Times, a::b::[] -> Binop(a, Times, b)
	| Div, a::b::[] -> Binop(a, Div, b)
	| Cons, a::b::[] -> 
	| Fst. a::[] -> 
	| Snd, a::[] ->
	| Eq, a::b::[] -> Binop(a, Eq, b)
	| Lt, a::b::[] -> Binop(a, Lt, b)*)

let rec compile e =
	let body = compile_exp e in
	let body = (Let(result, (Int 0,0), ((Seq(body,((Return (Var result,0)),0))),0)), 0 )    in
    Fn({name="main"; args=[]; body = body; pos = 0})::[]

