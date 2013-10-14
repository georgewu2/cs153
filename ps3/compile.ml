(* Compile Cish AST to MIPS AST *)
open Ast
open Mips


exception IMPLEMENT_ME

type result = { code : Mips.inst list;
                data : Mips.label list }

(* generate fresh labels *)
let label_counter = ref 0
let new_int() = (label_counter := (!label_counter) + 1; !label_counter)
let new_label() = "L" ^ (string_of_int (new_int()))

module VarMap = Map.Make (String)

type envir = {epilogue : label; map : int VarMap.t} 

let new_env (epi : label) : envir = {epilogue = epi; map = VarMap.empty;}
let length (env : envir) : int = List.length (VarMap.bindings env.map)
let add_var (v : Ast.var) (env : envir) : envir = 
	if VarMap.mem v env.map then env
	else
		let offset  = (length env) * 4 in
		let map = VarMap.add v offset env.map in
		{epilogue = env.epilogue; map = map}
let get_offset (v : Ast.var) (env : envir) : int = 
	VarMap.find v env.map

let sp = R29
let fp = R30
let ra = R31

(* collect vars implemented funtionally *)
let rec collect_vars ((s,_) : Ast.stmt) (env : envir) : envir = 
    let rec collect_var ((e,_):Ast.exp) (env : envir) : envir = 
        match (e : Ast.rexp) with
            | Ast.Var v -> add_var v env
            | Ast.Binop(e1,_,e2) | Ast.And(e1,e2) | Ast.Or(e1,e2) ->
                collect_var e2 (collect_var e1 env)
            | Ast.Not e1  -> collect_var e1 env   
            | Ast.Assign(v,e1) -> 
                collect_var e1 (add_var v env)
            | _ -> env
    in
    match (s : Ast.rstmt) with
    | Ast.Exp e | Ast.Return e -> collect_var e env
    | Ast.Seq(s1,s2) ->  collect_vars s2 (collect_vars s1 env)     
    | Ast.If(e,s1,s2) -> collect_vars s2 (collect_vars s1 (collect_var e env))      
    | Ast.While(e,s) -> collect_vars s (collect_var e env)      
    | Ast.For(e1,e2,e3,s) -> 
        collect_vars s (collect_var e3 (collect_var e2 (collect_var e1 env)))
    | Ast.Let(v,e,s) -> collect_vars s (collect_var e (add_var v env))
 

let extract_name (f : Ast.func) : string =
	let Ast.Fn s = f in
	s.name

let collect_funcs (p :Ast.program) : Mips.label list = 
	List.map (fun f -> "func_"^extract_name f) p


let rec compile (p:Ast.program) : result = {code = []; data = [];}

let allocate_word : inst = Add (sp, sp, Immed (-4l))

(* we need to make an environment 
collect all vars
give them a place in our map
and then compile with env
*)
let compile_function (f:Ast.funcsig) : inst list = 

	let epi_l = new_label() in
	let env = collect_vars f.body (new_env epi_l) in

	let make_prologue (():unit) : inst list = 
		allocate_word::Sw(ra, sp, 0l)::allocate_word::Sw(fp, sp, 0l)::
		Add(fp, sp, Immed 4l)::Sw(R4, fp, 4l)::Sw(R5, fp, 8l)::
		Sw(R6, fp, 12l)::Sw(R7, fp, 16l)::[] in
	let make_epilogue (():unit) : inst list = 
		Label(epi_l)::Lw(ra, fp, 0l)::Lw(fp, fp, 4l)::
		Add(sp, sp, Immed 0l)::Jr(ra)::[] in
	let compile_stmt ((s, _):Ast.stmt) (env : envir) : inst list = [] 

in
	make_prologue () @ (compile_stmt f.Ast.body env) @ make_epilogue ()


let result2string (res:result) : string = 
    let code = res.code in
    let data = res.data in
    let strs = List.map (fun x -> (Mips.inst2string x) ^ "\n") code in
    let vaR8decl x = x ^ ":\t.word 0\n" in
    let readfile f =
      let stream = open_in f in
      let size = in_channel_length stream in
      let text = String.create size in
      let _ = really_input stream text 0 size in
		  let _ = close_in stream in 
      text in
	  let debugcode = readfile "print.asm" in
	    "\t.text\n" ^
	    "\t.align\t2\n" ^
	    "\t.globl main\n" ^
	    (String.concat "" strs) ^
	    "\n\n" ^
	    "\t.data\n" ^
	    "\t.align 0\n"^
	    (String.concat "" (List.map vaR8decl data)) ^
	    "\n" ^
	    debugcode 