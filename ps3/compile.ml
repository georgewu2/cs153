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

type envir = {epilogue : label; vars : int VarMap.t; args : int VarMap.t;} 

let num_vars (env : envir) : int = List.length (VarMap.bindings env.vars)
let num_args (env : envir) : int = List.length (VarMap.bindings env.args)
let length (env : envir) : int32 = Int32.of_int ((num_vars env) + 8)
let add_var (v : Ast.var) (env : envir) : envir = 
	if VarMap.mem v env.vars then env
	else
		let offset  = -1 * ((num_vars env) * 4 + 8) in
		let vars = VarMap.add v offset env.vars in
		{epilogue = env.epilogue; vars = vars; args = env.args;}
let add_arg (v : Ast.var) (env : envir) : envir = 
    if VarMap.mem v env.args then env
    else
        let offset  = ((num_args env) * 4 + 4) in
        let args = VarMap.add v offset env.args in
        {epilogue = env.epilogue; vars = env.vars; args = args}
let get_offset (v : Ast.var) (env : envir) : int32 = 
	let offset = 
        if VarMap.mem v env.vars
        then VarMap.find v env.vars
        else VarMap.find v env.args
    in
        Int32.of_int offset
let new_env (epi : label) (args: Ast.var list) : envir = 
    List.fold_right 
        (fun i a -> add_arg ("var_" ^ i) a) args
        {epilogue = epi; vars = VarMap.empty; args = VarMap.empty;}

let sp = R29
let fp = R30
let ra = R31

let allocate_word : inst = Add (sp, sp, Immed (-4l))
let deallocate_word : inst = Add (sp, sp, Immed (4l))
(* we need to make an environment 
collect all vars
give them a place in our map
and then compile with env
*)

let rec compile_stmt ((s,_):Ast.stmt) (env : envir) : inst list = 
    let rec compile_exp ((e,_):Ast.exp) (env : envir) : inst list =
        let push r = allocate_word::Sw (r, sp, 0l)::[] in
        let pop r = Lw (r, sp, 0l)::deallocate_word::[] in
        match (e:Ast.rexp) with 
            | Int i -> Li (R2, (Word32.fromInt i))::[] 
            | Var v -> Lw(R2, fp, get_offset ("var_" ^ v) env)::[]
            | Binop(e1,b,e2) -> 
                (
                    (compile_exp e1 env) @ push R2
                    (* Doing this order, we have transformed Binop(e1,b,e2) 
                       into putting e1 into R3, and e2 into R2, so we must 
                    switch the registers *) 
                    @ (compile_exp e2 env) @ pop R3
                    @ (match b with
                        | Plus -> Add(R2, R3, Reg R2)::[]
                        | Minus -> Sub(R2, R3, R2)::[]
                        | Times -> Mul(R2, R3, R2)::[]
                        | Ast.Div -> Mips.Div(R2, R3, R2)::[]
                        | Eq -> Mips.Seq(R2, R3, R2)::[]
                        | Neq -> Sne(R2, R3, R2)::[]
                        | Lt -> Slt(R2, R3, Reg R2)::[]
                        | Lte -> Sle(R2, R3, R2)::[]
                        | Gt -> Sgt (R2, R3, R2)::[]
                        | Gte -> Sge(R2, R3, R2)::[]
                        ))
			| Ast.And(e1,e2) -> 
                (let end_l = new_label() in
                    (compile_exp e1 env) @ Beq(R2,R0,end_l)::push R2 
                    @ (compile_exp e2 env) @ pop R3 @
                    Mips.And(R2, R2, Reg R3)::Label(end_l)::[])
            | Ast.Or(e1,e2) ->
                (let end_l = new_label() in
                    (compile_exp e1 env) @ Bne(R2,R0,end_l)::push R2 
                    @ (compile_exp e2 env) @ pop R3 @
                    Mips.Or(R2, R2, Reg R3)::Label(end_l)::[])
            
            | Not e  -> 
                (compile_exp e env) @ Mips.Seq(R2, R2, R0) ::[]
            | Assign(v,e) ->
                (compile_exp e env) @ Sw(R2, fp, get_offset ("var_" ^ v) env)::[]
            | Call (e, vars) -> 
	            let caller_prep = 
	            	if List.length vars > 4 
	            	then 
	            		match vars with
	            		| a0::a1::a2::a3::tl ->
	            			let rev_list = List.rev tl in
	            			(compile_exp a0 env) @ Or(R4, R0, Reg R2)::(compile_exp a1 env) @ 
	            			Or(R5, R0, Reg R2)::(compile_exp a2 env) @ Or(R6, R0, Reg R2)::
	            			(compile_exp a3 env) @ Or(R7, R0, Reg R2)::
	            			List.fold_right (fun i a -> allocate_word::(compile_exp i env) 
	            				@ Sw(R2, sp, 0l) :: a ) rev_list []
	            		| _ -> raise IMPLEMENT_ME
	            	else 
	            		match vars with
	            		| a0::a1::a2::a3::[] ->
	            			(compile_exp a0 env) @ Or(R4, R0, Reg R2)::(compile_exp a1 env) @ 
	            			Or(R5, R0, Reg R2)::(compile_exp a2 env) @ Or(R6, R0, Reg R2)::
	            			(compile_exp a3 env) @ Or(R7, R0, Reg R2)::[]
	            		| a0::a1::a2::[] ->
	            			(compile_exp a0 env) @ Or(R4, R0, Reg R2)::(compile_exp a1 env) @ 
	            			Or(R5, R0, Reg R2)::(compile_exp a2 env) @ Or(R6, R0, Reg R2)::[]
	            		| a0::a1::[] ->
	            			(compile_exp a0 env) @ Or(R4, R0, Reg R2)::(compile_exp a1 env) @ 
	            			Or(R5, R0, Reg R2)::[]
	            		| a0::[] ->
	            			(compile_exp a0 env) @ Or(R4, R0, Reg R2)::[]
	            		| _ -> []
	            in 
	            caller_prep @ Add(sp, sp, Immed (-16l))::Jal ("fun_"^e)::Add(sp, sp, Immed(16l))::[]
    in 
    match (s : Ast.rstmt) with
        | Return e -> (compile_exp e env) @ Add(R8, R2, Immed 0l)::J(env.epilogue)::[]
        | Exp e -> compile_exp e env
        | Ast.Seq(s1,s2) ->  compile_stmt s1 env @ compile_stmt s2 env
        | If(e,s1,s2) ->  
            (let else_l = new_label() in
             let end_l = new_label() in 
            (compile_exp e env) @ Beq(R2,R0,else_l)::[] @ 
            (compile_stmt s1 env) @ J(end_l)::Label(else_l)::[] @ 
            (compile_stmt s2 env) @ Label(end_l)::[])
        | While(e,s) -> 
            (let condition_l = new_label() in
             let top_l = new_label() in
            J(condition_l)::Label(top_l)::[] @ (compile_stmt s env) @ Label(condition_l)::[] @ 
            (compile_exp e env) @ Bne(R2,R0,top_l)::[])
        | For(e1,e2,e3,s) -> 
            compile_stmt (Ast.Seq((Exp e1,0),
                (While(e2,(Ast.Seq(s,(Exp e3, 0)), 0)), 0)), 0) env
        | Let (v,e,s) -> 
        	let env = add_var ("var_" ^ v) env in
        		compile_exp e env @ allocate_word::Sw(R2, fp, get_offset ("var_" ^ v) env)::compile_stmt s env

let compile_func ((Fn f):Ast.func) : inst list = 
	let epi_l = new_label() in
	let env = new_env epi_l (f.Ast.args) in

	let make_prologue (():unit) : inst list = 
		Label("fun_"^f.name)::allocate_word::Sw(ra, sp, 0l)::allocate_word::Sw(fp, sp, 0l)::
		Add(fp, sp, Immed 4l)::Sw(R4, fp, 4l)::Sw(R5, fp, 8l)::
		Sw(R6, fp, 12l)::Sw(R7, fp, 16l)::[] 
	in
	let make_epilogue (():unit) : inst list = 
		Label(epi_l)::Lw(ra, fp, 0l)::Lw(fp, fp, -4l)::
		Add(sp, sp, Immed (length env))::Jr(ra)::[] 
	in
	make_prologue () @ (compile_stmt f.body env) @ make_epilogue ()

let rec compile (p:Ast.program) : result = 
	let code = List.fold_right (fun i a -> compile_func i @ a) p [] in
	{code = code; data = [];}

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