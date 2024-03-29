(* George Wu and Roger Zurawicki *)
(* Compile Fish AST to MIPS AST *)
open Mips
open Ast

exception IMPLEMENT_ME

type result = { code : Mips.inst list;
                data : Mips.label list }

(* generate fresh labels *)
let label_counter = ref 0
let new_int() = (label_counter := (!label_counter) + 1; !label_counter)
let new_label() = "L" ^ (string_of_int (new_int()))

(* sets of variables -- Ocaml Set and Set.S *)
module VarSet = Set.Make(struct
                           type t = Ast.var
                           let compare = String.compare
                         end)

(* a table of variables that we need for the code segment *)
let variables : VarSet.t ref = ref (VarSet.empty)

(* This is the invariant return register *)
(* TODO we should express more of our used registers like this *)
let rreg = R2

(* generate a fresh temporary variable and store it in the variables set. *)
let rec new_temp() = 
    let t = "T" ^ (string_of_int (new_int())) in
    (* make sure we don't already have a variable with the same name! *)
    if VarSet.mem t (!variables) then new_temp()
    else (variables := VarSet.add t (!variables); t)

(* reset internal state *)
let reset() = (label_counter := 0; variables := VarSet.empty)

let zero = 0l

(* find all of the variables in a program and add them to
 * the set variables *)
let rec collect_vars ((s,_) : Ast.program) : unit = 
    let rec collect_var ((e,_):Ast.exp) : unit = 
        match (e : Ast.rexp) with
            | Var v -> variables := (VarSet.add ("var" ^ v) !variables)
            | Binop(e1,_,e2) | And(e1,e2) | Or(e1,e2) ->
                collect_var e1; collect_var e2
            | Not e  -> collect_var e   
            | Assign(v,e) -> 
                variables := VarSet.add ("var" ^ v) !variables;
                collect_var e
            | _ -> ()
    in
    
    match (s : Ast.rstmt) with
    | Exp e | Return e -> collect_var e
    | Seq(s1,s2) ->  collect_vars s1; collect_vars s2      
    | If(e,s1,s2) -> collect_var e; collect_vars s1; collect_vars s2      
    | While(e,s) -> collect_var e; collect_vars s        
    | For(e1,e2,e3,s) -> 
        collect_var e1; collect_var e2; collect_var e3; collect_vars s
        

(* compiles a Fish statement down to a list of MIPS instructions.
 * Note that a "Return" is accomplished by placing the resulting
 * value in R2 and then doing a Jr R31.
 *)
let rec compile_stmt ((s,_):Ast.stmt) : inst list = 
    (* TODO Avoid append , use revappend instead  *)
    (*************************************************************
    raise IMPLEMENT_ME
    *************************************************************)
    let rec compile_exp ((e,_):Ast.exp) : inst list =
        match (e:Ast.rexp) with 
            | Int i -> Li (rreg, (Word32.fromInt i))::[] 
            (* TODO this does not handle 32-bit nums *)
            | Var v -> La(rreg, ("var" ^ v))::Lw(rreg, R2, zero)::[]
            | Binop(e1,b,e2) -> 
                (let t = new_temp() in
                    (compile_exp e1) @ La(R3,t)::Sw(R2,R3,zero)::[]
                    (* Doing this order, we have transformed Binop(e1,b,e2) 
                       into putting e1 into R3, and e2 into R2, so we must 
                    switch the registers *) 
                    @ (compile_exp e2) @ La(R3,t)::Lw(R3,R3,zero)::[]
                    @ (match b with
                        | Plus -> Add(rreg, R3, Reg R2)::[]
                        | Minus -> Sub(rreg, R3, R2)::[]
                        | Times -> Mul(rreg, R3, R2)::[]
                        | Div -> Mips.Div(rreg, R3, R2)::[]
                        | Eq -> Mips.Seq(rreg, R3, R2)::[]
                        | Neq -> Sne(rreg, R3, R2)::[]
                        | Lt -> Slt(rreg, R3, R2)::[]
                        | Lte -> Sle(rreg, R3, R2)::[]
                        | Gt -> Sgt (rreg, R3, R2)::[]
                        | Gte -> Sge(rreg, R3, R2)::[]
                        ))
            | And(e1,e2) -> 
                (let end_l = new_label() in
                 let t = new_temp() in
                    (compile_exp e1) @ Beq(R2,R0,end_l)::La(R3,t)::Sw(R2,R3,zero)::[] 
                    @ (compile_exp e2) @ La(R3,t)::Lw(R3,R3,zero)::
                    Mips.And(rreg, R2, Reg R3)::Label(end_l)::[])
            | Or(e1,e2) ->
                (let end_l = new_label() in
                 let t = new_temp() in
                    (compile_exp e1) @ Bne(R2,R0,end_l)::La(R3,t)::Sw(R2,R3,zero)::[] 
                    @ (compile_exp e2) @ La(R3,t)::Lw(R3,R3,zero)::
                    Mips.Or(rreg, R2, Reg R3)::Label(end_l)::[])
            
            | Not e  -> 
                (compile_exp e) @ Mips.Seq(rreg, R2, R0) ::[]
            | Assign(v,e) ->
                (compile_exp e) @ La(R3, ("var" ^ v))::Sw(rreg, R3, zero)::[]
    in 
    match (s : Ast.rstmt) with
        | Return e -> (compile_exp e) @ Jr(R31)::[]
        | Exp e -> compile_exp e
        | Seq(s1,s2) ->  compile_stmt s1 @ compile_stmt s2
        | If(e,s1,s2) ->  
            (let else_l = new_label() in
             let end_l = new_label() in 
            (compile_exp e) @ Beq(R2,R0,else_l)::[] @ 
            (compile_stmt s1) @ J(end_l)::Label(else_l)::[] @ 
            (compile_stmt s2) @ Label(end_l)::[])
        | While(e,s) -> 
            (let condition_l = new_label() in
             let top_l = new_label() in
            J(condition_l)::Label(top_l)::[] @ 
            (compile_stmt s) @ 
            Label(condition_l)::[] @ 
            (compile_exp e) @ 
            Bne(R2,R0,top_l)::[])
        | For(e1,e2,e3,s) -> 
            compile_stmt(Seq((Exp e1,0),
                (While(e2,(Seq(s,(Exp e3, 0)), 0)), 0)), 0) 
    


(* compiles Fish AST down to MIPS instructions and a list of global vars *)
let compile (p : Ast.program) : result = 
    let _ = reset() in
    let _ = collect_vars(p) in
    let insts = (Label "main") :: (compile_stmt p) in
    { code = insts; data = VarSet.elements (!variables) }

(* converts the output of the compiler to a big string which can be 
 * dumped into a file, assembled, and run within the SPIM simulator
 * (hopefully). *)
let result2string ({code;data}:result) : string = 
    let strs = List.map (fun x -> (Mips.inst2string x) ^ "\n") code in
    let var2decl x = x ^ ":\t.word 0\n" in
    "\t.text\n" ^
    "\t.align\t2\n" ^
    "\t.globl main\n" ^
    (String.concat "" strs) ^
    "\n\n" ^
    "\t.data\n" ^
    "\t.align 0\n"^
    (String.concat "" (List.map var2decl data)) ^
    "\n"

