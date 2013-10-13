(* Compile Cish AST to MIPS AST *)
open Mips

exception IMPLEMENT_ME

type result = { code : Mips.inst list;
                data : Mips.label list }

(* generate fresh labels *)
let label_counter = ref 0
let new_int() = (label_counter := (!label_counter) + 1; !label_counter)
let new_label() = "L" ^ (string_of_int (new_int()))

let sp = R29
let fp = R30
let ra = R31

let rec compile (p:Ast.program) : result =
	match p with
	| hd::tl -> 
	| [] -> 
let allocate_word : inst = Add (sp, sp, Immed -4l) in

(* we need to make an environment *)
let compile_function (f:funcsig) : inst list = 
	let make_prologue (():unit) : inst list = 
		allocate_word::Sw(ra, sp, 0l)::allocate_word::Sw(fp, sp, 0l)::
		Add(fp, sp, Immed 4l)::Sw(R4, fp, Immed 4l)::Sw(R5, fp, Immed 8l)::
		Sw(R6, fp, Immed 12l)::(R7, fp, Immed 16l)::[] in
	let make_epilogue (():unit) : inst list = 
		Lw(ra, fp, Immed 0l)::Lw(fp, fp, Immed 4l)::
		Add(sp, sp, Immed value)::Jr(ra)[] in
	let compile_stmt ((s, _):Ast.stmt) : inst list = 
		
	let make_body (f:funsig) : inst list = 
		compile_stmt f.body in
	make_prologue () @ make_body f @ make_epilogue ()


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
