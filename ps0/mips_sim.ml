open Mips_ast
open Byte

exception TODO
exception FatalError

(* Register file definitions. A register file is a map from a register 
   number to a 32-bit quantity. *)
module IntMap = Map.Make(struct type t = int let compare = compare end)
type regfile = int32 IntMap.t 
let empty_rf = IntMap.empty
let rf_update (r : int) (v : int32) (rf : regfile) : regfile = 
  IntMap.add r v rf
let rf_lookup (r : int) (rf : regfile) : int32 = 
  try IntMap.find r rf with Not_found -> Int32.zero
let string_of_rf (rf : regfile) : string = 
  IntMap.fold (fun key v s -> 
    s^(string_of_int key)^" -> "^(Int32.to_string v)^"\n") rf ""

(* Memory definitions. A memory is a map from 32-bit addresses to bytes. *)
module Int32Map = Map.Make(struct type t = int32 let compare = Int32.compare end)
type memory = byte Int32Map.t
let empty_mem = Int32Map.empty
let mem_update (a : int32) (v : byte) (m : memory) : memory =
  Int32Map.add a v m
let mem_lookup (a : int32) (m : memory) : byte =
  try (Int32Map.find a m) with Not_found -> mk_byte Int32.zero
let string_of_mem (m : memory) : string =
  Int32Map.fold (fun key v s ->
    s^(Int32.to_string key)^" -> "^(Int32.to_string (b2i32 v))^"\n") m ""

(* State *)
type state = { r : regfile; pc : int32; m : memory }

let inst2str i =
  match i with
    Add (r1,r2,r3) -> "add "^(reg2str r1)^", "^(reg2str r2)^", "^(reg2str r3)^"\n"
  | Beq (r1,r2,imm) -> "beq "^(reg2str r1)^", "^(reg2str r2)^", "^(Int32.to_string imm)^"\n"
  | Jr (r) -> "jr "^(reg2str r)^"\n"
  | Jal (imm) -> "jal "^(Int32.to_string imm)^"\n"
  | Li (r,imm) -> "li "^(reg2str r)^", "^(Int32.to_string imm)^"\n"
  | Lui (r,imm) -> "lui "^(reg2str r)^", "^(Int32.to_string imm)^"\n"
  | Ori (r1,r2,imm) -> "ori "^(reg2str r1)^", "^(reg2str r2)^", "^(Int32.to_string imm)^"\n"
  | Lw (r1,r2,imm) -> "lw "^(reg2str r1)^", "^(Int32.to_string imm)^"("^(reg2str r2)^")\n"
  | Sw (r1,r2,imm) -> "sw "^(reg2str r1)^", "^(Int32.to_string imm)^"("^(reg2str r2)^")\n"

(* This shifts the current bits to the left by length and adds value to it *)
let push (onto : int32) (length : int) (value : int32) : int32 = 
  Int32.logor (Int32.shift_left onto length) value 

let _ = assert ( push 1l 5 1l = 33l ) 

let pop (value: int32) (length : int) : int32*int32 = 
  (* TODO Explain in comment *)
  let e = Int32.logand (Int32.pred (Int32.shift_left Int32.one length)) (value) in
  (e, Int32.shift_right_logical value length)

let _ = assert ( pop 33l 5 = (1l,1l) )

(* Helper function for registers specifically *)
let push_reg (onto : int32) (r : reg) : int32 = 
  push onto 5 (Int32.of_int (reg2ind r))

let pop_reg (from : int32) : reg * int32 = 
  let r,v = pop from 5 in
  ((ind2reg (Int32.to_int r)),v) 

(* Converts an instruction into the corresponding encoding *)
let rec inst2word (instr : inst) : int32 list = 
  match instr with
  | Add (rd, rs, rt) -> 
    let val1 = push_reg 0x0l rs in
    let val2 = push_reg val1 rt in
    let val3 = push_reg val2 rd in
    let val4 = push val3 5 0x0l in
    (push val4 6 0x20l)::[]
  | Beq (rs, rt, offset) ->
    let val1 = push_reg 0x4l rs in
    let val2 = push_reg val1 rt in
    (push val2 16 offset)::[]
  | Jr (rs) ->
    let val1 = push_reg 0x0l rs in
    let val2 = push val1 15 0x0l in
    (push val2 6 0x8l)::[]
  | Jal (target) ->
    push 0x3l 26 target::[]
  | Li (rdest, imm) ->
    (inst2word (Lui (rdest, Int32.shift_right imm 16)))@
    (inst2word (Ori (rdest, rdest, (Int32.logand 0xFFFFl imm))))
  | Lui (rt, imm) -> 
    let val1 = push 0xfl 5 0x0l in
    let val2 = push_reg val1 rt in
    (push val2 16 imm)::[]
  | Ori (rt, rs, imm) ->
    let val1 = push_reg 0xdl rs in
    let val2 = push_reg val1 rt in
    (push val2 16 imm)::[]
  | Lw (rt, rs, offset) ->
    let val1 = push_reg 0x23l rs in
    let val2 = push_reg val1 rt in
    (push val2 16 offset)::[]
  | Sw (rt, rs, offset) -> 
    let val1 = push_reg 0x2bl rs in
    let val2 = push_reg val1 rt in
    push val2 16 offset::[]

let word2inst (word: int32) : inst = 
  let _,code = pop word (32-6) in
  match code with 
  | 0x0l -> 
    let last,word = pop word 6 in
    if last = 0x20l then
      let _,word = pop word 5 in
      let rd,word = pop_reg word in
      let rt,word = pop_reg word in
      let rs,_ = pop_reg word in
        Add(rd,rs,rt)
    else
      let _,word = pop word 15 in
      let rs,_ = pop_reg word in
        Jr (rs)
  | 0x3l -> 
    let target,_ = pop word 26 in
      Jal (target)
  | 0x4l -> 
    let offset,word = pop word 16 in
    let rt,word = pop_reg word in
    let rs,_ = pop_reg word in
      Beq (rs, rt, offset)
  | 0xdl -> 
    let imm,word = pop word 16 in
    let rt,word = pop_reg word in
    let rs,_ = pop_reg word in
      Ori (rt, rs, imm)
  | 0xfl ->
    let imm,word = pop word 16 in
    let rt,_ = pop_reg word in
      Lui (rt,imm)
  | 0x23l ->
    let offset,word = pop word 16 in 
    let rt,word = pop_reg word in 
    let rs,_ = pop_reg word in 
      Lw (rt, rs, offset)
  | 0x2bl ->
    let offset,word = pop word 16 in 
    let rt,word = pop_reg word in 
    let rs,_ = pop_reg word in 
      Sw (rt, rs, offset)
  | _ -> raise (Failure "Instruction not found/implemented")

(* TODO test*)
let _ = 
  let insts = [
  Add(R0,R1,R2);
  Beq(R3,R4,0x5678l);
  Jr(R5);
  Jal(0xABCDEFl);
  Lui(R6,0xFEDCl);
  Ori(R7,R8,0x1020l);
  Lw(R18,R19,0x4321l);
  Sw(R23,R26,0x8765l)] in
  List.iter 
  (fun i -> 
    let word = List.hd (inst2word i) in
    assert ( word2inst word = i ) )
  insts

(* TODO test *) (* Adds an encoding to memory *)
let mem_update_word (a : int32) (word : int32) (m : memory) : memory =
  let byte0 = mk_byte word in
  let byte1 = mk_byte (Int32.shift_right_logical word 8) in
  let byte2 = mk_byte (Int32.shift_right_logical word 16) in
  let byte3 = mk_byte (Int32.shift_right_logical word 24) in
  let m = mem_update (Int32.add a 0l) byte0 m in
  let m = mem_update (Int32.add a 1l) byte1 m in
  let m = mem_update (Int32.add a 2l) byte2 m in
  mem_update (Int32.add a 3l) byte3 m

let rec mem_update_list (pos : int32) (instrs : int32 list) (m : memory) : memory = 
  match instrs with
  | hd::tl -> mem_update_list (Int32.add pos 4l) tl (mem_update_word pos hd m)
  | [] -> m 

(* Map a program, a list of Mips assembly instructions, down to a starting 
   state. You can start the PC at any address you wish. Just make sure that 
   you put the generated machine code where you started the PC in memory! *)
let assem (prog : program) : state = 
  let load (prog : program) (mem : memory) (pos : int32) : memory= 
    let rec get_instrs (prog : program) : int32 list = 
      match prog with
      | instr::prog ->
        (inst2word instr) @ (get_instrs prog)
      | [] -> []
    in
    mem_update_list pos (get_instrs prog) mem
  in
  let pos : int32 = 0x100l in 
  {r = empty_rf; pc = pos; m = load prog empty_mem pos}

(* TODO test *)
let mem_lookup_word (a : int32) (m : memory) : int32 =
  let byte0 = mem_lookup (Int32.add a 0l) m in
  let byte1 = mem_lookup (Int32.add a 1l) m in
  let byte2 = mem_lookup (Int32.add a 2l) m in
  let byte3 = mem_lookup (Int32.add a 3l) m in
  let word = push (b2i32 (byte3)) 8 (b2i32 (byte2)) in
  let word = push word 8 (b2i32 (byte1)) in
  push word 8 (b2i32 (byte0))

let run_inst (i : inst) (s: state) : state = (* TODO *)
  match i with
    | Add (rd, rs, rt) -> 
      let sum = Int32.add (rf_lookup (reg2ind rs) s.r) 
        (rf_lookup (reg2ind rt) s.r) 
      in
      {r = rf_update (reg2ind rd) sum s.r; pc = (Int32.add s.pc 4l); m = s.m}
    | Beq (rs, rt, offset) ->
      if ((Int32.compare (rf_lookup (reg2ind rs) s.r) 
        (rf_lookup (reg2ind rt) s.r)) = 0)
      then {r = s.r; pc = (Int32.add (Int32.add s.pc 4l) 
        (Int32.mul 4l offset)); m = s.m}
      else {r = s.r; pc = (Int32.add s.pc 4l); m = s.m}
    | Jr (rs) -> 
      {r = s.r; pc = Int32.mul 4l (rf_lookup (reg2ind rs) s.r); m = s.m}
    | Jal (target) ->
      {r = rf_update 31 (Int32.add s.pc 4l) s.r; 
        pc = Int32.mul 4l (Int32.shift_left target 2); m = s.m}
    | Li (rdest, imm) -> raise(Failure "not possible")
    | Lui (rt, imm) -> 
      let zeroed = Int32.logand 0xFFFFl (rf_lookup (reg2ind rt) s.r) in
      let shifted_imm = Int32.shift_left imm 16 in
      {r = rf_update (reg2ind rt) (Int32.logor shifted_imm zeroed) s.r;
      pc = (Int32.add s.pc 4l); m = s.m}
    | Ori (rt, rs, imm) ->
      {r = rf_update (reg2ind rt) 
      (Int32.logor (rf_lookup (reg2ind rs) s.r) imm) s.r; 
      pc = Int32.add s.pc 4l; m = s.m}
    | Lw (rt, address, offset) -> 
      let word = mem_lookup_word 
        (Int32.add (Int32.of_int (reg2ind address)) offset) s.m in
      {r = rf_update (reg2ind rt) (Int32.add word offset) s.r;
      pc = Int32.add s.pc 4l; m = s.m}
    | Sw (rt, address, offset) -> 
      let word = rf_lookup (reg2ind rt) s.r in
      let mem_location = Int32.add (Int32.of_int (reg2ind address)) offset in
      {r = s.r; pc = Int32.add s.pc 4l; 
      m = mem_update_word mem_location word s.m}


(* Testing memory word functions *)
let _ =
  let addr = 0x01234567l in
  let word = 0x89ABCDEFl in
  let m = mem_update_word addr word empty_mem in
  assert ( mem_lookup_word addr m = word )

(* Given a starting state, simulate the Mips machine code to get a final state *)
let rec interp (init_state : state) : state = 
  if init_state.pc = 0x0l then
    init_state
  else
    let word : int32 = mem_lookup_word init_state.pc init_state.m in
    let instr : inst = word2inst word in
    let new_state = run_inst instr init_state in
    interp new_state
 
