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

(* This shifts the current bits to the left by length and adds value to it *)
let push (onto : int32) (length : int) (value : int32) : int32 = 
  Int32.logor (Int32.shift_left onto length) value

assert ( push 1l 5 1l = 33l ) ;

(* Helper function for registers specifically *)
let push_reg (onto : int32) (r : reg) = 
  push onto 5 Int32.of_int (reg2ind r)

(* Converts an instruction into the corresponding encoding *)
let rec inst2word (instr : inst) : int32 = 
  match instr with
  | Add (rd, rs, rt) -> 
    let val1 = push_reg 0x0l rs;
    let val2 = push_reg val1 rt;
    let val3 = push_reg val2 rd;
    let val4 = push val3 5 0x0l;
    push val4 6 0x20l
  | Beq (rs, rt, offset) ->
    let val1 = push_reg 0x4l rs;
    let val2 = push_reg val1 rt;
    push val2 16 offset
  | Jr (rs) ->
    let val1 = push_reg 0x0l rs;
    let val2 = push val1 15 0x0l;
    push val2 6 0x8l
  | Jal (target) ->
    push 0x3l 26 target
  (* same as Lui (rd, hi16) + Ori (rd, rd, lo16) *)
  (* ask how to include 2 instructions in one word? *)
  | Li (rdest, imm) ->
    inst2word (Lui (rdest, Int32.shift_right imm 16)) (* you need to do an ori after this! *)
  | Lui (rt, imm) -> 
    let val1 = push 0xf 5 0x0l;
    let val2 = push_reg val1 rt;
    push val2 16 (Int32.shift_right imm)
  | Ori (rt, rs, imm) ->
    let val1 = push_reg 0xd rs;
    let val2 = push_reg val1 rt;
    push val2 16 (Int32.logand 0xFFFF imm)
  | Lw (rt, rs, offset) ->
    let val1 = push_reg 0x23 rs;
    let val2 = push_reg val1 rt;
    push val2 16 offset
  | Sw (rt, rs, offset) -> 
    let val1 = push_reg 0x2b rs;
    let val2 = push_reg val1 rt;
    push val2 = 16 offset

(* TODO test *) (* Adds an encoding to memory *)
let mem_update_word (a : int32) (word : int32) (m : memory) : memory =
  let byte0 = mk_byte word;
  let byte1 = mk_byte (Int32.shift_right_logical word 8);
  let byte2 = mk_byte (Int32.shift_right_logical word 16);
  let byte3 = mk_byte (Int32.shift_right_logical word 24);
  mem_update (Int32.add a 0l) byte0 m;
  mem_update (Int32.add a 1l) byte1 m;
  mem_update (Int32.add a 2l) byte2 m;
  mem_update (Int32.add a 3l) byte3 m

(* Map a program, a list of Mips assembly instructions, down to a starting 
   state. You can start the PC at any address you wish. Just make sure that 
   you put the generated machine code where you started the PC in memory! *)
let rec assem (prog : program) : state = 
  let rec load (prog : program) (mem : memory) (pos : int32) : memory= 
    match prog with
    | instr::prog ->
      load prog (mem_update_word pos inst2word(instr) mem) (Int32.add pos 4l)
    | [] -> mem
  in
  let pos : int32 = 0x100l in 
  {r = empty_rf; pc = pos; m = load prog empty_mem pos}

(* TODO test *)
let mem_lookup_word (a : int32) (m : memory) : int32 =
  let byte0 = mem_lookup (Int32.add a 0l) m;
  let byte1 = mem_lookup (Int32.add a 1l) m;
  let byte2 = mem_lookup (Int32.add a 2l) m;
  let byte3 = mem_lookup (Int32.add a 3l) m;

  let val = push (b2i32 (byte3)) 8 (b2i32 (byte2)) in
  let val = push val 8 (b2i32 (byte1)) in
  push val 8 (b2i32 (byte0))

let word2inst (word: int32) : inst = raise TODO

let run_inst (i : inst) (s: state) : state = (* TODO *)
  match i with
    | Add (rd, rs, rt) -> 
      let sum = Int32.add (rf_lookup rs s.r) (rf_lookup rt s.r) in
        rf_update sum s.r
    | Beq (rs, rt, offset) ->
    | Jr (rs) ->
    | Jal (target) ->
    | Li (rdest, imm) ->     
    | Lui (rt, imm) -> 
    | Ori (rt, rs, imm) ->
    | Lw (rt, address, offset) -> 
    | Sw (rt, address, offset) ->;

(* Given a starting state, simulate the Mips machine code to get a final state *)
let rec interp (init_state : state) : state = 
  if init_state.pc == 0x0l then
    init_state
  else
    let word : int32 = mem_lookup_word pc in
    let instr : inst = word2inst word in
    let new_state = run_inst instr init_state;
    interp new_state

