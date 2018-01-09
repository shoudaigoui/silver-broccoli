open Filename

type instruction =
       |R of int * int * int * int * int * int
       |I of int * int * int * int
       |J of int * int

let l = ["add";"addi";"addiu";"addu";"and";"andi";"beq";"bne";"j";"jal";"jr";"lbu";"lhu";"ll";"lui";"lw";"nor";"or";"ori";"slt";"slti";
		"sltiu";"sltu";"sll";"srl";"sb";"sc";"sh";"sw";"sub";"subu";"xor";"xori";"div";"divu";"mult";"multu";"mfhi";"mflo";"sra"]

let cts i = String.make 1 (Char.chr i) (* juste un raccourci *)
let cts' c = String.make 1 c (* un raccourci d'écriture *)

let read_from_file p =
	let in_channel = open_in p in
	let a = ref [] in
	begin try
	while true do
		let line = input_line in_channel in
		a := line::(!a)
	done
	with End_of_file -> close_in in_channel
	  end;
	  List.rev !a

let break_down_instr i =
	let rec aux s acc curr = let n = String.length s in
		if n = 0 then List.rev (curr::acc) else
		match s.[0] with
			|' '|',' -> begin if curr <> "" then aux (String.sub s 1 (n-1)) (curr::acc) "" else aux (String.sub s 1 (n-1)) acc curr end
			|_ -> aux (String.sub s 1 (n-1)) acc (curr^(cts' (s.[0])))
	in aux i [] ""
		
let rti r = match r with (* register to integer *)
	|"$zero" -> 0|"$at" -> 1 |"$v0" -> 2 |"$v1" -> 3
	|"$a0" -> 4  |"$a1" -> 5 |"$a2" -> 6 |"$a3" -> 7
	|"$t0" -> 8  |"$t1" -> 9 |"$t2" -> 10|"$t3" -> 11
	|"$t4" -> 12 |"$t5" -> 13|"$t6" -> 14|"$t7" -> 15
	|"$s0" -> 16 |"$s1" -> 17|"$s2" -> 18|"$s3" -> 19
	|"$s4" -> 20 |"$s5" -> 21|"$s6" -> 22|"$s7" -> 23
	|"$t8" -> 24 |"$t9" -> 25|"$k0" -> 26|"$k1" -> 27
	|"$gp" -> 28 |"$sp" -> 29|"$fp" -> 30|"$s3" -> 31
	|_ -> failwith ("Invalid register name : "^r)

let broken_down_instr_to_machine_instr i =
  match (List.length i) with
  |2 -> begin
    match i with
    |["j";addr]       -> J(2,int_of_string addr)
    |["jal";addr]     -> J(3,int_of_string addr)
    |["jr";rs]        -> R(0,rti rs,0,0,0,8)
    |["mfhi";rd]      -> R(0,0,0,rti rd,0,16)
    |["mflo";rd]      -> R(0,0,0,rti rd,0,18)
    |_                -> let x = List.hd i in if List.mem x l 
			 then failwith ("Invalid arity for operation : "^x)
			 else failwith ("Unknown operation : "^x)
  end
  |3 -> begin
    match i with
    |["lui";rt;i]     -> I(15,0,rti rt,int_of_string i)
    |["div";rs;rt]    -> R(0,rti rs,rti rt,0,0,26)
    |["divu";rs;rt]   -> R(0,rti rs,rti rt,0,0,27)
    |["mult";rs;rt]   -> R(0,rti rs,rti rt,0,0,24)
    |["multu";rs;rt]  -> R(0,rti rs,rti rt,0,0,25)
    |_                -> let x = List.hd i in if List.mem x l 
			 then failwith ("Invalid arity for operation : "^x)
			 else failwith ("Unknown operation : "^x)
  end
  |4 -> begin
    match i with
    |["add";rs;rt;rd] -> R(0,rti rs,rti rt,rti rd,0,32)
    |["addi";rs;rt;i] -> I(8,rti rs,rti rt,int_of_string i)   
    |["addiu";rs;rt;i]-> I(9,rti rs,rti rt,int_of_string i)   
    |["addu";rs;rt;rd]-> R(0,rti rs,rti rt,rti rd,0,33)
    |["and";rs;rt;rd] -> R(0,rti rs,rti rt,rti rd,0,36)
    |["andi";rs;rt;i] -> I(12,rti rs,rti rt,int_of_string i)
    |["beq";rs;rt;i]  -> I(4,rti rs,rti rt,int_of_string i)
    |["bne";rs;rt;i]  -> I(5,rti rs,rti rt,int_of_string i)
    |["lbu";rs;rt;i]  -> I(36,rti rs,rti rt,int_of_string i)
    |["lhu";rs;rt;i]  -> I(37,rti rs,rti rt,int_of_string i)
    |["ll";rs;rt;i]   -> I(48,rti rs,rti rt,int_of_string i)
    |["lw";rs;rt;i]   -> I(35,rti rs,rti rt,int_of_string i)
    |["nor";rs;rt;rd] -> R(0,rti rs,rti rt,rti rd,0,39)
    |["or";rs;rt;rd]  -> R(0,rti rs,rti rt,rti rd,0,37)
    |["ori";rs;rt;i]  -> I(13,rti rs,rti rt,int_of_string i)
    |["slt";rs;rt;rd] -> R(0,rti rs,rti rt,rti rd,0,42)
    |["slti";rs;rt;i] -> I(10,rti rs,rti rt,int_of_string i)
    |["sltiu";rs;rt;i]-> I(11,rti rs,rti rt,int_of_string i)
    |["sltu";rs;rt;rd]-> R(0,rti rs,rti rt,rti rd,0,43)
    |["sll";rt;rd;smt]-> R(0,0,rti rt,rti rd,int_of_string smt,0)
    |["srl";rt;rd;smt]-> R(0,0,rti rt,rti rd,int_of_string smt,2)
    |["sb";rs;rt;i]   -> I(40,rti rs,rti rt,int_of_string i)
    |["sc";rs;rt;i]   -> I(56,rti rs,rti rt,int_of_string i)
    |["sh";rs;rt;i]   -> I(41,rti rs,rti rt,int_of_string i)
    |["sw";rs;rt;i]   -> I(43,rti rs,rti rt,int_of_string i)
    |["sub";rs;rt;rd] -> R(0,rti rs,rti rt,rti rd,0,34)
    |["subu";rs;rt;rd]-> R(0,rti rs,rti rt,rti rd,0,35)
    |["xor";rs;rt;rd] -> R(0,rti rs,rti rt,rti rd,0,38)
    |["xori";rs;rt;i] -> I(14,rti rs,rti rt,int_of_string i)
    |["sra";rt;rd;smt]-> R(0,0,rti rt,rti rd,int_of_string smt,3)
    |_                -> let x = List.hd i in if List.mem x l 
			 then failwith ("Invalid arity for operation : "^x)
			 else failwith ("Unknown operation : "^x)
  end
  |_ -> let x = List.hd i in if List.mem x l 
        then failwith ("Invalid arity for operation : "^x)
        else failwith ("Unknown operation : "^x)
	
let instruction_to_binary = function
	|R(opcode,rs,rt,rd,shamt,funct) -> 
	funct + ( shamt + ( rd + ( rt + ( rs + opcode lsl 5 ) lsl 5 ) lsl 5 ) lsl 5 ) lsl 6
	|I(opcode,rs,rt,immt) -> 
	immt + ( rt + ( rs + opcode lsl 5 ) lsl 5 ) lsl 16
	|J(opcode,addr) -> 
	addr + opcode lsl 26

let word_to_chars w = let w0,a0 = w /256,w  mod 256 in
					  let w1,a1 = w0/256,w0 mod 256 in
					  let a3,a2 = w1/256,w1 mod 256 in
					  (cts a3) ^ (cts a2) ^ (cts a1) ^ (cts a0)
let rec convert_stream l = match l with
	|x::q -> (word_to_chars (instruction_to_binary (broken_down_instr_to_machine_instr (break_down_instr x))))^(convert_stream q)
	|[]   -> ""

let main path =
	(* for path *)
        let f = ref "" in
       	if check_suffix path ".mips" then f := ((chop_suffix path ".mips")^".rom")
       	else failwith ("Invalid extension");
	let l = read_from_file path in
	let x = convert_stream l in
	let o = open_out !f in
	output_string o x;
	close_out o
	
let file =
  let file = ref None in
  let set_file s =
    if not (check_suffix s ".mips") then
      raise (Arg.Bad "No .mips extension");
    file := Some s
  in
  Arg.parse [] set_file "";
  match !file with Some f -> main f | None -> exit 1

