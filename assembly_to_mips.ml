type instruction =
	R of int * int * int * int * int * int
	I of int * int * int * int
	J of int * int

let rti r = match r with (* register to integer *)
	|"$zero" -> 0|"$at" -> 1 |"$v0" -> 2 |"$v1" -> 3
	|"$a0" -> 4  |"$a1" -> 5 |"$a2" -> 6 |"$a3" -> 7
	|"$t0" -> 8  |"$t1" -> 9 |"$t2" -> 10|"$t3" -> 11
	|"$t4" -> 12 |"$t5" -> 13|"$t6" -> 14|"$t7" -> 15
	|"$s0" -> 16 |"$s1" -> 17|"$s2" -> 18|"$s3" -> 19
	|"$s4" -> 20 |"$s5" -> 21|"$s6" -> 22|"$s7" -> 23
	|"$t8" -> 24 |"$t9" -> 25|"$k0" -> 26|"$k1" -> 27
	|"$gp" -> 28 |"$sp" -> 29|"$fp" -> 30|"$s3" -> 31

let broken_down_instr_to_machine_instr i = match i with
	|["add",rs,rt,rd] -> R(0,rti rs,rti rt,rti rd,0,32)
	|["addi",rs,rt,i] -> I(8,rti rs,rti rt,i)
	|["addiu",rs,rt,i]-> I(9,rti rs,rti rt,i)
	|["addu",rs,rt,rd]-> R(0,rti rs,rti rt,rti rd,0,33)
	|["and",rs,rt,rd] -> R(0,rti rs,rti rt,rti rd,0,36)
	|["andi",rs,rt,i] -> I(12,rti rs,rti rt,i)
	|["beq",rs,rt,i]  -> I(4,rti rs,rti rt,i)
	|["bne",rs,rt,i]  -> I(5,rti rs,rti rt,i)
	|{"j",addr]       -> J(2,addr)
	|["jal",addr]     -> J(3,addr)
	|["jr",rs]        -> R(0,rti rs,0,0,0,8)
	|["lbu",rs,rt,i]  -> I(36,rti rs,rti rt,i)
	|["lhu",rs,rt,i]  -> I(37,rti rs,rti rt,i)
	|["ll",rs,rt,i]   -> I(48,rti rs,rti rt,i)
	|["lui",rt,i]     -> I(15,0,rti rt,i)
	|["lw",rs,rt,i]   -> I(35,rti rs,rti rt,i)
	|["nor",rs,rt,rd] -> R(0,rti rs,rti rt,rti rd,0,39)
	|["or",rs,rt,rd]  -> R(0,rti rs,rti rt,rti rd,0,37)
	|["ori",rs,rt,i]  -> I(13,rti rs,rti rt,i)
	|["slt",rs,rt,rd] -> R(0,rti rs,rti rt,rti rd,0,42)
	|["slti",rs,rt,i] -> I(10,rti rs,rti rt,i)
	|["sltiu",rs,rt,i]-> I(11,rti rs,rti rt,i)
	|["sltu",rs,rt,rd]-> R(0,rti rs,rti rt,rti rd,0,43)
	|["sll",rt,rd,smt]-> R(0,0,rti rt,rti rd,smt,0)
	|["srl",rt,rd,smt]-> R(0,0,rti rt,rti rd,smt,2)
	|["sb",rs,rt,i]   -> I(40,rti rs,rti rt,i)
	|["sc",rs,rt,i]   -> I(56,rti rs,rti rt,i)
	|["sh",rs,rt,i]   -> I(41,rti rs,rti rt,i)
	|["sw",rs,rt,i]   -> I(43,rti rs,rti rt,i)
	|["sub",rs,rt,rd] -> R(0,rti rs,rti rt,rti rd,0,34)
	|["subu",rs,rt,rd]-> R(0,rti rs,rti rt,rti rd,0,35)
	|["xor",rs,rt,rd] -> R(0,rti rs,rti rt,rti rd,0,38)
	|["xori",rs,rt,i] -> I(14,rti rs,rti rt,i)
	|["div",rs,rt]    -> R(0,rti rs,rti rt,0,0,26)
	|["divu",rs,rt]   -> R(0,rti rs,rti rt,0,0,27)
	|["mult",rs,rt]   -> R(0,rti rs,rti rt,0,0,24)
	|["multu",rs,rt]  -> R(0,rti rs,rti rt,0,0,25)
	|["mfhi",rd]      -> R(0,0,0,rti rd,0,16)
	|["mflo",rd]      -> R(0,0,0,rti rd,0,18)
	|["sra",rt,rd,smt]-> R(0,0,rti rt,rti rd,smt,3)