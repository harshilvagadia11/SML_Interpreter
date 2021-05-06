structure AST =
struct

exception environment_error of string

type id = string

datatype binop = Add | Sub | Mul | Div | Equals | LessThan | GreaterThan | And | Or | Xor | Implies
and unop = Negate | Not

datatype basictype = Int | Bool
and compoundtype = SingleType of basictype | CompoundType of compoundtype * compoundtype

datatype decl = ValDecl of id * exp | FunDecl of id * id * compoundtype * compoundtype * exp
and program = Leaf of exp | Node of exp * program

and exp = NumExp of int
		| BoolExp of bool
    	| VarExp of id
		| BinExp of binop * exp * exp
		| UnExp of unop * exp
		| LetExp of decl * exp
		| IfExp of exp * exp * exp
		| FnExp of id * compoundtype * compoundtype * exp
		| FunExp of id * id * compoundtype * compoundtype * exp
		| AppExp of exp * exp
				
and value = IntVal of int
	       	   | BoolVal of bool
			   | FunVal of id * exp * ((id * value) list)

type typeEnvironment = (id * compoundtype) list

fun typeEnvAdd (var: id, t: compoundtype, env: typeEnvironment) = (var, t) :: env

fun typeEnvLookup (var: id, env: typeEnvironment, statement: int) =
    case List.find(fn (x, _) => x = var) env of
				       SOME (x, v)   => v
				    |   NONE => raise environment_error ("Environment lookup error " ^ var ^ " in statement " ^ Int.toString(statement))

type environment = (id * value) list

fun envAdd (var: id, v: value, env: environment) = (var, v) :: env

fun envLookup (var: id, env:environment) =
    case List.find(fn (x, _) => x = var) env of
				       SOME (x, v)   => v
				    |   NONE => raise environment_error ("Environment lookup error " ^ var)
and
envConcat(env: environment, env1: environment) = 
	env @ env1 (* TODO: Add only if not present *)
and printEnv(env: environment) = 
	case env of
		[] => TextIO.output(TextIO.stdOut, "\n")
	|	(x, v) :: env1 => (TextIO.output(TextIO.stdOut, x ^ " " ^ valToString(v) ^ " "); printEnv(env1))
and
valToString(v: value): string = 
    case v of
        IntVal i => Int.toString(i)
    |   BoolVal b => Bool.toString(b)
    |   FunVal (x, v, env) => "Function"
and
typetoString(t: compoundtype): string = 
	case t of
		SingleType t => if t = Int then "int" else "bool"
	|	CompoundType (t1, t2) => "(" ^ typetoString(t1) ^ "->" ^ typetoString(t2) ^ ")"
and
binopToString(b: binop): string = 
	case b of
		Add => "Add"
	|	Sub => "Sub"
	|	Mul => "Mul"
	|	Div => "Div"
	|	Equals => "Equals"
	|	LessThan => "LessThan"
	|	GreaterThan => "GreaterThan"
	|	And => "And"
	|	Or => "Or"
	|	Xor => "Xor"
	|	Implies => "Implies"
and
unopToString(u: unop): string = 
	case u of
		Negate => "Negate"
	|	Not => "Not"
and
programToString(p: program): string = 
	case p of
		Leaf(e) => "Leaf (" ^ expToString(e) ^ ")"
	|	Node(e, p1) => "Node (" ^ expToString(e) ^ ", " ^ programToString(p1) ^ ")"
and
expToString(e: exp): string = 
	case e of
		NumExp i => "NumExp (" ^ Int.toString(i) ^ ")"
	|	BoolExp b => "BoolExp (" ^ Bool.toString(b) ^ ")"
	|	VarExp x => "VarExp (" ^ x ^ ")"
	|	BinExp (b, e1 ,e2) => "BinExp (" ^ binopToString(b) ^ ", " ^ expToString(e1) ^ ", " ^ expToString(e2) ^ ")"
	|	UnExp (u, e1) => "UnExp (" ^ unopToString(u) ^ ", " ^ expToString(e1) ^ ")"
	|	LetExp (d, e1) => "LetExp (" ^ declToString(d) ^ ", " ^ expToString(e1) ^ ")"
	|	IfExp (e1, e2 ,e3) => "IfExp (" ^ expToString(e1) ^ ", " ^ expToString(e2) ^ ", " ^ expToString(e3) ^ ")"
	|	FnExp (x, t1, t2, e) => "FnExp (" ^ x ^ ", " ^ typetoString(t1) ^ ", " ^ typetoString(t2) ^ ", " ^ expToString(e) ^ ")"
	|	FunExp (x1, x2, t1, t2, e) => "FunExp (" ^ x1 ^ ", " ^ x2 ^ ", " ^ typetoString(t1) ^ ", " ^ typetoString(t2) ^ ", " ^ expToString(e) ^ ")"
	|	AppExp(e1, e2) => "AppExp (" ^ expToString(e1) ^ ", " ^ expToString(e2) ^ ")"
and
declToString(d: decl): string = 
	case d of
		ValDecl (x, e) => "ValDecl (" ^ x ^ ", " ^ expToString(e) ^ ")"
	|	FunDecl (x1, x2, t1, t2, e) => "FunDecl (" ^ x1 ^ ", " ^ x2 ^ ", " ^ typetoString(t1) ^ ", " ^ typetoString(t2) ^ ", " ^ expToString(e) ^ ")"
end


