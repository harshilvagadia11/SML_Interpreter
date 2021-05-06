structure TYPECHECKER  =
struct
open AST

exception type_error of string;
val statement = ref 0;

fun xor(a: bool, b: bool) = (a orelse b) andalso not(a andalso b);

fun typeProgram(p: program, env: typeEnvironment): compoundtype list = (
    statement := !statement + 1;
    case p of
        Leaf e => [typeExp(e, env)]
    |   Node (e, p1) => case e of
            FunExp (x1, x2, t1, t2, e) => if typeExp(e, typeEnvAdd(x2, t1, typeEnvAdd(x1, CompoundType(t1, t2), env))) = t2 then CompoundType(t1, t2) :: typeProgram(p1, typeEnvAdd(x1, CompoundType(t1, t2), env)) else raise type_error("Function " ^ x1 ^ " does not evaluate to type " ^ typetoString(t2) ^ " in statement " ^ Int.toString(!statement))
        |   _ => typeExp(e, env) :: typeProgram(p1, env)
)
and
typeExp(e: exp, env: typeEnvironment): compoundtype =
    case e of
	    NumExp i => SingleType Int
      | BoolExp b => SingleType Bool
      | VarExp x => typeEnvLookup(x, env, !statement) 				  
      | BinExp (b, e1, e2)  => typeBinExp(b, e1, e2, env)
      | UnExp (u, e) => typeUnExp(u, e, env)
      | LetExp(ValDecl(x, e1), e2)  =>
        let
            val v1 = typeExp (e1, env)
        in
            typeExp(e2, typeEnvAdd (x, v1, env))
        end
      | LetExp(FunDecl(x, y, t1, t2, e1), e2) => if typeExp(e1, typeEnvAdd(y, t1, typeEnvAdd(x, CompoundType(t1, t2), env))) = t2 then typeExp(e2, typeEnvAdd(x, CompoundType(t1, t2), env)) else raise type_error("Function " ^ x ^ " does not evaluate to type " ^ typetoString(t2) ^ " in statement " ^ Int.toString(!statement))
      | IfExp (e1, e2, e3) => typeIfExp(e1, e2, e3, env)
      | FnExp (x, t1, t2, e) => if typeExp(e, typeEnvAdd(x, t1, env)) = t2 then CompoundType(t1, t2) else raise type_error("Function expression with argument " ^ x ^ " does not evaluate to type " ^ typetoString(t2) ^ " in statement " ^ Int.toString(!statement))
      | FunExp (x1, x2, t1, t2, e) => if typeExp(e, typeEnvAdd(x2, t1, typeEnvAdd(x1, CompoundType(t1, t2), env))) = t2 then CompoundType(t1, t2) else raise type_error("Function " ^ x1 ^ " does not evaluate to type " ^ typetoString(t2) ^ " in statement " ^ Int.toString(!statement))
      | AppExp (e1, e2) => typeAppExp(e1, e2, env) 
and
typeBinExp(b: binop, e1: exp, e2: exp, env: typeEnvironment): compoundtype =
    case (b, typeExp(e1, env), typeExp(e2, env))  of
        (Add, SingleType Int, SingleType Int) => SingleType Int
    |   (Sub, SingleType Int, SingleType Int) => SingleType Int
    |   (Mul, SingleType Int, SingleType Int) => SingleType Int
    |   (Div, SingleType Int, SingleType Int) => SingleType Int
    |   (Equals, SingleType Int, SingleType Int)  => SingleType Bool
    |   (Equals, SingleType Bool, SingleType Bool)  => SingleType Bool
    |   (LessThan, SingleType Int, SingleType Int)  => SingleType Bool
    |   (GreaterThan, SingleType Int, SingleType Int)  => SingleType Bool
    |   (And, SingleType Bool, SingleType Bool)  => SingleType Bool
    |   (Or, SingleType Bool, SingleType Bool)  => SingleType Bool
    |   (Xor, SingleType Bool, SingleType Bool)  => SingleType Bool
    |   (Implies, SingleType Bool, SingleType Bool)  => SingleType Bool
    |   _  => raise type_error("Invalid arguements to binary operator " ^ binopToString(b) ^ " in statement " ^ Int.toString(!statement) ^ ": " ^ typetoString(typeExp(e1, env)) ^ " and " ^ typetoString(typeExp(e2, env)))
and
typeUnExp(u: unop, e: exp, env: typeEnvironment): compoundtype =
    case(u, typeExp(e, env)) of
        (Negate, SingleType Int) => SingleType Int
    |   (Not, SingleType Bool) => SingleType Bool
    |   _ => raise type_error("Invalid arguement to unary operator " ^ unopToString(u) ^ " in statement " ^ Int.toString(!statement) ^ ": " ^ typetoString(typeExp(e, env)))
and
typeIfExp(e1: exp, e2: exp, e3: exp, env: typeEnvironment): compoundtype =
    let
        val t1 = typeExp(e1, env)
        val t2 = typeExp(e2, env)
        val t3 = typeExp(e3, env)
    in
        if t1 = SingleType Bool andalso t2 = t3 then t2 else raise type_error("Invalid arguments to if expression in statement " ^ Int.toString(!statement) ^ ": " ^ typetoString(t1) ^ ", " ^ typetoString(t2) ^ " and " ^ typetoString(t3))
    end
and
typeAppExp(e1: exp, e2: exp, env: typeEnvironment): compoundtype = 
    case(typeExp(e1, env), typeExp(e2, env)) of
        (CompoundType (t1, t2), t3) => if t1 = t3 then t2 else raise type_error("Invalid argument in application expression in statement " ^ Int.toString(!statement) ^ ": " ^ typetoString(t3))
    |   _ => raise type_error("First expression in application not a function in statement " ^ Int.toString(!statement) ^ ": " ^ typetoString(typeExp(e1, env)))
end
