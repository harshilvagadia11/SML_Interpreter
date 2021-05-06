structure EVALUATOR  =
struct
open AST

exception runtime_error;

fun xor(a: bool, b: bool) = (a orelse b) andalso not(a andalso b);

val brokenTypes = runtime_error

fun evalProgram(p: program, env: environment): value list = 
    case p of
        Leaf e => [evalExp(e, env)]
    |   Node (e, p1) => case e of
            FunExp (x1, x2, t1, t2, e) => (FunVal (x2, e, [])) :: evalProgram(p1, envAdd(x1, FunVal (x2, e, []), env))
        |   _ => evalExp(e, env) :: evalProgram(p1, env)
and
evalExp(e: exp, env: environment): value =
    case e of
	    NumExp i => IntVal i
      | BoolExp b => BoolVal b
      | VarExp x => envLookup(x, env) 				  
      | BinExp (b, e1, e2)  => evalBinExp(b, e1, e2, env)
      | UnExp (u, e) => evalUnExp(u, e, env)
      | LetExp(ValDecl(x, e1), e2)  =>
        let
            val v1 = evalExp (e1, env)
        in
            evalExp(e2, envAdd (x, v1, env))
        end
      | LetExp(FunDecl(x, y, t1, t2, e1), e2) => evalExp(e2, envAdd(x, FunVal(y, e1, []), env)) 
      | IfExp (e1, e2, e3) => evalIfExp(e1, e2, e3, env)
      | FnExp (x, t1, t2, e) => FunVal(x, e, [])
      | FunExp(x1, x2, t1, t2, e) => FunVal(x2, e, [])
      | AppExp (e1, e2) => evalAppExp(e1, e2, env) 
and
evalBinExp(b: binop, e1: exp, e2: exp, env: environment):value =
    case (b, evalExp(e1, env), evalExp(e2, env))  of
        (Add, IntVal i1, IntVal i2) => IntVal (i1+i2)
    |   (Sub, IntVal i1, IntVal i2) => IntVal (i1-i2)
    |   (Mul, IntVal i1, IntVal i2) => IntVal (i1*i2)
    |   (Div, IntVal i1, IntVal i2) => IntVal (i1 div i2)
    |   (Equals, IntVal i1, IntVal i2)  => BoolVal (i1 = i2)
    |   (Equals, BoolVal b1, BoolVal b2)  => BoolVal (b1 = b2)
    |   (LessThan, IntVal i1, IntVal i2)  => BoolVal (i1 < i2)
    |   (GreaterThan, IntVal i1, IntVal i2)  => BoolVal (i1 > i2)
    |   (And, BoolVal b1, BoolVal b2)  => BoolVal (b1 andalso b2)
    |   (Or, BoolVal b1, BoolVal b2)  => BoolVal (b1 orelse b2)
    |   (Xor, BoolVal b1, BoolVal b2)  => BoolVal (xor(b1, b2))
    |   (Implies, BoolVal b1, BoolVal b2)  => BoolVal ((not b1) orelse b2)
    |   _  => raise brokenTypes
and
evalUnExp(u: unop, e: exp, env: environment): value =
    case(u, evalExp(e, env)) of
        (Negate, IntVal i) => IntVal (~i)
    |   (Not, BoolVal b) => BoolVal (not b)
    |   _ => raise brokenTypes
and
evalIfExp(e1: exp, e2: exp, e3: exp, env: environment): value =
    case evalExp(e1, env) of
        BoolVal b => if b then evalExp(e2, env) else evalExp(e3, env)
    |   _ => raise brokenTypes
and
evalAppExp(e1: exp, e2: exp, env: environment): value = 
    case(evalExp(e1, env), evalExp(e2, env)) of
        (FunVal (x, e, env1), v) => updateFunEnv(evalExp(e, envConcat(envAdd(x, v, env1), env)), envAdd(x, v, env1))
    |   _ => raise brokenTypes
and
updateFunEnv(f: value, env1: environment): value = 
    case f of
        FunVal(y, e, env) => FunVal(y, e, envConcat(env1, env))
    |   _ => f
end
