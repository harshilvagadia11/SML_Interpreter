open EVALUATOR
open TYPECHECKER

structure CalcLrVals = CalcLrValsFun(structure Token = LrParser.Token)
structure CalcLex = CalcLexFun(structure Tokens = CalcLrVals.Tokens);
structure CalcParser =
	  Join(structure LrParser = LrParser
     	       structure ParserData = CalcLrVals.ParserData
     	       structure Lex = CalcLex)
     
fun invoke lexstream =
		let fun print_error (s, pos, col) =
		    	TextIO.output(TextIO.stdOut, "Syntax Error: " ^ (Int.toString pos) ^ ":" ^ (Int.toString col) ^ ":" ^ s ^ "\n")
		in
		    CalcParser.parse(0,lexstream,print_error,())
		end

fun stringToLexer str =
    let val done = ref false
    	val lexer =  CalcParser.makeLexer (fn _ => if (!done) then "" else (done:=true;str))
    in
	lexer
    end	
		
fun parse (lexer) =
    let val dummyEOF = CalcLrVals.Tokens.EOF(0,0)
    	val (result, lexer) = invoke lexer
	val (nextToken, lexer) = CalcParser.Stream.get lexer
    in
        if CalcParser.sameToken(nextToken, dummyEOF) then result
 	else (TextIO.output(TextIO.stdOut, "Warning: Unconsumed input \n"); result)
    end

val parseString = parse o stringToLexer

val typeString = fn (str: string) => typeProgram(parseString str, [])
val evaluateString = fn (str: string) => evalProgram(parseString str, [])

fun parseFile (file) = 
	let
		val In = TextIO.openIn(file)
		val s = TextIO.inputAll(In)
		val tree = parseString s;
		val t = typeProgram(tree, [])
		val v = evalProgram(tree, [])
		val _ = TextIO.closeIn(In)
		fun process (t, v) = case (t,v) of
			(tt::ts, vv::vs) => (TextIO.output(TextIO.stdOut, valToString(vv) ^ ": " ^ typetoString(tt) ^ "\n"); process(ts, vs))
		|	([], []) => ()
		|	_ => TextIO.output(TextIO.stdOut, "Runtime Error\n")
	in
		TextIO.output(TextIO.stdOut, programToString(tree) ^ "\n\n");
		process(t, v)
	end handle type_error(s) => TextIO.output(TextIO.stdOut, s ^ "\n")
		|	environment_error(s) => TextIO.output(TextIO.stdOut, s ^ "\n")
		|	runtime_error => TextIO.output(TextIO.stdOut, "Runtime Error\n")

val args = CommandLine.arguments()
val file = List.nth(args, 0)

fun main() = (parseFile(file))

val _ = main()
