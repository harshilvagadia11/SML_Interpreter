structure Tokens= Tokens
  
  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token

  val pos = ref 0
  val col = ref 0
  val eof = fn () => Tokens.EOF(!pos, !col)
  val error = fn (str, pos, col) => TextIO.output(TextIO.stdOut, "Unknown Token:" ^ Int.toString(pos) ^ ":" ^ Int.toString(col) ^ ":" ^ str ^ "\n")

  fun revfold _ nil b = b
  | revfold f (hd::tl) b = revfold f tl (f(hd,b))


  val keywords =
  [
   ("end", Tokens.END),
   ("in", Tokens.IN),
   ("let", Tokens.LET),
   ("if", Tokens.IF),
   ("then", Tokens.THEN),
   ("else", Tokens.ELSE),
   ("fi", Tokens.FI),
   ("IMPLIES", Tokens.IMPLIES),
   ("AND", Tokens.AND),
   ("OR", Tokens.OR),
   ("XOR", Tokens.XOR),
   ("NOT", Tokens.NOT),
   ("PLUS", Tokens.PLUS),
   ("MINUS", Tokens.SUB),
   ("TIMES", Tokens.TIMES),
   ("DIV", Tokens.DIV),
   ("EQUALS", Tokens.EQUALS),
   ("LESSTHAN", Tokens.LESSTHAN),
   ("GREATERTHAN", Tokens.GREATERTHAN),
   ("NEGATE", Tokens.NEGATE),
   ("fn", Tokens.FN),
   ("fun", Tokens.FUN),
   ("int", Tokens.INT),
   ("bool", Tokens.BOOL)
   ]

  fun findKeywords(str: string, pos: pos, col: pos) =
  case List.find (fn (s, _) => s = str )  keywords of 
  SOME (_, tk) => tk(pos, col) 
  | NONE => if str = "TRUE" then 
              Tokens.CONST(true, pos, col)
            else if str = "FALSE" then
              Tokens.CONST(false, pos, col)
            else
              Tokens.ID (str, pos, col)
  %%
%header (functor CalcLexFun(structure Tokens:Calc_TOKENS));

alpha=[A-Za-z];
digit=[0-9];
ws = [\ \t];
%%
[\n|\r\n]       => (pos := (!pos) + 1; col := 0; lex());
{ws}+    => (col := !col + size yytext; lex());
{digit}+ => (col := !col + size yytext; Tokens.NUM
	     (List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode yytext), !pos, !col));
"+"      => (col := !col + size yytext; Tokens.PLUS(!pos,!col));
"-"      => (col := !col + size yytext; Tokens.SUB(!pos,!col));
"*"      => (col := !col + size yytext; Tokens.TIMES(!pos,!col));
"/"      => (col := !col + size yytext; Tokens.DIV(!pos,!col));
"="      => (col := !col + size yytext; Tokens.ASSIGN(!pos,!col));
"<"      => (col := !col + size yytext; Tokens.LESSTHAN(!pos,!col));
">"      => (col := !col + size yytext; Tokens.GREATERTHAN(!pos,!col));
"~"      => (col := !col + size yytext; Tokens.NEGATE(!pos,!col));
"("      => (col := !col + size yytext; Tokens.LPAREN(!pos,!col));
")"      => (col := !col + size yytext; Tokens.RPAREN(!pos,!col));
"->"      => (col := !col + size yytext; Tokens.ARROW(!pos,!col));
"=>"      => (col := !col + size yytext; Tokens.DEF(!pos,!col));
":"      => (col := !col + size yytext; Tokens.COLON(!pos,!col));
";"      => (col := !col + size yytext; Tokens.TERM(!pos,!col));
[A-Za-z][A-Za-z0-9]* => (col := !col + size yytext; findKeywords(yytext,!pos,!col));
.      => (error(yytext, !pos, !col); col := !col + size yytext; lex());