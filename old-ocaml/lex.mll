(* lex.mll
   A lexer for Dumblang.


   Simon Heath
   18/4/2005
*)

{

open Parse
exception Eof
exception LexerError of string

let inComment = ref 0;;

let chrNum = ref 0;;
let lineNum = ref 0;;

let nl () =
  chrNum := 0;
  incr lineNum
;;

(* Abbreviation for the func that returns the string
   being lexed.
*)
let gs = Lexing.lexeme;;

(* Advances the position of the error-checking vars. *)
let adv lb =
  chrNum := !chrNum + (String.length (Lexing.lexeme lb));;

let str2float x =
   Scanf.sscanf x "%f" (fun x -> x)
;;

let str2int x =
   Scanf.sscanf x "%i" (fun x -> x)
;;
(*
let str2char x =
   Scanf.sscanf x "%C" (fun x -> x) 
;;

let str2str x =
   Scanf.sscanf x "%S" (fun x -> x) 
;;
*)

}


let id = 
        ['a'-'z' 'A'-'Z' '_']*

let inum =
   '-'?(['0'-'9']+|"0x"['0'-'9''a'-'f''A'-'F']+|"0o"['0'-'7']+)
let bnum =
   '-'?"0b"['0''1']+
let fnum =
   '-'?['0'-'9']+'.'['0'-'9']*
(*
let chr =
   ("'"_"'") | ("'\\"(inum|bnum)"'") | ("'\\"("n"|"b"|"r"|"t"|"'"|"\\")"'")

let str = '"'([^'"''\\']|'\\'_)*'"'
*)

rule token = parse
   ";"			{ adv lexbuf; lcomment lexbuf }
 | "/-"			{ adv lexbuf; incr inComment; bcomment lexbuf }
 | (inum|bnum)		{ adv lexbuf; INT( str2int (gs lexbuf) ) }
(* | fnum			{ adv lexbuf; FLOAT( str2float (gs lexbuf) ) } *)
 (*| chr                  { adv lexbuf; CHAR( str2char (gs lexbuf) ) } *)
 | "\n"			{ nl (); token lexbuf }
 | [' ''\t']		{ adv lexbuf; token lexbuf }
 | "["                  { adv lexbuf; LBRACK }
 | "]"                  { adv lexbuf; RBRACK }
 | "("                  { adv lexbuf; LPAREN }
 | ")"                  { adv lexbuf; RPAREN }
 | "!"                  { adv lexbuf; BANG }
 | "@"                  { adv lexbuf; AT }
 | ","                  { adv lexbuf; COMMA }
 | "Null"               { adv lexbuf; NULL }


 | id			{ adv lexbuf; let s = gs lexbuf in
 Printf.printf "Symbol read: '%s'\n" s;
SYMBOL( s ) }
 | eof			{ EOF }
 | _			{ raise (LexerError( "Invalid token!" )) }

and bcomment = parse
   "/-"			{ adv lexbuf; incr inComment; 
   (*Printf.printf "Pushing comment stack, now %d\n" !inComment;*)
                          bcomment lexbuf }
 | "-/"			{ adv lexbuf; decr inComment; 
   (*Printf.printf "Popping comment stack, now %d\n" !inComment;*)
                          if !inComment <= 0 then token lexbuf
			                    else bcomment lexbuf }
 | '\n'			{ nl (); bcomment lexbuf }
 | _			{ adv lexbuf; bcomment lexbuf } 


and lcomment = parse
   '\n'			{ nl (); token lexbuf }
 | _			{ adv lexbuf; lcomment lexbuf }
