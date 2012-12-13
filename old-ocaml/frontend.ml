(* REP! *)

open Backend

let read string =
  let lexbuf = Lexing.from_string string in
  Parse.main Lex.token lexbuf
;;


let _ =
  let stbl = makeSymtbl() in
  let add x = 
    match x with
	Tuple( 2, [Value( Int( a ) ); Value( Int( b ) )] ) ->
	  Int( a + b )

      | _ -> raise (Fubar( "Danger, will robinson!" ))
  in

    setSymbol (Value( Symbol( "add" ) )) (Func( add )) stbl;
    setSymbol (Value( Symbol( "a" ) )) (Int( 10 )) stbl;

    let continue = ref true in

      while !continue do
	try
	  print_string "> ";
	  flush stdout;
	  print_endline (value2str (evalList (read (read_line ())) stbl));
	  flush stdout;
	with
	    UnboundSymbol( s ) -> Printf.printf "Symbol %s not found\n" s
	  | Lex.LexerError( s ) ->
	      Printf.printf "Lexer unhappy: %s\n" s
	  | Parsing.Parse_error -> print_endline "Parser unhappy."
	  | Failure( s ) -> Printf.printf "You fail: %s\n" s
	  | End_of_file -> (print_endline "Bye!"; continue := false)
      done
;;
