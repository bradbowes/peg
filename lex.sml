structure Lexer : sig
   val lexer : string -> int -> Token.token * int
end =
struct
   open Token
   fun lexer s = let
      val sz = size s

      fun getChar pos =
      if pos >= sz then #"\^D" else String.sub (s, pos)

      fun skipWhite pos = let
         val ch = getChar pos
         fun skipComment pos = let
            val ch = getChar pos
         in
            if ch = #"\r" orelse ch = #"\n" orelse ch = #"\^D"
            then skipWhite (pos + 1)
            else skipComment (pos + 1)
         end
      in
         if Char.isSpace ch then skipWhite (pos + 1)
         else if ch = #"#" then skipComment (pos + 1)
         else (ch, pos)
      end

      fun getId (ls, pos) = let
         val ch = getChar pos
      in
         if Char.isAlphaNum ch orelse ch = #"-"
         then getId (ch :: ls, pos + 1)
         else (ID (String.implode (List.rev ls)), pos)
      end

      fun getLiteral (tok, delim, pos) = let
         fun escape (ch, pos) = case ch of
              #"\^D" => raise Err.Peg ("unexpected end of input", pos)
            | #"\\"  => (
               let val ch = getChar (pos + 1)
               in case ch of
                    #"\^D" => raise Err.Peg ("unexpected end of input", pos)
                  | #"\\"  => ch
                  | #"'"   => ch
                  | #"\""  => ch
                  | #"["   => ch
                  | #"]"   => ch
                  | #"n"   => #"\n"
                  | #"t"   => #"\t"
                  | #"r"   => #"\r"
                  | _      => raise Err.Peg ("illegal escape sequence", pos)
               end, pos + 2)
            | _      => (ch, pos + 1)

         fun loop (ls, pos) = let
            val c = getChar pos
         in
            if c = delim
            then (tok (String.implode (List.rev ls)), pos + 1)
            else let val (c, p) = escape (c, pos) in loop (c :: ls, p) end
         end
      in loop ([], pos) end

	in
      fn pos => let
			val (ch, p) = skipWhite pos
			fun look n = getChar (p + n)
		in case ch of
			  #"\^D" => (EOF, p)
			| #"+"   => (PLUS, p + 1)
			| #"*"   => (STAR, p + 1)
			| #"/"   => (SLASH, p + 1)
			| #"."   => (DOT, p + 1)
			| #"&"   => (AND, p + 1)
			| #"!"   => (NOT, p + 1)
			| #"?"   => (QUESTION, p + 1)
			| #"("   => (LPAREN, p + 1)
			| #")"   => (RPAREN, p + 1)
			| #"["   => getLiteral (CLASS, #"]", p + 1)
			| #"'"   => getLiteral (LIT, ch, p + 1)
			| #"\""  => getLiteral (LIT, ch, p + 1)
			| #"<"   => if (look 1) = #"-" then (LEFTARROW, p + 2)
							else raise Err.Peg ("illegal character", p)
			| _      =>
				if Char.isAlpha ch then getId ([ch], p + 1)
				else raise Err.Peg ("illegal character", p)
		end
   end
end

