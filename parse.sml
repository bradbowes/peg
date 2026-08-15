structure Parser =
struct
   fun parse s = let
      val lex = Lexer.lexer s

      fun eat (tok, c) = let
         val (t, c') = lex c
      in
         if t = tok then c'
         else raise Err.Peg (
            ("expected '" ^ Token.toString tok ^ "', found '" ^ Token.toString t ^ "'"),
            c)
      end

      fun getClass (cls, start) = let
         fun loop (str, ls) = case str of
              x1 :: #"-" :: x2 :: xs   => loop (xs, (Ast.RGE (x1, x2)) :: ls)
            | _ :: #"-" :: []          => raise Err.Peg ("incomplete range", start)
            | x :: xs                  => loop (xs, Ast.CHR x :: ls)
            | []                       =>
                  let val alts = List.rev ls
                  in case alts of
                       []        => raise Err.Peg ("empty class not allowed", start)
                     | x :: []   => x
                     | _         => Ast.ALT alts
                  end
      in loop (String.explode cls, []) end

      fun getLiteral (lit, start) = let
         val ls = map Ast.CHR (String.explode lit)
      in case ls of
           []        => raise Err.Peg ("empty string not allowed", start)
         | x :: []   => x
         | _         => Ast.SEQ ls
      end

      fun getPrimary (t, c, start) = case t of
           Token.LIT lit   => (getLiteral (lit, start), c)
         | Token.CLASS cls => (getClass (cls, start), c)
         | Token.ID id     => (Ast.NT id, c)
         | Token.DOT       => (Ast.ANY, c)
         | Token.LPAREN    => let
                                 val (e, c) = getExpression c
                                 val c = eat (Token.RPAREN, c)
                               in (e, c) end
         | _               => raise Err.Peg ("expected primary", start)

      and getSuffix (t, c, start) = let
         val (p, c) = getPrimary (t, c, start)
         val (t, c') = lex c
      in case t of
           Token.QUESTION  => (Ast.OPT p, c')
         | Token.STAR      => (Ast.REP0 p, c')
         | Token.PLUS      => (Ast.REP1 p, c')
         | _               => (p, c)
      end

      and getPrefix (t, c, start) = let
         val (t', c') = lex c
      in case t of
           Token.AND => let val (s, c) = getSuffix (t', c', c) in (Ast.PEEK s, c) end
         | Token.NOT => let val (s, c) = getSuffix (t', c', c) in (Ast.NOT s, c) end
         | _         => getSuffix (t, c, start)
      end

      and getSequence c = let
         fun loop (ls, c) = let
            val (t, c') = lex c

            fun continue () = let
               val (item, c) = getPrefix (t, c', c)
            in loop (item :: ls, c) end

            fun done () = case ls of
                 []        => raise Err.Peg ("expected expression", c)
               | x :: []   => (x, c)
               | _         => (Ast.SEQ (List.rev ls), c)
         in
            case t of
                 Token.ID id     => let
                        val (t, _) = lex c'
                     in
                        if t = Token.LEFTARROW then done ()
                        else continue ()
                     end
               | Token.AND       => continue ()
               | Token.NOT       => continue ()
               | Token.LIT _     => continue ()
               | Token.CLASS _   => continue ()
               | Token.DOT       => continue ()
               | Token.LPAREN    => continue ()
               | _               => done ()
         end
      in loop ([], c) end

      and getExpression c = let
         fun loop (ls, c) = let
            val (t, c') = lex c
         in
            case t of
                 Token.SLASH  => let val (seq, c) = getSequence c'
                                 in loop (seq :: ls, c) end
               | _            => (case ls of
                          []        => raise Err.Peg ("expected expression", c)
                        | x :: []   => (x, c)
                        | _         => (Ast.ALT (List.rev ls), c) )
         end
         val (seq, c) = getSequence c
      in loop ([seq], c) end

      fun getDef (grm, (t, c)) = let
         val id = case t of
                       Token.ID s => s
                     | _            => raise Err.Peg (("expected identifier, found '"
                                         ^ (Token.toString t) ^ "'"), c)
         val c = eat (Token.LEFTARROW, c)
         val (exp, c) = getExpression c
      in (Map.insert (grm, id, exp), c) end

      fun getDefs (grm, c) = let
         val (t, c) = lex c
      in
         case t of
              Token.EOF => grm
            | _         => getDefs (getDef (grm, (t, c)))
      end
   in
      getDefs (Map.empty, 0)
      handle Err.Peg (msg, p) => (Err.print (Err.formatMsg (s, msg, p)); Map.empty)
   end

   fun parseFile file = let
      val input = TextIO.openIn file
      val s = TextIO.inputAll input
      val _ = TextIO.closeIn input
   in parse s end

fun show s = let val g = parse ("a<-" ^ s) in Map.lookup (g, "a") end
end

