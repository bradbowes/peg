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

      fun getClass cls = let
         fun loop (str, ls) = case str of
              x1 :: #"-" :: x2 :: xs   => loop (xs, (Ast.RGE (x1, x2)) :: ls)
            | _ :: #"-" :: []          => raise Fail "incomplete range"
            | x :: xs                  => loop (xs, Ast.CHR x :: ls)
            | []                       => Ast.ALT (List.rev ls)
      in loop (String.explode cls, []) end

      fun getPrimary (t, c) = case t of
           Token.LIT lit   => (Ast.SEQ (map Ast.CHR (String.explode lit)), c)
         | Token.CLASS cls => (getClass cls, c)
         | Token.ID id     => (Ast.NT id, c)
         | Token.DOT       => (Ast.ANY, c)
         | Token.LPAREN    => let
                                 val (e, c) = getExpression c
                                 val c = eat (Token.RPAREN, c)
                               in (e, c) end
         | _               => raise Err.Peg ("expected primary", c)

      and getSuffix (t, c) = let
         val (p, c) = getPrimary (t, c)
         val (t, c') = lex c
      in case t of
           Token.QUESTION  => (Ast.OPT p, c')
         | Token.STAR      => (Ast.REP0 p, c')
         | Token.PLUS      => (Ast.REP1 p, c')
         | _               => (p, c)
      end

      and getPrefix (t, c) = case t of
           Token.AND => let val (s, c) = getSuffix (lex c) in (Ast.PEEK s, c) end
         | Token.NOT => let val (s, c) = getSuffix (lex c) in (Ast.NOT s, c) end
         | _         => getSuffix (t, c)

      and getSequence c = let
         fun loop (ls, c) = let
            val (t, c') = lex c

            fun continue () = let
               val (item, c) = getPrefix (t, c')
            in loop (item :: ls, c) end

            fun done () = case ls of
                 []        => raise Err.Peg ("expected sequence", c)
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
      in
         loop ([seq], c)
      end

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

   fun parseFile f =
      parse (TextIO.inputAll (TextIO.openIn f))

end

