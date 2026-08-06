structure Parser =
struct
   fun parse s = let
      val lex = Lexer.lexer s

      fun eat (tok, c, err) = let
         val (t, c') = lex c
      in
         if t = tok then c'
         else raise (Fail err)
      end

      fun getClass cls = let
         fun loop (str, ls) = case str of
              x1 :: #"-" :: x2 :: xs   => loop (xs, (Ast.RGE (x1, x2)) :: ls)
            | _ :: #"-" :: []          => raise (Fail "incomplete range")
            | x :: xs                  => loop (xs, Ast.CHR x :: ls)
            | []                       => Ast.ALT (List.rev ls)
      in loop (String.explode cls, []) end

      fun getPrimary (t, c) = case t of
           Token.LIT lit   => (Ast.SEQ (map Ast.CHR (String.explode lit)), c)
         | Token.CLASS cls => (getClass cls, c)
         | Token.ID id     => (Ast.NT id, c)
         | Token.DOT       => (Ast.ANY, c)
         | _               => raise (Fail "not implemented")

      fun getSuffix (t, c) = let
         val (p, c) = getPrimary (t, c)
         val (t, c') = lex c
      in case t of
           Token.QUESTION  => (Ast.OPT p, c')
         | Token.STAR      => (Ast.REP0 p, c')
         | Token.PLUS      => (Ast.REP1 p, c')
         | _               => (p, c)
      end

      fun getPrefix (t, c) = case t of
           Token.AND => let val (s, c) = getSuffix (lex c) in (Ast.PEEK s, c) end
         | Token.NOT => let val (s, c) = getSuffix (lex c) in (Ast.NOT s, c) end
         | _         => getSuffix (t, c)

      fun getSequence c = let
         fun loop (ls, c) = let
            val (t, c') = lex c

            fun continue () = let
               val (item, c) = getPrefix (t, c')
            in loop (item :: ls, c) end

            fun done () = case ls of
                 []        => raise (Fail "expected sequence")
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

      fun getExpression c = let
         fun loop (ls, (t, c)) = case t of
              Token.SLASH  => let val (seq, c) = getSequence c
                              in loop (seq :: ls, (lex c)) end
            | _            => (case ls of
                       []        => raise (Fail "expected expression")
                     | x :: []   => (x, c)
                     | _         => (Ast.ALT (List.rev ls), c) )
         val (seq, c) = getSequence c
      in
         loop ([seq], (lex c))
      end

      fun getDef (grm, (t, c)) = let
         val id = case t of Token.ID s => s | _ => raise (Fail "expected identifier")
         val c = eat (Token.LEFTARROW, c, "expected '<-'")
         val (exp, c) = getExpression c
      in (Map.insert (grm, id, exp), c) end

      fun getDefs (grm, c) = let
         val (t, c) = lex c
      in
         case t of
              Token.EOF => grm
            | _         => getDefs (getDef (grm, (t, c)))
      end
   in getDefs (Map.empty, 0) end
end

