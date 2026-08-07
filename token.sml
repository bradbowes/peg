structure Token =
struct
   datatype token
      = LEFTARROW
      | RIGHTARROW
      | SLASH
      | AND
      | NOT
      | QUESTION
      | STAR
      | PLUS
      | LPAREN
      | RPAREN
      | DOT
      | LIT of string
      | CLASS of string
      | ID of string
      | ACTION of string
      | EOF

   fun toString t = case t of
        LEFTARROW    => "<-"
      | RIGHTARROW   => "=>"
      | SLASH        => "/"
      | AND          => "&"
      | NOT          => "!"
      | QUESTION     => "?"
      | STAR         => "*"
      | PLUS         => "+"
      | LPAREN       => "("
      | RPAREN       => ")"
      | DOT          => "."
      | LIT s        => "\"" ^ s ^ "\""
      | CLASS s      => "[" ^ s ^ "]"
      | ID s         => s
      | ACTION s     => ""
      | EOF          => "<EOF>"

end
