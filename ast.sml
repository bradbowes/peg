structure Ast =
struct
   datatype node   =
        CHR of char
      | RGE of char * char
      | ANY
      | SEQ of node list
      | ALT of node list
      | OPT of node
      | REP0 of node
      | REP1 of node
      | PEEK of node
      | NOT of node
      | NT of string
end

