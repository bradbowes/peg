structure Err =
struct

   exception Peg of string * int

   fun formatMsg (s, msg, pos) = let
      val sz = String.size s
      val n = Int.min (sz, pos)

      fun eol p =
         if p >= sz then sz
         else if String.sub (s, p) = #"\n" then p + 1
         else eol (p + 1)

      fun spaces n = let
         fun loop (str, n) =
            if n > 0 then loop (str ^ " ", n - 1) else str
      in loop ("", n) end

      fun loop (last, next, line) =
         if next >= n
         then
            let val t = String.substring (s, last, next - last)
            in
               "Error in line " ^ (Int.toString line) ^ ": " ^ msg ^ "\n" ^
               t ^ (if String.isSuffix "\n" t then "" else "\n") ^
               spaces (n - last) ^ "^" ^ "\n"
            end
         else loop (next, eol next, line + 1)

   in loop (0, eol 0, 1) end

   fun print s = (TextIO.output (TextIO.stdErr, s ^ "\n"); TextIO.flushOut TextIO.stdErr)

end

