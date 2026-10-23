(* The default instance: [Text_base] plus the string, UTF-8 and lexeme
   layers. *)

include Text_base
include Text_strings
module Utf8 = Text_utf8
module Lexeme = Text_lexeme
