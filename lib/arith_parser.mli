open Parser

val digit : int t
val addop : (int -> int -> int) t
val mulop : (int -> int -> int) t
val factor : int t
val term : int t
val expr : int t
