open Parser

let digit =
  let is_digit = function
    | '0' | '1' | '2' | '3' | '4'
    | '5' | '6' | '7' | '8' | '9' -> true
    | _ -> false
  in
  let* x = token (sat is_digit) in
  return (int_of_char x - int_of_char '0')
and addop =
  ( let* _ = symb ['+'] in return (+) )
  +++
  ( let* _ = symb ['-'] in return (-) )
and mulop =
  ( let* _ = symb ['*'] in return ( * ) )
  +++
  ( let* _ = symb ['/'] in return (/) )

(* expr uses term, which users factor, which uses term, ... *)
let rec expr_lazy = lazy (chainl1 (Lazy.force term_lazy) addop)
and term_lazy = lazy (chainl1 (Lazy.force factor_lazy) mulop)
and factor_lazy =
  lazy (
    digit +++
    ( let* _ = symb ['('] in
      let* n = Lazy.force expr_lazy in
      let* _ = symb [')'] in
      return n
    )
  )

let expr = Lazy.force expr_lazy
let term = Lazy.force term_lazy
let factor = Lazy.force factor_lazy
