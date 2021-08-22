open Parser

let digit =
  let is_digit = function
    | '0' | '1' | '2' | '3' | '4'
    | '5' | '6' | '7' | '8' | '9' -> true
    | _ -> false
  in
  token (sat is_digit) >>= fun x ->
  return (int_of_char x - int_of_char '0')
and addop =
  (symb ['+'] >>= fun _ -> return (+))
  +++ (symb ['-'] >>= fun _ -> return (-))
and mulop =
  (symb ['*'] >>= fun _ -> return ( * ))
  +++ (symb ['/'] >>= fun _ -> return (/))

(* expr uses term, which users factor, which uses term, ... *)
let rec expr_lazy = lazy (chainl1 (Lazy.force term_lazy) addop)
and term_lazy = lazy (chainl1 (Lazy.force factor_lazy) mulop)
and factor_lazy =
  lazy (
    digit +++
    ( symb ['('] >>= fun _ ->
      (Lazy.force expr_lazy) >>= fun n ->
      symb [')'] >>= fun _ ->
      return n
    )
  )

let expr = Lazy.force expr_lazy
let term = Lazy.force term_lazy
let factor = Lazy.force factor_lazy
