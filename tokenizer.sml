use "token.sml";
use "exceptions.sml";

fun power (a, 0) = 1
  | power (a, b) = a * power(a, b - 1)

fun empty_numbers_stack stack =
  let 
    fun empty_numbers_stack_internals (n, []) = 0
      | empty_numbers_stack_internals (n, x::rest) =
          x * power(10, n) + empty_numbers_stack_internals(n + 1, rest)
  in
    empty_numbers_stack_internals(0, stack)
  end

fun empty_vars_stack stack = implode (rev stack);

fun char_map #"+" = TAdd
  | char_map #"-" = TSub
  | char_map #"*" = TMul
  | char_map #"/" = TDiv
  | char_map #"$" = TLet
  | char_map #"=" = TEqual
  | char_map #"@" = TIn
  | char_map #"!" = TEnd
  | char_map #"(" = TLParen
  | char_map #")" = TRParen
  | char_map _ = raise UnsupportedToken

fun tlsify ([]) = []
  | tlsify (TLet::(TId var)::TEqual::rest) = TLParen::(TLetStart var)::tlsify(rest)
  | tlsify (TEnd::rest) = TEnd::TRParen::tlsify(rest)
  | tlsify (t::rest) = t::tlsify(rest)

fun tokenize expression =
  let
    fun tokenize_internals (NONE, NONE, []) = []
      | tokenize_internals (SOME(digits), NONE, []) =
          (TInt (empty_numbers_stack digits))::tokenize_internals(NONE, NONE, [])
      | tokenize_internals (NONE, SOME(chars), []) =
          (TId (empty_vars_stack chars))::tokenize_internals(NONE, NONE, [])

      | tokenize_internals (SOME(_), SOME(_), _) = raise UnsupportedToken

      | tokenize_internals (numbers_stack, vars_stack, c::rest) =
          if Char.isDigit c then tokenize_internals(SOME(
            (Char.ord c - 48)::(
              case numbers_stack of
                  NONE => []
                | SOME(digits) => digits
            )
          ), vars_stack, rest)
          else if Char.isAlpha c then tokenize_internals(numbers_stack, SOME(
            c::(
              case vars_stack of
                  NONE => []
                | SOME(chars) => chars
            )
          ), rest)
          else
            case (numbers_stack, vars_stack, c) of
                 (NONE, NONE, #" ") => tokenize_internals(NONE, NONE, rest)
               | (SOME(digits), NONE, #" ") =>
                   (TInt (empty_numbers_stack digits))::tokenize_internals(NONE, NONE, rest)
               | (NONE, SOME(chars), #" ") =>
                   (TId (empty_vars_stack chars))::tokenize_internals(NONE, NONE, rest)

               | (NONE, NONE, operator) => (char_map operator)::tokenize_internals(NONE, NONE, rest)
               | (SOME(digits), NONE, operator) =>
                   (TInt (empty_numbers_stack digits))::(char_map operator)::tokenize_internals(NONE, NONE, rest)
               | (NONE, SOME(chars), operator) =>
                   (TId (empty_vars_stack chars))::(char_map operator)::tokenize_internals(NONE, NONE, rest)
               | _ => raise SyntaxError
  in
    tlsify(tokenize_internals(NONE, NONE, (explode expression)))
  end
