use "token.sml";
use "exceptions.sml";

(* power(a, b) returns a^b *)
fun power (a, 0) = 1
  | power (a, b) = a * power(a, b - 1)

(* this function evaluates the number inside the number_stack 
where each token is one single digit *)
fun empty_numbers_stack stack =
  let 
    fun empty_numbers_stack_internals (n, []) = 0
      | empty_numbers_stack_internals (n, x::rest) =
          x * power(10, n) + empty_numbers_stack_internals(n + 1, rest)
  in
    empty_numbers_stack_internals(0, stack)
  end

(* this function evaluates the variable name inside the var_stack
where each token is one of the characters of the variable name *)
fun empty_vars_stack stack = implode (rev stack);

(* this function maps every allowed character into a token *)
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

(* this is a helper function which transforms "$ x =" into
a single (TLetStart "x") token, while also placing  "(" before it
and ")" right after every "!", to handle precedence with
other operators *)
fun tlsify ([]) = []
  | tlsify (TLet::(TId var)::TEqual::rest) = TLParen::(TLetStart var)::tlsify(rest)
  | tlsify (TEnd::rest) = TEnd::TRParen::tlsify(rest)
  | tlsify (t::rest) = t::tlsify(rest)

(* given a program string, this function returns the corresponding
token list that will be used to evaluate the program *)
fun tokenize expression =
  let
    fun tokenize_internals (NONE, NONE, []) = []

        (* if there are no more characters to read in the program string, and
        there are still either digits or characters inside the respective
        stacks, dump the whole stack inside the output list *)
      | tokenize_internals (SOME(digits), NONE, []) =
          (TInt (empty_numbers_stack digits))::tokenize_internals(NONE, NONE, [])
      | tokenize_internals (NONE, SOME(chars), []) =
          (TId (empty_vars_stack chars))::tokenize_internals(NONE, NONE, [])

      | tokenize_internals (SOME(_), SOME(_), _) = raise UnsupportedToken

      | tokenize_internals (numbers_stack, vars_stack, c::rest) =
          (* if the next character is a digit, then push it
          inside the number_stack *)
          if Char.isDigit c then tokenize_internals(SOME(
            (Char.ord c - 48)::(
              case numbers_stack of
                  NONE => []
                | SOME(digits) => digits
            )
          ), vars_stack, rest)

          (* if the next character is between a...z or A...Z, then push
          inside the var_stack *)
          else if Char.isAlpha c then tokenize_internals(numbers_stack, SOME(
            c::(
              case vars_stack of
                  NONE => []
                | SOME(chars) => chars
            )
          ), rest)

          (* if the character is something else, it must be an operator *)
          else
            case (numbers_stack, vars_stack, c) of
                 (* spaces are ignored *)
                 (NONE, NONE, #" ") => tokenize_internals(NONE, NONE, rest)
               | (SOME(digits), NONE, #" ") =>
                   (TInt (empty_numbers_stack digits))::tokenize_internals(NONE, NONE, rest)
               | (NONE, SOME(chars), #" ") =>
                   (TId (empty_vars_stack chars))::tokenize_internals(NONE, NONE, rest)

                  (* if there is a token that is not a character, nor a digit,
                  the variable/number that was being built previously (if any)
                  must be dumped inside the output list *)
               | (NONE, NONE, operator) => (char_map operator)::tokenize_internals(NONE, NONE, rest)
               | (SOME(digits), NONE, operator) =>
                   (TInt (empty_numbers_stack digits))::(char_map operator)::tokenize_internals(NONE, NONE, rest)
               | (NONE, SOME(chars), operator) =>
                   (TId (empty_vars_stack chars))::(char_map operator)::tokenize_internals(NONE, NONE, rest)
               | _ => raise SyntaxError
  in
    tlsify(tokenize_internals(NONE, NONE, (explode expression)))
  end
