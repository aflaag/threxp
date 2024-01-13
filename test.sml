open SMLofNJ.Cont;

datatype expr = Int of int
              | Id of string
              | Add of expr * expr
              | Sub of expr * expr
              | Mul of expr * expr
              | Div of expr * expr
              | Let of string * expr * expr

datatype token = Int of int
               | Add
               | Sub
               | Mul
               | Div
               | Let
               | Equal
               | In
               | End
               | Rparen
               | Lparen

exception InvalidStack;
exception UnsupportedOperand;
exception UnsupportedToken;
exception MismatchedParenthesis;

fun power (a, 0) = 1
  | power (a, b) = a * power(a, b - 1)

fun empty_stack stack =
  let 
    fun empty_stack_internals (n, []) = 0
      | empty_stack_internals (n, t::rest) = (
          case t of
               Int(x) => x
             | _ => raise InvalidStack
        ) * power(10,n) + empty_stack_internals(n + 1, rest)
  in
    empty_stack_internals(0, stack)
  end

fun tokenize expression =
  let
    fun tokenize_internals (stack, chars) =
      case (stack, chars) of
           (NONE, []) => []
         | (SOME(digits), []) => (Int (empty_stack digits))::tokenize_internals(NONE, [])
         | (stack, c::rest) =>
             if Char.isDigit c
             then tokenize_internals(SOME (
               (Int (Char.ord c - 48))::(
                 case stack of
                      NONE => []
                    | SOME(digits) => digits
               )
             ), rest)
             else
               case (stack, c) of
                    (NONE, #"+") => Add::tokenize_internals(NONE, rest)
                  | (NONE, #"*") => Mul::tokenize_internals(NONE, rest)
                  | (NONE, #"(") => Lparen::tokenize_internals(NONE, rest)
                  | (NONE, #")") => Rparen::tokenize_internals(NONE, rest)
                  | (NONE, #" ") => tokenize_internals(NONE, rest)
                  | (SOME(digits), #"+") => (Int (empty_stack digits))::Add::tokenize_internals(NONE, rest)
                  | (SOME(digits), #"*") => (Int (empty_stack digits))::Mul::tokenize_internals(NONE, rest)
                  | (SOME(digits), #"(") => (Int (empty_stack digits))::Lparen::tokenize_internals(NONE, rest)
                  | (SOME(digits), #")") => (Int (empty_stack digits))::Rparen::tokenize_internals(NONE, rest)
                  | (SOME(digits), #" ") => (Int (empty_stack digits))::tokenize_internals(NONE, rest)
                  | (_, _) => raise UnsupportedOperand
  in
    tokenize_internals(NONE, (explode expression))
  end;

fun precedence Add = 0
  | precedence Sub = 0
  | precedence Mul = 1
  | precedence Div = 1
  | precedence _ = raise UnsupportedToken

fun rpnify_internals ([], []) = []
  | rpnify_internals (token::stack, []) = token::rpnify_internals(stack, [])
  | rpnify_internals (stack, Lparen::rest) = rpnify_internals(Lparen::stack, rest)

  | rpnify_internals ([], Rparen::rest) = raise MismatchedParenthesis
  | rpnify_internals (stack_top::stack, Rparen::rest) =
      if stack_top = Lparen
      then rpnify_internals(stack, rest)
      else stack_top::rpnify_internals(stack, Rparen::rest)
  | rpnify_internals (stack, (Int x)::rest) = (Int x)::rpnify_internals(stack, rest)

  | rpnify_internals (stack, Mul::rest) = rpnify_internals(Mul::stack, rest)

  | rpnify_internals ([], Add::rest) = rpnify_internals([Add], rest)
  | rpnify_internals (stack_top::stack, Add::rest) =
      if stack_top = Lparen then rpnify_internals(Add::stack_top::stack, rest)
      else if precedence stack_top >= precedence Add then stack_top::rpnify_internals(stack, Add::rest)
      else rpnify_internals(Add::stack, rest)

  | rpnify_internals (_, _) = raise UnsupportedToken;

val tokens = tokenize "3 + 4 * (2 + 5)";
(* val tokens = tokenize " 0012 + 24 "; *)

val rpn = rpnify_internals([], tokens);

OS.Process.exit(OS.Process.success);
