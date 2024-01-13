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
                  | (NONE, #" ") => tokenize_internals(NONE, rest)
                  | (SOME(digits), #"+") => (Int (empty_stack digits))::Add::tokenize_internals(NONE, rest)
                  | (SOME(digits), #"*") => (Int (empty_stack digits))::Mul::tokenize_internals(NONE, rest)
                  | (SOME(digits), #" ") => (Int (empty_stack digits))::tokenize_internals(NONE, rest)
                  | (_, _) => raise UnsupportedOperand
  in
    tokenize_internals(NONE, (explode expression))
  end;

fun rpnify ([], []) = []
  | rpnify (token::stack, []) = token::rpnify(stack, [])
  | rpnify (stack, Add::rest) = rpnify(Add::stack, rest)

  (* handle integers *)
  | rpnify (stack, (Int x)::rest) = (Int x)::rpnify(stack, rest)

  | rpnify (_, _) = raise UnsupportedToken;

(* val tokens = tokenize "3 + 4 * 2 + 5"; *)
val tokens = tokenize " 0 32   +  0  4  78";

(* val rpn = rpnify ([], tokens); *)

OS.Process.exit(OS.Process.success);
