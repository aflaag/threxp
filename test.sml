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
  (* | precedence Sub = 0 *)
  | precedence Mul = 1
  | precedence _ = raise UnsupportedToken

(* fun pop_stack ([], _) = [] *)
(*   | pop_stack (stack_top::stack, operator) = *)
(*       if (precedence stack_top) >= (precedence operator) *)
(*       then pop_stack(stack, operator) *)
(*       else stack *)

fun rpnify ([], []) = []
  | rpnify (token::stack, []) = token::rpnify(stack, [])
  | rpnify (stack, Lparen::rest) = rpnify(Lparen::stack, rest)

  (* | rpnify ([], Rparen::rest) = raise UnsupportedToken *)
  | rpnify (stack_top::stack, Rparen::rest) =
      if stack_top = Lparen
      then rpnify(stack, rest)
      else stack_top::rpnify(stack, Rparen::rest)
  | rpnify (stack, (Int x)::rest) = (Int x)::rpnify(stack, rest)

  | rpnify (stack, Mul::rest) = rpnify(Mul::stack, rest)

  | rpnify ([], Add::rest) = rpnify([Add], rest)
  | rpnify (stack_top::stack, Add::rest) =
      if precedence stack_top >= precedence Add
      then stack_top::rpnify(stack, Add::rest)
      else rpnify(Add::stack, rest)

  | rpnify (_, _) = raise UnsupportedToken;

val tokens = tokenize "3 + 4 * (2 + 5)";
(* val tokens = tokenize " 0012 + 24 "; *)

val rpn = rpnify([], tokens);

OS.Process.exit(OS.Process.success);
