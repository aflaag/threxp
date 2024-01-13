open SMLofNJ.Cont;

Control.Print.printDepth := 1024;

datatype expr = Int of int
              | Id of string
              | Add of expr * expr
              | Sub of expr * expr
              | Mul of expr * expr
              | Div of expr * expr
              | Let of string * expr * expr

datatype token = TInt of int
               | TAdd
               | TSub
               | TMul
               | TDiv
               | TLet
               | TEqual
               | TIn
               | TEnd
               | TRParen
               | TLParen

exception UnsupportedOperand;
exception UnsupportedToken;
exception MismatchedParenthesis;
exception SyntaxError;
exception UnknownError;
exception EmptyProgram;

fun power (a, 0) = 1
  | power (a, b) = a * power(a, b - 1)

fun empty_stack stack =
  let 
    fun empty_stack_internals (n, []) = 0
      | empty_stack_internals (n, (TInt x)::rest) = x * power(10, n) + empty_stack_internals(n + 1, rest)
      | empty_stack_internals (_, _) = raise UnknownError
  in
    empty_stack_internals(0, stack)
  end

fun tokenize expression =
  let
    fun tokenize_internals (stack, chars) =
      case (stack, chars) of
           (NONE, []) => []
         | (SOME(digits), []) => (TInt (empty_stack digits))::tokenize_internals(NONE, [])
         | (stack, c::rest) =>
             if Char.isDigit c
             then tokenize_internals(SOME (
               (TInt (Char.ord c - 48))::(
                 case stack of
                      NONE => []
                    | SOME(digits) => digits
               )
             ), rest)
             else
               case (stack, c) of
                    (NONE, #"+") => TAdd::tokenize_internals(NONE, rest)
                  | (NONE, #"-") => TSub::tokenize_internals(NONE, rest)
                  | (NONE, #"*") => TMul::tokenize_internals(NONE, rest)
                  | (NONE, #"/") => TDiv::tokenize_internals(NONE, rest)
                  | (NONE, #"(") => TLParen::tokenize_internals(NONE, rest)
                  | (NONE, #")") => TRParen::tokenize_internals(NONE, rest)
                  | (NONE, #" ") => tokenize_internals(NONE, rest)
                  | (SOME(digits), #"+") => (TInt (empty_stack digits))::TAdd::tokenize_internals(NONE, rest)
                  | (SOME(digits), #"-") => (TInt (empty_stack digits))::TSub::tokenize_internals(NONE, rest)
                  | (SOME(digits), #"*") => (TInt (empty_stack digits))::TMul::tokenize_internals(NONE, rest)
                  | (SOME(digits), #"/") => (TInt (empty_stack digits))::TDiv::tokenize_internals(NONE, rest)
                  | (SOME(digits), #"(") => (TInt (empty_stack digits))::TLParen::tokenize_internals(NONE, rest)
                  | (SOME(digits), #")") => (TInt (empty_stack digits))::TRParen::tokenize_internals(NONE, rest)
                  | (SOME(digits), #" ") => (TInt (empty_stack digits))::tokenize_internals(NONE, rest)
                  | (_, _) => raise UnsupportedOperand
  in
    tokenize_internals(NONE, (explode expression))
  end

fun precedence TLParen = ~1
  | precedence TAdd = 0
  | precedence TSub = 0
  | precedence TMul = 1
  | precedence TDiv = 1
  | precedence _ = raise UnsupportedToken

fun print_token (TInt x) = "TInt " ^ Int.toString(x)
  | print_token TAdd = "TAdd"
  | print_token TSub = "TSub"
  | print_token TMul = "TMul"
  | print_token TDiv = "TDiv"
  | print_token TRParen = "TRParen"
  | print_token TLParen = "TLParen"
  | print_token _ = raise UnsupportedToken
  (* | print_token TLet = "TLet" *)
  (* | print_token TEqual *)
  (* | print_token TIn *)
  (* | print_token TEnd *)

fun print_tokens ([]) = "\n"
  | print_tokens (t::rest) = (print_token t) ^ ", " ^ print_tokens(rest)

fun rpnify tokens =
  let
    fun rpnify_internals ([], []) = []
      | rpnify_internals (token::stack, []) = token::rpnify_internals(stack, [])

      | rpnify_internals (stack, TLParen::rest) = rpnify_internals(TLParen::stack, rest)

      | rpnify_internals ([], TRParen::rest) = raise MismatchedParenthesis
      | rpnify_internals (stack_top::stack, TRParen::rest) =
          if stack_top = TLParen
          then rpnify_internals(stack, rest)
          else stack_top::rpnify_internals(stack, TRParen::rest)

      | rpnify_internals (stack, (TInt x)::rest) = (TInt x)::rpnify_internals(stack, rest)
      
      | rpnify_internals ([], TAdd::rest) = rpnify_internals([TAdd], rest)
      | rpnify_internals (stack_top::stack, TAdd::rest) =
          if precedence stack_top >= precedence TAdd
          then stack_top::rpnify_internals(stack, TAdd::rest)
          else rpnify_internals(TAdd::stack_top::stack, rest)

      | rpnify_internals ([], TSub::rest) = rpnify_internals([TSub], rest)
      | rpnify_internals (stack_top::stack, TSub::rest) =
          if precedence stack_top >= precedence TSub
          then stack_top::rpnify_internals(stack, TSub::rest)
          else rpnify_internals(TSub::stack_top::stack, rest)

      | rpnify_internals ([], TMul::rest) = rpnify_internals([TMul], rest)
      | rpnify_internals (stack_top::stack, TMul::rest) =
          if precedence stack_top >= precedence TMul
          then stack_top::rpnify_internals(stack, TMul::rest)
          else rpnify_internals(TMul::stack_top::stack, rest)

      | rpnify_internals ([], TDiv::rest) = rpnify_internals([TDiv], rest)
      | rpnify_internals (stack_top::stack, TDiv::rest) =
          if precedence stack_top >= precedence TDiv
          then stack_top::rpnify_internals(stack, TDiv::rest)
          else rpnify_internals(TDiv::stack_top::stack, rest)

      | rpnify_internals (_, _) = raise UnsupportedToken;
  in
    rpnify_internals([], tokens)
  end

fun evaluate rpn =
  let
    val stack = ref [];

    fun evaluate_internals ([]) = ()
      | evaluate_internals ((TInt x)::rest) = (
          stack := (Int x)::(!stack);
          evaluate_internals(rest)
        )
      | evaluate_internals (TAdd::rest) = (
         case !stack of
              y::x::stack_tail => (
                stack := (Add (x, y))::stack_tail;
                evaluate_internals(rest)
              )
            | _ => raise SyntaxError
       )
      | evaluate_internals (TSub::rest) = (
         case !stack of
              y::x::stack_tail => (
                stack := (Sub (x, y))::stack_tail;
                evaluate_internals(rest)
              )
            | _ => raise SyntaxError
       )
      | evaluate_internals (TMul::rest) = (
         case !stack of
              y::x::stack_tail => (
                stack := (Mul (x, y))::stack_tail;
                evaluate_internals(rest)
              )
            | _ => raise SyntaxError
       )
      | evaluate_internals (TDiv::rest) = (
         case !stack of
              y::x::stack_tail => (
                stack := (Div (x, y))::stack_tail;
                evaluate_internals(rest)
              )
            | _ => raise SyntaxError
       )
     | evaluate_internals _ = raise UnsupportedToken
  in
    evaluate_internals rpn;

    case !stack of
         result::[] => result
       | x::rest => raise SyntaxError
       | [] => raise EmptyProgram
  end

fun exprify code = evaluate(rpnify(tokenize(code)))

val output1 = exprify "10 - (11 * (123 - 52 / 1 + 25) / (12 - (12 + (12)) * 5))";
val expected1 = Sub (Int 10, Div (Mul (Int 11,Add (Sub (Int 123,Div (Int 52,Int 1)),Int 25)), Sub (Int 12,Mul (Add (Int 12,Int 12),Int 5))));

val output2 = exprify "1 - (2 * (1 + 3 * 4 + 5))";
val expected2 = Sub (Int 1,Mul (Int 2,Add (Add (Int 1,Mul (Int 3,Int 4)),Int 5)));

val output3 = exprify "(12 + 3) * (4 - 9)";
val expected3 = Mul (Add (Int 12,Int 3),Sub (Int 4,Int 9));

val output4 = exprify "3 + 4 * (12 + 5)";
val expected4 = Add (Int 3, Mul (Int 4, Add(Int 12, Int 5)));

val output5 = exprify "7 + (4 * (5 + 6))";
val expected5 = Add (Int 7, Mul (Int 4, Add(Int 5, Int 6)));

val output6 = exprify "1 + 3 * 4 + 5";
val expected6 = Add (Add (Int 1,Mul (Int 3,Int 4)),Int 5)

val output7 = exprify "5 + (5 + (5 + 5))";
val expected7 = Add (Int 5, Add (Int 5, Add(Int 5, Int 5)))

val check1 = output1 = expected1;
val check2 = output2 = expected2;
val check3 = output3 = expected3;
val check4 = output4 = expected4;
val check5 = output5 = expected5;
val check6 = output6 = expected6;
val check7 = output7 = expected7;

OS.Process.exit(OS.Process.success);
