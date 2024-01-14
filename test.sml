use "expr.sml";

Control.Print.printDepth := 1024;

datatype token = TInt of int
               | TId of string
               | TAdd
               | TSub
               | TMul
               | TDiv
               | TLet
               | TLetStart of string
               | TEqual
               | TIn
               | TEnd
               | TRParen
               | TLParen

exception UnsupportedToken;
exception SyntaxError;
exception EmptyProgram;

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
  in
    tlsify(tokenize_internals(NONE, NONE, (explode expression)))
  end

fun precedence TLParen = ~1
  | precedence (TLetStart _) = ~1
  | precedence TIn = ~1
  | precedence TEnd = ~1
  | precedence TAdd = 0
  | precedence TSub = 0
  | precedence TMul = 1
  | precedence TDiv = 1
  | precedence _ = raise UnsupportedToken

fun rpnify tokens =
  let
    fun rpnify_internals ([], []) = []
      | rpnify_internals (token::stack, []) = token::rpnify_internals(stack, [])

      | rpnify_internals (stack, TIn::rest) = rpnify_internals(TIn::stack, rest)
      | rpnify_internals (stack, TEnd::rest) = rpnify_internals(TEnd::stack, rest)
      | rpnify_internals (stack, TLParen::rest) = rpnify_internals(TLParen::stack, rest)
      | rpnify_internals (stack, (TLetStart v)::rest) = rpnify_internals((TLetStart v)::stack, rest)

      | rpnify_internals ([], TRParen::rest) = raise SyntaxError
      | rpnify_internals (stack_top::stack, TRParen::rest) =
          if stack_top = TLParen
          then rpnify_internals(stack, rest)
          else stack_top::rpnify_internals(stack, TRParen::rest)

      | rpnify_internals (stack, (TInt x)::rest) = (TInt x)::rpnify_internals(stack, rest)
      | rpnify_internals (stack, (TId v)::rest) = (TId v)::rpnify_internals(stack, rest)
      
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

    val exprM = ref NONE;
    val exprN = ref NONE;

    (* val buildingM = ref false; *)
    (* val buildingN = ref false; *)

    fun evaluate_internals (_, _, []) = ()

        (* push integers and variables inside the stack *)
      | evaluate_internals (bM, bN, (TInt x)::rest) = (
          stack := (Int x)::(!stack);
          evaluate_internals(bM, bN, rest)
        )
      | evaluate_internals (bM, bN, (TId v)::rest) = (
          stack := (Id v)::(!stack);
          evaluate_internals(bM, bN, rest)
        )

      | evaluate_internals (bM, bN, TEnd::next::rest) = (
          case next of
               TIn => (
                 (* if TIn is the next token, N will be the first expr
                 in the stack *)
                 exprN := SOME(hd (!stack));
                 stack := tl(!stack);

                 (* there is no need to flag N *)
                 evaluate_internals(bM, bN, next::rest)
               )
               
               (* if TLetStart is the next token, @ was skipped *)
             | (TLetStart _) => raise SyntaxError

               (* if TIn is not the next token, N will be T (x, y),
               where T is the next token and x and y are the next
               elements on top of the stack *)
             | _ => evaluate_internals(bM, true, next::rest)
        )
      | evaluate_internals (bM, bN, TIn::next::rest) = (
          case next of
               (TLetStart _) => (
                 (* if TLetStart is the next token, M will be the first expr in
                   the stack *)
                 exprM := SOME(hd (!stack));
                 stack := tl(!stack);

                 (* there is no need to flag M *)
                 evaluate_internals(bM, bN, next::rest)
               )

               (* if TLetStart is not the next token, M will be T (x, y)
               where T is the next token and x and y are the next
               elements on top of the stack *)
             | _ => evaluate_internals(true, bN, next::rest)
      )

      | evaluate_internals (bM, bN, (TLetStart var)::rest) = (
          case (!exprM, !exprN) of
               (* if there are both M and N, push the Let clause
               on top of the stack and flush M and N*)
               (SOME(m), SOME(n)) => (
                 stack := (Let (var, m, n))::(!stack);
                 exprM := NONE;
                 exprN := NONE;

                 evaluate_internals(bM, bN, rest)
               )

               (* if either one of the two expressions is NONE,
               a part of the Let clause is missing *)
             | (_, _) => raise SyntaxError
        )

      | evaluate_internals (bM, bN, TAdd::rest) = (
         case !stack of
              y::x::stack_tail => (
                if bN then (
                  exprN := SOME(Add (x, y));
                  stack := tl(tl(!stack));

                  evaluate_internals(bM, false, rest)
                )
                else if bM then (
                  exprM := SOME(Add (x, y));
                  stack := tl(tl(!stack));

                  evaluate_internals(false, bN, rest)
                )
                else (
                  stack := (Add (x, y))::stack_tail;

                  evaluate_internals(bM, bN, rest)
                )
              )
            | _ => raise SyntaxError
        )
      | evaluate_internals (bM, bN, TSub::rest) = (
         case !stack of
              y::x::stack_tail => (
                if bN then (
                  exprN := SOME(Sub (x, y));
                  stack := tl(tl(!stack));

                  evaluate_internals(bM, false, rest)
                )
                else if bM then (
                  exprM := SOME(Sub (x, y));
                  stack := tl(tl(!stack));

                  evaluate_internals(false, bN, rest)
                )
                else (
                  stack := (Sub (x, y))::stack_tail;

                  evaluate_internals(bM, bN, rest)
                )
              )
            | _ => raise SyntaxError
        )
      | evaluate_internals (bM, bN, TMul::rest) = (
         case !stack of
              y::x::stack_tail => (
                if bN then (
                  exprN := SOME(Mul (x, y));
                  stack := tl(tl(!stack));

                  evaluate_internals(bM, false, rest)
                )
                else if bM then (
                  exprM := SOME(Mul (x, y));
                  stack := tl(tl(!stack));

                  evaluate_internals(false, bN, rest)
                )
                else (
                  stack := (Mul (x, y))::stack_tail;

                  evaluate_internals(bM, bN, rest)
                )
              )
            | _ => raise SyntaxError
        )
      | evaluate_internals (bM, bN, TDiv::rest) = (
         case !stack of
              y::x::stack_tail => (
                if bN then (
                  exprN := SOME(Div (x, y));
                  stack := tl(tl(!stack));

                  evaluate_internals(bM, false, rest)
                )
                else if bM then (
                  exprM := SOME(Div (x, y));
                  stack := tl(tl(!stack));

                  evaluate_internals(false, bN, rest)
                )
                else (
                  stack := (Div (x, y))::stack_tail;

                  evaluate_internals(bM, bN, rest)
                )
              )
            | _ => raise SyntaxError
        )
      | evaluate_internals _ = raise UnsupportedToken
  in
    evaluate_internals(false, false, rpn);

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

val output3 = exprify "(12 + var) * (4 - 9)";
val expected3 = Mul (Add (Int 12,Id "var"),Sub (Int 4,Int 9));

val output4 = exprify "3 + 4 * (12 + 5)";
val expected4 = Add (Int 3, Mul (Int 4, Add(Int 12, Int 5)));

val output5 = exprify "7 + (4 * (5 + 6))";
val expected5 = Add (Int 7, Mul (Int 4, Add(Int 5, Int 6)));

val output6 = exprify "1 + 3 * test + 5";
val expected6 = Add (Add (Int 1,Mul (Int 3,Id "test")),Int 5);

val output7 = exprify "5 + (x + (ciao + 5))";
val expected7 = Add (Int 5, Add (Id "x", Add(Id "ciao", Int 5)));

val output8 = exprify "$ x = ($ y = 3 @ 2 + 3 ! + 3) @ x !";
val expected8 = Let ("x",Add (Let ("y",Int 3,Add (Int 2,Int 3)),Int 3),Id "x");

val output9 = exprify "$ x = $ x = 10 @ x + x ! + $ x =  10 @ x + x ! @ x !";
val expected9 = Let ("x", Add (Let("x", Int 10, Add(Id "x", Id "x")), Let("x", Int 10, Add(Id "x", Id "x"))), Id "x");

val output10 = exprify "2 + $ x = 2 * 3 @ x / 3 ! * 4";
val expected10 = Add(Int 2, Mul (Let("x", Mul (Int 2, Int 3), Div (Id "x", Int 3)), Int 4));

val output11 = exprify "$ x = 5 @ x !";
val expected11 = Let("x", Int 5, Id "x");

val output12 = exprify "$ x = 5 @ ($ y = 3 @ x + y!)!";
val expected12 = Let("x", Int 5, Let("y", Int 3, Add( Id "x", Id "y")));

val output13 = exprify "$ x = $ y = 3 @ 2 + 3! @ x!";
val expected13 = Let("x", Let("y", Int 3, Add(Int 2, Int 3)), Id "x");

val check1 = output1 = expected1;
val check2 = output2 = expected2;
val check3 = output3 = expected3;
val check4 = output4 = expected4;
val check5 = output5 = expected5;
val check6 = output6 = expected6;
val check7 = output7 = expected7;
val check8 = output8 = expected8;
val check9 = output9 = expected9;
val check10 = output10 = expected10;
val check11 = output11 = expected11;
val check12 = output12 = expected12;
val check13 = output13 = expected13;

val all_passed = check1 andalso
               check2 andalso
               check3 andalso
               check4 andalso
               check5 andalso
               check6 andalso
               check7 andalso
               check8 andalso
               check9 andalso
               check10 andalso
               check11 andalso
               check12 andalso
               check13;

OS.Process.exit(OS.Process.success);
