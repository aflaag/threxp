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

(* fun print_token (TInt x) = "TInt " ^ Int.toString(x) *)
(*   | print_token TAdd = "TAdd" *)
(*   | print_token TSub = "TSub" *)
(*   | print_token TMul = "TMul" *)
(*   | print_token TDiv = "TDiv" *)
(*   | print_token TRParen = "TRParen" *)
(*   | print_token TLParen = "TLParen" *)
(*   | print_token _ = raise UnsupportedToken *)
(*   | print_token TLet = "TLet" *)
(*   | print_token TEqual *)
(*   | print_token TIn *)
(*   | print_token TEnd *)
(**)
(* fun print_tokens ([]) = "\n" *)
(*   | print_tokens (t::rest) = (print_token t) ^ ", " ^ print_tokens(rest) *)

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

fun palle (Int x) = "Int " ^ Int.toString(x)
  | palle (Id v) = "Id " ^ v
  | palle (Add (x, y)) = "Add (" ^ palle(x) ^ ", " ^ palle(y) ^ ")"
  | palle (Sub (x, y)) = "Sub (" ^ palle(x) ^ ", " ^ palle(y) ^ ")"
  | palle (Mul (x, y)) = "Mul (" ^ palle(x) ^ ", " ^ palle(y) ^ ")"
  | palle (Div (x, y)) = "Div (" ^ palle(x) ^ ", " ^ palle(y) ^ ")"
  | palle (Let (x, m, n)) = "Let (" ^ x ^ ", " ^ palle(m) ^ ", " ^ palle(n) ^ ")"

fun cazzo ([]) = "\n"
  | cazzo (s::rest) = " | " ^ palle(s) ^ " | " ^ cazzo(rest)

fun evaluate rpn =
  let
    val stack = ref [];

    val exprM = ref NONE;
    val exprN = ref NONE;

    val buildingM = ref false;
    val buildingN = ref false;

    fun evaluate_internals ([]) = ()

      | evaluate_internals ((TInt x)::rest) = (
          stack := (Int x)::(!stack);
          evaluate_internals(rest)
        )
      | evaluate_internals ((TId v)::rest) = (
          stack := (Id v)::(!stack);
          evaluate_internals(rest)
        )

      | evaluate_internals (TEnd::next::rest) = (
          case next of
               TIn => (
                 exprN := SOME(hd (!stack));
                 stack := tl (!stack)
               )
             | (TLetStart _) => raise SyntaxError

             | _ => buildingN := true;

          evaluate_internals(next::rest)
        )
      | evaluate_internals (TIn::next::rest) = (
          case next of
               (TLetStart _) => (
                 exprM := SOME(hd (!stack));
                 stack := tl (!stack)
               )
             | _ => buildingM := true;

          evaluate_internals(next::rest)
      )

      | evaluate_internals ((TLetStart var)::rest) = (
          case (!exprM, !exprN) of
               (SOME(m), SOME(n)) => (
                 stack := (Let (var, m, n))::(!stack);
                 exprM := NONE;
                 exprN := NONE;

                 evaluate_internals(rest)
               )
             | (_, _) => raise SyntaxError
        )

      | evaluate_internals (TAdd::rest) = (
         case !stack of
              y::x::stack_tail => (
                if !buildingN then (exprN := SOME(Add (x, y)); buildingN := false; stack := tl ( tl(!stack)))
                else if !buildingM then (exprM := SOME(Add (x, y)); buildingM := false; stack := tl (tl (!stack)))
                else stack := (Add (x, y))::stack_tail;

                evaluate_internals(rest)
              )
            | _ => raise SyntaxError
        )
      | evaluate_internals (TSub::rest) = (
         case !stack of
              y::x::stack_tail => (
                if !buildingN then (exprN := SOME(Sub (x, y)); buildingN := false; stack := tl ( tl(!stack)))
                else if !buildingM then (exprM := SOME(Sub (x, y)); buildingM := false; stack := tl (tl (!stack)))
                else stack := (Sub (x, y))::stack_tail;

                evaluate_internals(rest)
              )
            | _ => raise SyntaxError
        )
      | evaluate_internals (TMul::rest) = (
         case !stack of
              y::x::stack_tail => (
                if !buildingN then (exprN := SOME(Mul (x, y)); buildingN := false; stack := tl ( tl(!stack)))
                else if !buildingM then (exprM := SOME(Mul (x, y)); buildingM := false; stack := tl (tl (!stack)))
                else stack := (Mul (x, y))::stack_tail;

                evaluate_internals(rest)
              )
            | _ => raise SyntaxError
        )
      | evaluate_internals (TDiv::rest) = (
         case !stack of
              y::x::stack_tail => (
                if !buildingN then (exprN := SOME(Div (x, y)); buildingN := false; stack := tl ( tl(!stack)))
                else if !buildingM then (exprM := SOME(Div (x, y)); buildingM := false; stack := tl (tl (!stack)))
                else stack := (Div (x, y))::stack_tail;

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

val output3 = exprify "(12 + var) * (4 - 9)";
val expected3 = Mul (Add (Int 12,Id "var"),Sub (Int 4,Int 9));

val output4 = exprify "3 + 4 * (12 + 5)";
val expected4 = Add (Int 3, Mul (Int 4, Add(Int 12, Int 5)));

val output5 = exprify "7 + (4 * (5 + 6))";
val expected5 = Add (Int 7, Mul (Int 4, Add(Int 5, Int 6)));

val output6 = exprify "1 + 3 * test + 5";
val expected6 = Add (Add (Int 1,Mul (Int 3,Id "test")),Int 5)

val output7 = exprify "5 + (x + (ciao + 5))";
val expected7 = Add (Int 5, Add (Id "x", Add(Id "ciao", Int 5)))

val output8 = exprify "$ x = ($ y = 3 @ 2 + 3 ! + 3) @ x !";
val expected8 = Let ("x",Add (Let ("y",Int 3,Add (Int 2,Int 3)),Int 3),Id "x")

val output9 = exprify "$ x = $ x = 10 @ x + x ! + $ x =  10 @ x + x ! @ x !";
val expected9 = Let ("x", Add (Let("x", Int 10, Add(Id "x", Id "x")), Let("x", Int 10, Add(Id "x", Id "x"))), Id "x");

val check1 = output1 = expected1;
val check2 = output2 = expected2;
val check3 = output3 = expected3;
val check4 = output4 = expected4;
val check5 = output5 = expected5;
val check6 = output6 = expected6;
val check7 = output7 = expected7;
val check8 = output8 = expected8;
val check9 = output9 = expected9;

(* exprify "$ x = 5 @ x + 3 !"; *)

(*let x = 5 in (let y = 3 in x + y)*)

(* let x = (let y = 3 in 2 + 3 end) in x end *)

OS.Process.exit(OS.Process.success);
