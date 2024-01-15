use "expr.sml";

(* this is a helper function used to map operator tokens into 
operator inside expr *)
fun map_operator (TAdd, x, y) = Add (x, y)
  | map_operator (TSub, x, y) = Sub (x, y)
  | map_operator (TMul, x, y) = Mul (x, y)
  | map_operator (TDiv, x, y) = Div (x, y)
  | map_operator _ = raise UnsupportedToken

(* this function evaluates a token list in RPN *)
fun evaluate rpn =
  let
    (* this stack will contain the output value *)
    val stack = ref [];

    fun evaluate_internals (_, _, _, _, []) = ()

        (* push integers and variables inside the stack *)
      | evaluate_internals (bM, bN, exprM, exprN, (TInt x)::rest) = (
          stack := (Int x)::(!stack);
          evaluate_internals(bM, bN, exprM, exprN, rest)
        )
      | evaluate_internals (bM, bN, exprM, exprN, (TId v)::rest) = (
          stack := (Id v)::(!stack);
          evaluate_internals(bM, bN, exprM, exprN, rest)
        )

      | evaluate_internals (bM, bN, NONE, NONE, TEnd::next::rest) = (
          case next of
               TIn => (
                 let
                   (* if TIn is the next token, N will be the first expr
                   in the stack *)
                   val exprN = SOME(hd(!stack));
                 in
                   stack := tl(!stack);

                   (* there is no need to flag N *)
                   evaluate_internals(bM, bN, NONE, exprN, next::rest)
                 end
               )
               
               (* if TLetStart is the next token, @ was skipped *)
             | (TLetStart _) => raise SyntaxError

               (* if TIn is not the next token, N will be T (x, y),
               where T is the next token and x and y are the next
               elements on top of the stack *)
             | _ => evaluate_internals(bM, true, NONE, NONE, next::rest)
        )
      | evaluate_internals (bM, bN, NONE, exprN, TIn::next::rest) = (
          case next of
               (TLetStart _) => (
                 let
                   (* if TLetStart is the next token, M will be the first expr in
                   the stack *)
                   val exprM = SOME(hd(!stack));
                 in
                   stack := tl(!stack);

                   (* there is no need to flag M *)
                   evaluate_internals(bM, bN, exprM, exprN, next::rest)
                 end
               )

               (* if TLetStart is not the next token, M will be T (x, y)
               where T is the next token and x and y are the next
               elements on top of the stack *)
             | _ => evaluate_internals(true, bN, NONE, exprN, next::rest)
      )

      | evaluate_internals (bM, bN, exprM, exprN, (TLetStart var)::rest) = (
          case (exprM, exprN) of
               (* if there are both M and N, push the Let clause
               on top of the stack and flush M and N*)
               (SOME(m), SOME(n)) => (
                 stack := (Let (var, m, n))::(!stack);

                 evaluate_internals(bM, bN, NONE, NONE, rest)
               )

               (* if either one of the two expressions is NONE,
               a part of the Let clause is missing *)
             | (_, _) => raise SyntaxError
        )

      | evaluate_internals (bM, bN, exprM, exprN, operator::rest) = (
          case !stack of
               y::x::stack_tail => (
                 (* if either bM or bN is true, then either M or N is waiting to
                 be built, and the operands of the operator will be the first
                 two elements on top of the stack *)
                 if bN then (
                   stack := tl(tl(!stack));

                   evaluate_internals(bM, false, exprM, SOME(map_operator(operator, x, y)), rest)
                 )
                 else if bM then (
                   stack := tl(tl(!stack));

                   evaluate_internals(false, bN, SOME(map_operator(operator, x, y)), exprN, rest)
                 )

                 (* otherwise, replace the top of the stack with the two
                 operands combined with the given operator *)
                 else (
                   stack := (map_operator(operator, x, y))::stack_tail;

                   evaluate_internals(bM, bN, exprM, exprN, rest)
                 )
               )
             | _ => raise SyntaxError
        )
  in
    evaluate_internals(false, false, NONE, NONE, rpn);

    case !stack of
         result::[] => result
       | x::rest => raise SyntaxError
       | [] => raise EmptyProgram
  end
