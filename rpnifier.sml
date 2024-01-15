(* this function returns the precedence of every token but ")"
since it's not necessary due to how the algorithm works *)
fun precedence TLParen = ~1
  | precedence (TLetStart _) = ~1
  | precedence TIn = ~1
  | precedence TEnd = ~1
  | precedence TAdd = 0
  | precedence TSub = 0
  | precedence TMul = 1
  | precedence TDiv = 1
  | precedence _ = raise UnsupportedToken

(* this algorithm is an implementation of the shunting-yard algorithm by
dijkstra, which given a token list as input, it return the list in RPN, or
postfix notation, by using a stack to handle precedence; the stack is
passed as argument bewteen recursive function calls *)
fun rpnify tokens =
  let
    fun rpnify_internals ([], []) = []

      (* if the token list has ended, and there are still tokens
      inside the stack, dump every token inside the output list*)
      | rpnify_internals (token::stack, []) = token::rpnify_internals(stack, [])

      (* if the next token is "@", "!", "(" or "$ x =", push them inside the stack *)
      | rpnify_internals (stack, TIn::rest) = rpnify_internals(TIn::stack, rest)
      | rpnify_internals (stack, TEnd::rest) = rpnify_internals(TEnd::stack, rest)
      | rpnify_internals (stack, TLParen::rest) = rpnify_internals(TLParen::stack, rest)
      | rpnify_internals (stack, (TLetStart v)::rest) = rpnify_internals((TLetStart v)::stack, rest)

      (* if the next token is a ")" dump everything there is on top of the stack
      until a "(" is found (if any) *)
      | rpnify_internals ([], TRParen::rest) = raise SyntaxError
      | rpnify_internals (stack_top::stack, TRParen::rest) =
          if stack_top = TLParen
          then rpnify_internals(stack, rest)

          (* to dump the entire stack, the ")" token is put again as the next
          token until a "(" is found *)
          else stack_top::rpnify_internals(stack, TRParen::rest)

        (* if there are either integers or variables, put them inside
        the output list directly *)
      | rpnify_internals (stack, (TInt x)::rest) = (TInt x)::rpnify_internals(stack, rest)
      | rpnify_internals (stack, (TId v)::rest) = (TId v)::rpnify_internals(stack, rest)
      
        (* if the next token is an operator, and the operator stack is empty,
        pusth the operator on top of the stack; otherwise, pop out of the stack
        every token that has precedence greater or equal than the precedence of
        the next operator, and then put the next operator itself on top of the stack *)
      | rpnify_internals ([], operator::rest) = rpnify_internals([operator], rest)
      | rpnify_internals (stack_top::stack, operator::rest) =
          if precedence stack_top >= precedence operator

          (* to pop out of the stack every token that has precedence greater or
          equal than the precedence of the next operator, the next operator is
          put agains as the next token until an operator with greater precedence
          is found *)
          then stack_top::rpnify_internals(stack, operator::rest)
          else rpnify_internals(operator::stack_top::stack, rest)
  in
    rpnify_internals([], tokens)
  end
