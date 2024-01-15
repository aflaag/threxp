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
