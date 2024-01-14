use "tokenizer.sml";
use "rpnifier.sml";
use "evaluator.sml";

fun exprify code = evaluate(rpnify(tokenize(code)))
