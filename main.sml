use "exprifier.sml";
use "lang.sml";

fun threxp program = run(exprify program);

val p_example = threxp "2 + $ x = 2 * 3 @ x / 3 ! * 4 ";

OS.Process.exit(OS.Process.success);
