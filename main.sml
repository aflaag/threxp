use "exprifier.sml";
use "lang.sml";

fun threxp program = run(exprify program);

OS.Process.exit(OS.Process.success);
