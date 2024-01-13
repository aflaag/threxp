use "threads.sml";

datatype expr = Int of int
             | Id of string
             | Add of expr * expr
             | Sub of expr * expr
             | Mul of expr * expr
             | Div of expr * expr
             | Let of string * expr * expr
             | Print of expr;

fun printStringIntOptionPairsRef refList =
  let
    fun printPair (str, opt) =
      case opt of
        SOME value => print ("(" ^ str ^ ", " ^ Int.toString value ^ ") ")
      | NONE => print ("(" ^ str ^ ", NONE) ");
    val pairList = !refList;
  in
    List.app printPair pairList;
    print "\n"
  end;

val res : (string * int option) list ref = ref [];

fun find_association (id, []) = raise Fail ("Variable not found: " ^ id)
  | find_association (id, (key, value)::rest) =
    if id = key then value
    else ( printStringIntOptionPairsRef res;find_association (id, rest))

fun delete_element(tid: string) =
  let
    val updated_list = List.filter (fn (key, _) => key <> tid) (!res)
  in
    res := updated_list
  end;

fun wait(trid) = (
  T.yield ();
  print "trying\n";
  printStringIntOptionPairsRef res;
  print (trid ^ "\n");
  case find_association(trid, !res) of SOME x => x | NONE => wait(trid)
  ) handle Fail _ => wait(trid)

fun start_eval (tid, env, Let (id, e1, e2)) =
      let
        val tid1 = tid ^ "0"
        val tid2 = tid ^ "1"
        val _ = res := (tid1, NONE) :: (tid2, NONE) :: !res
        val _ = T.spawn(fn () => start_eval (tid1, env, e1))
        val _ = T.spawn(fn () => start_eval (tid2, (id, tid1) :: env, e2))
        val v = wait(tid2)
      in
        wait(tid2);
        print ("bebore deleting " ^ tid ^ " ");
        printStringIntOptionPairsRef res;
        delete_element(tid);
        res := (tid, SOME v) :: !res;
        delete_element(tid1);
        delete_element(tid2);
        print ("after deleting " ^ tid ^ " ");
        printStringIntOptionPairsRef res;
        (T.dispatch () handle T.NoRunnableThreads => ())
      end
  | start_eval (tid, env, e) =
      let
        val _ = print "new thread entered\n"
        val v = eval (tid, env, e)
      in
        delete_element(tid);
        res := (tid, SOME v) :: !res;
        printStringIntOptionPairsRef res;
        (T.dispatch () handle T.NoRunnableThreads => ())
      end

and wait_eval (tid, env, e) = (T.yield (); eval (tid, env, e))

and eval (tid, env, Int n) = n
  | eval (tid, env, Id id) = (case find_association(find_association (id, env), !res) of SOME x => x | NONE =>  wait(find_association (id, env)))
  | eval (tid, env, Add (e1, e2)) = wait_eval (tid, env, e1) + wait_eval (tid, env, e2)
  | eval (tid, env, Sub (e1, e2)) = wait_eval (tid, env, e1) - wait_eval (tid, env, e2)
  | eval (tid, env, Mul (e1, e2)) = wait_eval (tid, env, e1) * wait_eval (tid, env, e2)
  | eval (tid, env, Div (e1, e2)) = wait_eval (tid, env, e1) div wait_eval (tid, env, e2)
  | eval (tid, env, Let (id, e1, e2)) =
      let
        val tid1 = tid ^ "0"
        val tid2 = tid ^ "1"
        val _ = res := (tid1, NONE) :: (tid2, NONE) :: !res
        val _ = T.spawn(fn () => start_eval (tid1, env, e1))
        val _ = T.spawn(fn () => start_eval (tid2, (id, tid1) :: env, e2))
        val v = wait(tid2)
      in
        wait(tid2);
        print ("bebore deleting " ^ tid ^ " ");
        printStringIntOptionPairsRef res;
        delete_element(tid1);
        delete_element(tid2);
        print ("after deleting " ^ tid ^ " ");
        printStringIntOptionPairsRef res;
        res := (tid, SOME v) :: !res;
        v
      end
  | eval (tid, env, Print e) =
      let
        val v = wait_eval (tid, env, e)
      in
        print (Int.toString v ^ "\n");
        v
      end;

fun run program = (
    let
        val _ = res := [("", NONE)];
        val _ = print "boh\n"
        val _ = start_eval ("", [], program);
    in
        case find_association("", !res) of SOME x => x | NONE => 0-1
    end
) 

val program1 = Add (Int 1, Mul (Int 2, Int 3));
val result1 = run program1;
print ("Result 1: " ^ Int.toString result1 ^ " = 7\n");

val program2 = Let ("x", Int 5, Add (Id "x", Id "x"));
val result2 = run program2;
print ("Result 2: " ^ Int.toString result2 ^ " = 10\n");

val program3 = Let ("x", Int 5, Add (Let ("x", Int 6, Int 2), Id "x"));
val result3 = run program3;
print ("Result 3: " ^ Int.toString result3 ^ " = 7\n");

val program4 = Let ("x", Int 5, Let ("y", Id "x", Let ("x", Int 4, Id "y")));
val result4 = run program4;
print ("Result 4: " ^ Int.toString result4 ^ " = 5\n");
