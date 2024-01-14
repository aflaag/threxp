use "threads.sml";

datatype expr = Int of int
             | Id of string
             | Add of expr * expr
             | Sub of expr * expr
             | Mul of expr * expr
             | Div of expr * expr
             | Let of string * expr * expr
             | Print of expr;

val threads_result : (string * int option) list ref = ref [];

fun find_association (id, []) = raise Fail ("Variable not found: " ^ id)
  | find_association (id, (key, value)::rest) =
    if id = key then value
    else find_association (id, rest)

fun delete_element(thread_id: string) =
  let
    val updated_list = List.filter (fn (key, _) => key <> thread_id) (!threads_result )
  in
    threads_result := updated_list
  end;

fun wait(thread_id) = (
  T.yield ();
  case find_association(thread_id, !threads_result ) of SOME x => x | NONE => wait(thread_id)
  ) handle Fail _ => (0-1)

fun start_eval (thread_id, env, Let (id, e1, e2)) =
      let
        val thread_id1 = thread_id ^ "0"
        val thread_id2 = thread_id ^ "1"
        val _ = threads_result := (thread_id1, NONE) :: (thread_id2, NONE) :: !threads_result 
        val _ = T.spawn(fn () => start_eval (thread_id1, env, e1))
        val _ = T.spawn(fn () => start_eval (thread_id2, (id, thread_id1) :: env, e2))
        val v = wait(thread_id2)
      in
        delete_element(thread_id);
        threads_result := (thread_id, SOME v) :: !threads_result ;
        delete_element(thread_id1);
        if thread_id = "" then (T.empty_readyQueue(); delete_element(thread_id2))
        else (delete_element(thread_id2); T.dispatch())
      end
  | start_eval (thread_id, env, e) =
      let
        val v = eval (thread_id, env, e)
      in
        delete_element(thread_id);
        if thread_id = "" then (T.empty_readyQueue(); threads_result := (thread_id, SOME v) :: !threads_result)
        else (threads_result := (thread_id, SOME v) :: !threads_result)
      end

and wait_eval (thread_id, env, e) = (
  T.yield (); 
  eval (thread_id, env, e))

and eval (thread_id, env, Int n) = n
  | eval (thread_id, env, Id id) = (case find_association(find_association (id, env), !threads_result ) of SOME x => x | NONE =>  wait(find_association (id, env)))
  | eval (thread_id, env, Add (e1, e2)) = wait_eval (thread_id, env, e1) + wait_eval (thread_id, env, e2)
  | eval (thread_id, env, Sub (e1, e2)) = wait_eval (thread_id, env, e1) - wait_eval (thread_id, env, e2)
  | eval (thread_id, env, Mul (e1, e2)) = wait_eval (thread_id, env, e1) * wait_eval (thread_id, env, e2)
  | eval (thread_id, env, Div (e1, e2)) = wait_eval (thread_id, env, e1) div wait_eval (thread_id, env, e2)
  | eval (thread_id, env, Let (id, e1, e2)) =
      let
        val thread_id1 = thread_id ^ "0"
        val thread_id2 = thread_id ^ "1"
        val _ = threads_result := (thread_id1, NONE) :: (thread_id2, NONE) :: !threads_result 
        val _ = T.spawn(fn () => start_eval (thread_id1, env, e1))
        val _ = T.spawn(fn () => start_eval (thread_id2, (id, thread_id1) :: env, e2))
        val v = wait(thread_id2)
      in
        wait(thread_id2);
        delete_element(thread_id1);
        delete_element(thread_id2);
        v
      end
  | eval (thread_id, env, Print e) =
      let
        val v = wait_eval (thread_id, env, e)
      in
        print (Int.toString v ^ "\n");
        v
      end;

fun run program = (
    let
        val _ = threads_result := [("", NONE)];
        val _ = start_eval ("", [], program);
    in
        case find_association("", !threads_result ) of SOME x => x | NONE => 0-1
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

val program5 = Let ("x", Add (Let("x", Int 10, Add(Id "x", Id "x")), Let("x", Int 10, Add(Id "x", Id "x"))), Id "x");
val result5 = run program5;
print ("Result 5: " ^ Int.toString result5 ^ " = 2\n");

val program6 = Let ("x", Let ("x", Add (Let("x", Int 10, Add(Id "x", Id "x")), Let("x", Int 10, Add(Id "x", Id "x"))), Id "x"), Int 2);
val result6 = run program6;
print ("Result 6: " ^ Int.toString result6 ^ " = 2\n");