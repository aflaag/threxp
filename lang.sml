use "threads.sml";

val threads_result : (string * int option) list ref = ref [];

fun find_association (id, []) =
    raise Fail ("Variable not found: " ^ id)
  | find_association (id, (key, value)::rest) =
    if id = key then value
    else find_association (id, rest)

fun delete_element(thread_id: string) =
  threads_result := List.filter (fn (key, _) => key <> thread_id) (!threads_result)

fun wait(waiting_thread, thread_id) = (
  T.yield waiting_thread;
  case find_association(thread_id, !threads_result) of
   SOME x => x
  | NONE => wait(waiting_thread, thread_id))
  handle Fail _ => (0 - 1)

fun start_eval (thread_id, env, Let (id, e1, e2)) =
    let
      val thread_id1 = thread_id ^ "0"
      val thread_id2 = thread_id ^ "1"
      val () = threads_result := (thread_id1, NONE) :: (thread_id2, NONE) :: !threads_result
      val () = T.spawn (fn () => start_eval (thread_id1, env, e1), thread_id)
      val () = T.spawn (fn () => start_eval (thread_id2, (id, thread_id1) :: env, e2), thread_id)
      val v = wait(thread_id, thread_id2)
    in
      delete_element(thread_id);
      threads_result := (thread_id, SOME v) :: !threads_result;
      delete_element(thread_id1);
      if thread_id = "" then (
        T.delete_sons_from_queue(thread_id);
        delete_element(thread_id2)
      ) else (
        delete_element(thread_id2)
      )
    end
  | start_eval (thread_id, env, e) =
    let
      val v = eval (thread_id, env, e)
    in
      delete_element(thread_id);
      threads_result := (thread_id, SOME v) :: !threads_result
    end

and wait_eval (thread_id, env, e) =
  (T.yield thread_id;
   eval (thread_id, env, e))

and eval (thread_id, env, Int n) = n
  | eval (thread_id, env, Id id) =
    (case find_association (find_association (id, env), !threads_result) of
       SOME x => x
     | NONE => wait(thread_id, find_association (id, env)))
  | eval (thread_id, env, Add (e1, e2)) = wait_eval (thread_id, env, e1) + wait_eval (thread_id, env, e2)
  | eval (thread_id, env, Sub (e1, e2)) = wait_eval (thread_id, env, e1) - wait_eval (thread_id, env, e2)
  | eval (thread_id, env, Mul (e1, e2)) = wait_eval (thread_id, env, e1) * wait_eval (thread_id, env, e2)
  | eval (thread_id, env, Div (e1, e2)) = wait_eval (thread_id, env, e1) div wait_eval (thread_id, env, e2)
  | eval (thread_id, env, Let (id, e1, e2)) =
    let
      val thread_id1 = thread_id ^ "0"
      val thread_id2 = thread_id ^ "1"
      val () = threads_result := (thread_id1, NONE) :: (thread_id2, NONE) :: !threads_result
      val () = T.spawn (fn () => start_eval (thread_id1, env, e1), thread_id)
      val () = T.spawn (fn () => start_eval (thread_id2, (id, thread_id1) :: env, e2), thread_id)
      val v = wait(thread_id, thread_id2)
    in
      wait(thread_id, thread_id2);
      delete_element(thread_id1);
      delete_element(thread_id2);
      T.delete_sons_from_queue(thread_id);
      v
    end

fun run program =
    let
      val () = threads_result := [("", NONE)]
      val () = start_eval ("", [], program)
    in
      case find_association("", !threads_result) of
        SOME x => x
      | NONE => 0 - 1
    end

val program1 = Add (Int 1, Mul (Int 2, Int 3))
val result1 = run program1
val () = print ("Result 1: " ^ Int.toString result1 ^ " = 7\n")

val program2 = Let ("x", Int 5, Add (Id "x", Id "x"))
val result2 = run program2
val () = print ("Result 2: " ^ Int.toString result2 ^ " = 10\n")

val program3 = Let ("x", Int 5, Add (Let ("x", Int 6, Int 2), Id "x"))
val result3 = run program3
val () = print ("Result 3: " ^ Int.toString result3 ^ " = 7\n")

val program4 = Let ("x", Int 5, Let ("y", Id "x", Let ("x", Int 4, Id "y")))
val result4 = run program4
val () = print ("Result 4: " ^ Int.toString result4 ^ " = 5\n")

val program5 = Let ("x", Add (Let("x", Int 10, Add(Id "x", Id "x")), Let("x", Int 10, Add(Id "x", Id "x"))), Id "x")
val result5 = run program5
val () = print ("Result 5: " ^ Int.toString result5 ^ " = 40\n")

val program6 = Let ("x", Let ("x", Add (Let("x", Int 10, Add(Id "x", Id "x")), Let("x", Int 10, Add(Id "x", Id "x"))), Id "x"), Int 2)
val result6 = run program6
val () = print ("Result 6: " ^ Int.toString result6 ^ " = 2\n");
