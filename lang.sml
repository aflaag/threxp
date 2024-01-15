use "threads.sml";

(* global list that save the result of all the active threads that can be NONE*)
val threads_result : (string * int option) list ref = ref [];

fun find_association (id, []) =
    raise Fail ("Variable not found: " ^ id)
  | find_association (id, (key, value)::rest) =
    if id = key then 
      value
    else 
      find_association (id, rest)

(* delete the thread_id and sons result from the global list *)
fun delete_element(thread_id: string) =
  threads_result := List.filter (fn (key, _) => (not (String.isPrefix thread_id key))) (!threads_result)

(* waiting for the thread with the thread_id *)
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
      val _ = threads_result := (thread_id1, NONE) :: (thread_id2, NONE) :: !threads_result
      val _ = T.spawn (fn () => start_eval (thread_id1, env, e1), thread_id)
      val _ = T.spawn (fn () => start_eval (thread_id2, (id, thread_id1) :: env, e2), thread_id)
      (* we know that (Let x = M in N) = N[x/M] so we are waiting the N result *)
      val v = wait(thread_id, thread_id2)
    in
      (* delete the current result wich is NONE and his sons*)
      delete_element(thread_id);

      threads_result := (thread_id, SOME v) :: !threads_result;

      (* delete from the threads queue the sons of the actual thread *)
      T.delete_sons_from_queue(thread_id)
    end

  | start_eval (thread_id, env, e) =
    let
      val v = eval (thread_id, env, e)
    in
      delete_element(thread_id);
      threads_result := (thread_id, SOME v) :: !threads_result
    end

(* waiting turn, in this way we apply concurrency *)
and wait_eval (thread_id, env, e) =
  (T.yield thread_id;
   eval (thread_id, env, e))

and eval (thread_id, env, Int n) = n
  | eval (thread_id, env, Id id) = (
      case find_association (find_association (id, env), !threads_result) of
       SOME x => x
      | NONE => wait(thread_id, find_association (id, env))
    )
  | eval (thread_id, env, Add (e1, e2)) = wait_eval (thread_id, env, e1) + wait_eval (thread_id, env, e2)
  | eval (thread_id, env, Sub (e1, e2)) = wait_eval (thread_id, env, e1) - wait_eval (thread_id, env, e2)
  | eval (thread_id, env, Mul (e1, e2)) = wait_eval (thread_id, env, e1) * wait_eval (thread_id, env, e2)
  | eval (thread_id, env, Div (e1, e2)) = wait_eval (thread_id, env, e1) div wait_eval (thread_id, env, e2)
  | eval (thread_id, env, Let (id, e1, e2)) =
    let
      val thread_id1 = thread_id ^ "0"
      val thread_id2 = thread_id ^ "1"
      val _ = threads_result := (thread_id1, NONE) :: (thread_id2, NONE) :: !threads_result
      val _ = T.spawn (fn () => start_eval (thread_id1, env, e1), thread_id)
      val _ = T.spawn (fn () => start_eval (thread_id2, (id, thread_id1) :: env, e2), thread_id)
      val v = wait(thread_id, thread_id2)
    in
      (* delete the sons's results of the actual thread *)
      delete_element(thread_id1);
      delete_element(thread_id2);

      (* delete the sons thread of the actual thread *)
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