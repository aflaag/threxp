open SMLofNJ.Cont;
open String;
open Queue;

val counter = ref 0;

signature THREADS =
  sig 
    exception NoRunnableThreads

    val spawn : (unit -> unit) * string -> unit
    val dispatch : unit -> 'a
    val yield : string -> unit
    val delete_sons_from_queue : string -> unit
  end;

structure T :> THREADS =
  struct
    exception NoRunnableThreads

    type thread = unit cont

    val readyQueue: (thread * string) Queue.queue = Queue.mkQueue ();

    (* takes the first thread from the ready queue and runs it *)
    fun dispatch () =
      let 
        val t = Queue.dequeue readyQueue
          handle Queue.Dequeue => raise NoRunnableThreads
      in 
        throw (#1 t) ()
      end

    (* puts the current continuation inside the ready queue and TODO BOH *)
    fun spawn (f, thread_id) = callcc (
      fn parent: thread => (
        Queue.enqueue (readyQueue, (parent, thread_id));
        f ();
        dispatch () (* ends with dispatch to keep the execution going *)
      )
    )

    (* puts the current continuation inside the ready queue and calls the dispatcher *)
    fun yield (thread_id) = callcc (
      fn parent: thread => (
        Queue.enqueue (readyQueue, (parent, thread_id));
        dispatch () (* ends with dispatch to keep the execution going *)
      )
    )
    
    fun delete_sons_from_queue (thread_id : string) =
      let
        val _ = counter := Queue.length (readyQueue)
        val _ = while !counter <> 0 do (
                  let 
                    val t = Queue.dequeue (readyQueue)
                  in 
                    (* delete the threads that are under the actual thread *)
                    if (String.isPrefix thread_id (#2 t)) then (
                      counter := !counter - 1
                    ) else (
                      Queue.enqueue (readyQueue, t); 
                      counter := !counter - 1
                    )
                  end
                )
      in
        ()
      end
  end;