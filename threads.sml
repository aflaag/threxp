open SMLofNJ.Cont;
open Queue;

signature THREADS =
  sig 
    exception NoRunnableThreads

    val spawn : (unit -> unit) -> unit
    val dispatch : unit -> 'a
    val yield : unit -> unit
    val empty_readyQueue : unit -> unit
  end;

structure T :> THREADS =
  struct
    exception NoRunnableThreads

    type thread = unit cont

    val readyQueue: thread Queue.queue = Queue.mkQueue ();

    (* takes the first thread from the ready queue and runs it *)
    fun dispatch () =
      let 
        val t = Queue.dequeue readyQueue
          handle Queue.Dequeue => raise NoRunnableThreads
      in 
        throw t ()
      end

    (* puts the current continuation inside the ready queue and TODO BOH *)
    fun spawn f = callcc (
      fn parent: thread => (
        Queue.enqueue (readyQueue, parent);
        f ();
        dispatch () (* ends with dispatch to keep the execution going *)
      )
    )

    (* puts the current continuation inside the ready queue and calls the dispatcher *)
    fun yield () = callcc (
      fn parent: thread => (
        Queue.enqueue (readyQueue, parent);
        dispatch () (* ends with dispatch to keep the execution going *)
      )
    )

    (* empties the readyQueue *)
    fun empty_readyQueue () =
      let
        val _ = while not (Queue.isEmpty readyQueue) do
                    Queue.dequeue readyQueue
      in
        ()
      end

  end;