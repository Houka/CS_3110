let m = Mutex.create ()
let printn n =
       print_string "Hello from thread ";print_int n; print_newline()

let t1 = Thread.create printn 1
let t2 = Thread.create printn 2
let t3 = Thread.create printn 3
let _ = (Thread.join t1, Thread.join t2, Thread.join t3)
