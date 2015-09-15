(*factorial function*)
let rec fact n = if n > 0 then n*fact(n-1) else 1

(*fibonacci seq*)
let rec fib n = if n > 1 then fib(n-1)+fib(n-2) else if n = 1 then 1 else 0

(*polymorphic functions*)
let f x = if x then x else x
  (*this has type bool -> bool*)

let g x y = if y then x else x
  (*this has type 'a -> bool -> 'a where 'a is a type variable (stands for unknown type)*)

let h x y z = if x then y else z
  (*this has type bool -> 'a -> 'a -> 'a*)

let i x y z = if x then y else y
  (*has type bool -> 'a -> 'b -> 'a*)

(*partial application*)
let add1 x = x + 1
  (*can also be written as let add1 = fun x -> x + 1*)

(*function that takes symantically the same as let addn x y = x+y*)
let addn n : int -> int = fun x -> x + n

(*Operators: defining our own or using an old one (ie: (+) 2 3)*)
let (^^) x y = max x y

(*$$ to compute average of 2*)
let ($$) x y = (x+.y)/.2.0

(*Higher Order Functions*)
let double x = 2*x
let square x = x*x

let quad x = double (double x)
let fourth x = square (square x)

(*-> now make these higher order functions*)
let twice f x = f (f x)   (*twice : 'a -> 'a -> 'a -> 'a = <fun>*)
let quad x = twice double x
let fourth x = twice fourth x

let add x y = x+y
(*twice twice (add 10) gives us a func, if twice twice (add 10) 1 = 41*)

(*n_times*)
let rec n_times f n x = if n > 1 then f (n_times f (n-1) x) else if n = 1 then f x else x

(*Labeling arguments*)
let f ~name1:arg1 ~name2:arg2 = arg1 + arg2
  (*by typing f you will get the names of the arguments*)
  (*can be called by passing argument in any order (ie: f name2:4 name1:2)*)
  (*short hand: let f ~name1 ~name2 = name1 + name2*)

(*Optional arguments*)
let h ?name1:(arg1=8) ~name2:arg2 = arg1 + arg2
  (* can do these two things:
        h ~name1:2 ~name2:7;;
        h 7;;
  *)

(*Reverse an integer: ex 1234 -> 4321*)
let rec rev_int n = if n >= 10 then
                      (n mod 10) * int_of_float(10.0**(floor (log10 (float(n))))) + rev_int(n/10)
                    else
                      if n < 0 then (-1)*rev_int (-n) else n

let rec rev_base ?base:(b=10) n = if n >= b then
                      (n mod b) * int_of_float((float(b))**(floor (log10 (float(n))))) + rev_int(n/b)
                    else
                      if n < 0 then (-1)*rev_int (-n) else n
