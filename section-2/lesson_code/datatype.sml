datatype test = a of int * int
       | b of string * string
       | test

datatype test2 = test
              | c of string * int

val mm = test


fun a (x) =
  case x
   of test => 9
   | test2 => 10

val cc = a mm
