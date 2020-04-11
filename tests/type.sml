val _ : (int, int)  option = SOME 10
val _ : int option = SOME 10
val _ : int = 10
val _ : 'a option = NONE
val _ : (int * int) option = NONE
val _ : {foo : int} = { foo = 10 }
type ('a, 'b) t = 'a list
val _ : ('a, 'b) Foo.t = ()
