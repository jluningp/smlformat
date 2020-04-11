functor CurriedPair (M : sig
  val x : int
end) = struct
  functor Pair (N : sig
    val y : int
  end) = struct
    val e = (M.x, N.y)
  end
end

structure P = CurriedPair (struct
  val x = 10
end)

signature F = sig
  type t
end

functor Foo
  (structure M : F
   structure N : F) = struct end

signature Foooo = sig end
structure Fo = struct
  type t = int
end

signature F = sig
  type 'a t
  type s
end

signature E = F where type 'a t = int list and s = bool
signature S = sig
  type t
  type s
  type i
  sharing type t = s = i
end
