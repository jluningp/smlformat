structure IntKey = struct
  type ord_key = int
  val compare = Int.compare
end

structure StringKey = struct
  type ord_key = string
  val compare = String.compare
end

structure IntSet = SplaySetFn (IntKey)
structure IntMap = SplayMapFn (IntKey)
structure StringMap = SplayMapFn (StringKey)
