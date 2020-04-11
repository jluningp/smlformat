structure IntKey = struct
  type ord_key = int
  val compare = Int.compare
end

structure IntSet = SplaySetFn (IntKey)
structure IntMap = SplayMapFn (IntKey)
