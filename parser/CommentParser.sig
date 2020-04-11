signature COMMENT_PARSER = sig
  val parse : string -> (string * int) list IntMap.map
end
