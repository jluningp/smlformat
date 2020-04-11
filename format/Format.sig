signature FORMAT = sig
  type formatInfo = {indent : int}

  val formatDec : formatInfo -> CommentedAst.dec -> string
end
