signature FORMAT = sig
  type formatInfo = { indent : int }
  val formatDec : formatInfo -> ElabAst.dec -> string
end
