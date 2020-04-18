signature ELAB = sig
  type conversionInfo =
    { sourceMap : SourceMap.sourcemap
    , comments : (int * string list) list ref
    , fixity : Fixity.fixity StringMap.map ref }

  val elaborate : conversionInfo -> Ast.dec -> ElabAst.dec
end
