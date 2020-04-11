signature ADD_COMMENTS = sig
  type conversionInfo = {sourceMap : SourceMap.sourcemap, comments : (int * string list) list ref}
  val convertDec : (conversionInfo -> (Ast.dec -> CommentedAst.dec))
end
