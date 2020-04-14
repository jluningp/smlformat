signature FIXITY_PARSER = sig
  datatype 'a fixexp =
        InfixApp of 'a fixexp * 'a * 'a fixexp
      | FlatApp of 'a list

  val map : ('a -> 'b) -> 'a fixexp -> 'b fixexp
  val parse : Ast.exp Ast.fixitem list -> Ast.exp Ast.fixitem fixexp
end
