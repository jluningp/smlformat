structure FixityParser : FIXITY_PARSER = struct
  open Ast

  datatype 'a fixexp =
        InfixApp of 'a fixexp * 'a * 'a fixexp
      | FlatApp of 'a list

  fun map f (InfixApp (l, x, r)) = InfixApp (map f l, f x, map f r)
    | map f (FlatApp exps) = FlatApp (List.map f exps)

  fun isFixity fixDecs fixity sym =
      case (StringMap.find (fixDecs, sym), fixity) of
          (SOME (Fixity.INfix (n1, m1)), Fixity.INfix (n2, m2)) =>
            n1 = n2 andalso m1 = m2
        | (SOME Fixity.NONfix, Fixity.NONfix) => true
        | _ => false

  fun isInfixR fixDecs n { fixity, item, region } =
      case fixity of
          NONE => false
        | SOME sym => isFixity fixDecs (Fixity.infixright n) (Symbol.name sym)

  fun isInfix fixDecs n { fixity, item, region } =
      case fixity of
          NONE => false
        | SOME sym => isFixity fixDecs (Fixity.infixleft n) (Symbol.name sym)

  fun parseFixityR f [] = FlatApp []
    | parseFixityR f (exp :: exps) =
      if f exp
      then InfixApp (FlatApp [], exp, parseFixityR f exps)
      else
        (case parseFixityR f exps of
             FlatApp exps => FlatApp (exp :: exps)
           | InfixApp (FlatApp es, e, eR) => InfixApp (FlatApp (exp :: es), e, eR)
           | InfixApp (InfixApp _, _, _) =>
               raise Fail "BUG: infix app to left in rfixity parsing")

  fun reverse (FlatApp exps) = FlatApp (List.rev exps)
    | reverse (InfixApp (eL, e, eR)) = InfixApp (reverse eR, e, reverse eL)

  fun parseFixExp (fixity, infixR, fixDecs) (InfixApp (eL, exp, eR)) =
      if fixity > 9
      then InfixApp (eL, exp, eR)
      else
        InfixApp
          (parseFixExp (fixity, infixR, fixDecs) eL
          , exp
          , parseFixExp (fixity, infixR, fixDecs) eR)
    | parseFixExp (fixity, infixR, fixDecs) (FlatApp exps) =
      if fixity > 9
      then FlatApp exps
      else
        if infixR
        then
          parseFixExp
            (fixity + 1, false, fixDecs)
            (parseFixityR (isInfixR fixDecs fixity) exps)
        else
          parseFixExp
            (fixity, true, fixDecs)
            (reverse (parseFixityR (isInfix fixDecs fixity) (List.rev exps)))

  fun parse fixDecs exps = parseFixExp (0, false, fixDecs) (FlatApp exps)
end
