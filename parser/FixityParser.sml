structure FixityParser : FIXITY_PARSER = struct
  open Ast

  datatype 'a fixexp =
        InfixApp of 'a fixexp * 'a * 'a fixexp
      | FlatApp of 'a list

  fun map f (InfixApp (l, x, r)) = InfixApp (map f l, f x, map f r)
    | map f (FlatApp exps) = FlatApp (List.map f exps)

  fun isInfix7 { fixity, item, region } =
      case fixity of
          NONE => false
        | SOME symbol =>
            case Symbol.name symbol of
                ("*" | "/" | "div" | "mod") => true
              | _ => false

  fun isInfix6 { fixity, item, region } =
      case fixity of
          NONE => false
        | SOME symbol =>
            case Symbol.name symbol of
                ("+" | "-" | "^") => true
              | _ => false

  fun isInfixr5 { fixity, item, region } =
      case fixity of
          NONE => false
        | SOME symbol =>
            case Symbol.name symbol of
                ("::" | "@") => true
              | _ => false

  fun isInfix4 { fixity, item, region } =
      case fixity of
          NONE => false
        | SOME symbol =>
            case Symbol.name symbol of
                ("=" | "<>" | ">" | ">=" | "<" | "<=") => true
              | _ => false

  fun isInfix3 { fixity, item, region } =
      case fixity of
          NONE => false
        | SOME symbol =>
            case Symbol.name symbol of
                (":=" | "o") => true
              | _ => false

  fun isInfix0 { fixity, item, region } =
      case fixity of
          NONE => false
        | SOME symbol =>
            case Symbol.name symbol of
                "before" => true
              | _ => false

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

  (* This function only accounts for the infixity of built-in functions. All
   * others are treated as prefix *)
  (* TODO: Also consider fixity of operators whose fixity
   * is declared inside the file *)
  fun parseFixExp fixity (InfixApp (eL, exp, eR)) =
      InfixApp (parseFixExp fixity eL, exp, parseFixExp fixity eR)
    | parseFixExp fixity (FlatApp exps) =
      case fixity of
          7 => reverse (parseFixityR isInfix7 (List.rev exps))
        | 6 => parseFixExp 7 (reverse (parseFixityR isInfix6 (List.rev exps)))
        | 5 => parseFixExp 6 (parseFixityR isInfixr5 exps)
        | 4 => parseFixExp 5 (reverse (parseFixityR isInfix4 (List.rev exps)))
        | 3 => parseFixExp 4 (reverse (parseFixityR isInfix3 (List.rev exps)))
        | 0 => parseFixExp 3 (reverse (parseFixityR isInfix0 (List.rev exps)))
        | _ => parseFixExp 0 (FlatApp exps)

  fun parse exps = parseFixExp 0 (FlatApp exps)
end
