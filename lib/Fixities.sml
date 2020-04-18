structure Fixities : FIXITIES = struct
  val basis =
    let
      val fixities =
        [ ("*", 7, false)
        , ("/", 7, false)
        , ("div", 7, false)
        , ("mod", 7, false)
        , ("+", 6, false)
        , ("-", 6, false)
        , ("^", 6, false)
        , ("::", 5, true)
        , ("@", 5, true)
        , ("=", 4, false)
        , ("<>", 4, false)
        , (">", 4, false)
        , (">=", 4, false)
        , ("<", 4, false)
        , ("<=", 4, false)
        , (":=", 3, false)
        , ("o", 3, false)
        , ("before", 0, false) ]
    in
      List.foldl
        (fn ((sym, fixity, isInfixR), fixityMap) =>
            StringMap.insert
              (fixityMap
              , sym
              , if isInfixR then Fixity.infixright fixity else Fixity.infixleft fixity))
        StringMap.empty
        fixities
    end
end
