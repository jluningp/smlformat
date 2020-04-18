structure ExtractLiteral = struct
  fun extractLiteral (_, n) = n
  fun extractRealLiteral (s, _) = s
end
