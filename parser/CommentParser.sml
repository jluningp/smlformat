structure CommentParser : COMMENT_PARSER = struct
  structure ParseInfo = struct
    type t =
      { previousCharacter : char
      , openCommentSymbolCount : int
      , inStringLiteral : bool
      , currentComment : string
      , comments : (string * int) list IntMap.map
      , currentLineNumber : int
      , currentColumnNumber : int }

    fun flipInStringLiteral
        { previousCharacter
        , openCommentSymbolCount
        , inStringLiteral
        , currentComment
        , comments
        , currentLineNumber
        , currentColumnNumber }
        =
        { previousCharacter = previousCharacter
        , openCommentSymbolCount = openCommentSymbolCount
        , inStringLiteral = not inStringLiteral
        , currentComment = currentComment
        , comments = comments
        , currentLineNumber = currentLineNumber
        , currentColumnNumber = currentColumnNumber }

    fun updatePreviousCharacter
        { previousCharacter
        , openCommentSymbolCount
        , inStringLiteral
        , currentComment
        , comments
        , currentLineNumber
        , currentColumnNumber }
        char
        =
        { previousCharacter = char
        , openCommentSymbolCount = openCommentSymbolCount
        , inStringLiteral = inStringLiteral
        , currentComment = currentComment
        , comments = comments
        , currentLineNumber = currentLineNumber
        , currentColumnNumber = currentColumnNumber }

    fun appendToCurrentComment
        { previousCharacter
        , openCommentSymbolCount
        , inStringLiteral
        , currentComment
        , comments
        , currentLineNumber
        , currentColumnNumber }
        char
        =
        { previousCharacter = previousCharacter
        , openCommentSymbolCount = openCommentSymbolCount
        , inStringLiteral = inStringLiteral
        , currentComment = currentComment ^ String.str char
        , comments = comments
        , currentLineNumber = currentLineNumber
        , currentColumnNumber = currentColumnNumber }

    fun clearCurrentComment
        { previousCharacter
        , openCommentSymbolCount
        , inStringLiteral
        , currentComment
        , comments
        , currentLineNumber
        , currentColumnNumber }
        =
        { previousCharacter = previousCharacter
        , openCommentSymbolCount = openCommentSymbolCount
        , inStringLiteral = inStringLiteral
        , currentComment = "(*"
        , comments = comments
        , currentLineNumber = currentLineNumber
        , currentColumnNumber = currentColumnNumber }

    fun inStringLiteral (t : t) = #inStringLiteral t
    fun openCommentSymbolCount (t : t) = #openCommentSymbolCount t

    fun incOpenCommentSymbolCount
        { previousCharacter
        , openCommentSymbolCount
        , inStringLiteral
        , currentComment
        , comments
        , currentLineNumber
        , currentColumnNumber }
        =
        { previousCharacter = previousCharacter
        , openCommentSymbolCount = openCommentSymbolCount + 1
        , inStringLiteral = inStringLiteral
        , currentComment = currentComment
        , comments = comments
        , currentLineNumber = currentLineNumber
        , currentColumnNumber = currentColumnNumber }

    fun decOpenCommentSymbolCount
        { previousCharacter
        , openCommentSymbolCount
        , inStringLiteral
        , currentComment
        , comments
        , currentLineNumber
        , currentColumnNumber }
        =
        { previousCharacter = previousCharacter
        , openCommentSymbolCount = openCommentSymbolCount - 1
        , inStringLiteral = inStringLiteral
        , currentComment = currentComment
        , comments = comments
        , currentLineNumber = currentLineNumber
        , currentColumnNumber = currentColumnNumber }

    fun insertMultiMap map key data =
        case IntMap.find (map, key) of
            NONE => IntMap.insert (map, key, [ data ])
          | SOME datas => IntMap.insert (map, key, data :: datas)

    fun commentEnded
        { previousCharacter
        , openCommentSymbolCount
        , inStringLiteral
        , currentComment
        , comments
        , currentLineNumber
        , currentColumnNumber }
        =
        { previousCharacter = previousCharacter
        , openCommentSymbolCount = openCommentSymbolCount
        , inStringLiteral = inStringLiteral
        , currentComment = currentComment
        , comments = insertMultiMap
          comments
          currentLineNumber
          (currentComment, currentColumnNumber - 1)
        , currentLineNumber = currentLineNumber
        , currentColumnNumber = currentColumnNumber }

    fun newLine
        { previousCharacter
        , openCommentSymbolCount
        , inStringLiteral
        , currentComment
        , comments
        , currentLineNumber
        , currentColumnNumber }
        =
        { previousCharacter = previousCharacter
        , openCommentSymbolCount = openCommentSymbolCount
        , inStringLiteral = inStringLiteral
        , currentComment = currentComment
        , comments = comments
        , currentLineNumber = currentLineNumber + 1
        , currentColumnNumber = 0 }

    fun incColumn
        { previousCharacter
        , openCommentSymbolCount
        , inStringLiteral
        , currentComment
        , comments
        , currentLineNumber
        , currentColumnNumber }
        =
        { previousCharacter = previousCharacter
        , openCommentSymbolCount = openCommentSymbolCount
        , inStringLiteral = inStringLiteral
        , currentComment = currentComment
        , comments = comments
        , currentLineNumber = currentLineNumber
        , currentColumnNumber = currentColumnNumber + 1 }

    val base =
      { previousCharacter = #" "
      , openCommentSymbolCount = 0
      , inStringLiteral = false
      , currentComment = "(*"
      , comments = IntMap.empty
      , currentLineNumber = 1
      , currentColumnNumber = 0 }

    fun toString
        { previousCharacter
        , openCommentSymbolCount
        , inStringLiteral
        , currentComment
        , comments
        , currentLineNumber
        , currentColumnNumber }
        =
        "((previousCharacter " ^ String.toString (String.str previousCharacter)
        ^ ") (openCommentSymbolCount "
        ^ Int.toString openCommentSymbolCount
        ^ ") (inStringLiteral "
        ^ Bool.toString inStringLiteral
        ^ ") (currentComment "
        ^ String.toString currentComment
        ^ ") (comments [opaque]) (currentLineNumber "
        ^ Int.toString currentLineNumber
        ^ ") (currentColumnNumber "
        ^ Int.toString currentColumnNumber
        ^ "))"
  end

  fun fold_stream
      (f : 'a * TextIO.elem -> 'a)
      (base : 'a)
      (instream : TextIO.StreamIO.instream)
      : 'a
      =
      case TextIO.StreamIO.input1 instream of
          NONE => base
        | SOME (elem, instream) => fold_stream f (f (base, elem)) instream

  fun combine (parseInfo : ParseInfo.t, char) =
      let
        val openCommentSymbolCount = ParseInfo.openCommentSymbolCount parseInfo

        fun appendChar char' =
            let
              val info =
                if openCommentSymbolCount > 0
                then ParseInfo.appendToCurrentComment parseInfo char
                else parseInfo
            in
              if char <> #"\n"
              then ParseInfo.incColumn (ParseInfo.updatePreviousCharacter info char')
              else ParseInfo.updatePreviousCharacter info char'
            end
      in
        case (#previousCharacter parseInfo, char) of
            (#"*", #")") =>
              if ParseInfo.inStringLiteral parseInfo
              then appendChar char
              else
                if openCommentSymbolCount > 1
                then ParseInfo.decOpenCommentSymbolCount (appendChar char)
                else
                  if openCommentSymbolCount = 1
                  then
                    (ParseInfo.clearCurrentComment
                       (ParseInfo.commentEnded
                          (ParseInfo.decOpenCommentSymbolCount (appendChar char))))
                  else raise Fail "Parse error: Invalid comments"
          | (#"\\", #"\\") =>
              if openCommentSymbolCount > 0 then appendChar char else appendChar (#" ")
          | (#"\\", #"\"") => appendChar char
          | (_, #"\"") =>
              if openCommentSymbolCount > 0
              then appendChar char
              else ParseInfo.flipInStringLiteral (appendChar char)
          | (#"(", #"*") =>
              if ParseInfo.inStringLiteral parseInfo
              then appendChar char
              else (ParseInfo.incOpenCommentSymbolCount (appendChar char))
          | (_, #"\n") => ParseInfo.newLine (appendChar char)
          | (_, char) => appendChar char
      end

  fun parse filename =
      let
        val stream = TextIO.getInstream (TextIO.openIn filename)
        val result = fold_stream combine ParseInfo.base stream
      in
        #comments result
      end
end
