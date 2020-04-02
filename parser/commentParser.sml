structure CommentParser = struct

  fun fold_stream (f : 'a * TextIO.elem -> 'a) (base : 'a) (instream : TextIO.StreamIO.instream) : 'a  =
      case TextIO.StreamIO.input1 instream of
          NONE => base
        | SOME (elem, instream) => fold_stream f (f (base, elem)) instream

  fun insertMultiMap map key data =
      case IntMap.find (map, key) of
          NONE => IntMap.insert (map, key, [ data ])
       |  SOME datas => IntMap.insert (map, key, data::datas)

  fun addToComment openCommentSyms str char =
      if openCommentSyms > 0
      then str ^ String.str char
      else str

  fun combine ((prevChar, openCommentSyms, isOpenQuote, openComment, comments, line), char) =
      let val extendedComment = addToComment openCommentSyms openComment char
          val () = print (String.str prevChar ^ " " ^ String.str char
                          ^ " " ^ (Int.toString openCommentSyms) ^
                          " " ^ (Bool.toString isOpenQuote) ^ " [openComment] "
                          ^ " [comments] " ^ Int.toString line ^ "\n")
      in
        case (prevChar, char) of
            (#"*" , #")") =>
            if isOpenQuote
            then (prevChar, openCommentSyms, isOpenQuote, extendedComment, comments, line)
            else
              if openCommentSyms > 1
              then (char, openCommentSyms - 1, isOpenQuote, extendedComment, comments, line)
              else
                if openCommentSyms = 1
                then (char
                     , 0
                     , isOpenQuote
                     , ""
                     , insertMultiMap comments line extendedComment
                     , line)
                else raise Fail "Parse error: Invalid comments"
          | (#"\\", #"\\") =>
            if openCommentSyms > 0
            then (char, openCommentSyms, isOpenQuote, extendedComment, comments, line)
            else (#" ", openCommentSyms, isOpenQuote, extendedComment, comments, line)
          | (#"\\", #"\"") =>
            if openCommentSyms > 0
            then (char, openCommentSyms, isOpenQuote, extendedComment, comments, line)
            else (char, openCommentSyms, isOpenQuote, extendedComment, comments, line)
          | (_   , #"\"") =>
            if openCommentSyms > 0
            then (char, openCommentSyms, isOpenQuote, extendedComment, comments, line)
            else (char, openCommentSyms, not isOpenQuote, openComment, comments, line)
          | (#"(", #"*" ) => (* f99 *)
            let
              val newCommentSyms = if isOpenQuote then openCommentSyms else openCommentSyms + 1
              val newComment = if isOpenQuote then extendedComment else "(*" ^ extendedComment
            in
              (char, newCommentSyms, isOpenQuote, newComment, comments, line)
            end
          | (_   , #"\n") => (char, openCommentSyms, isOpenQuote, extendedComment, comments, line + 1)
          | (_   , char ) => (char, openCommentSyms, isOpenQuote, extendedComment, comments, line)
      end

  fun parse filename =
      let
        val stream = TextIO.getInstream (TextIO.openIn filename)
        val (_, _, _, _, comments, _) =
            (* Doesn't matter what we put as the starting prevChar *)
            fold_stream combine (#"\n", 0, false, "", IntMap.empty, 1) stream
      in
        comments
      end
end
