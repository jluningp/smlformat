structure Utils = struct
  fun getSource filename =
      let
        val stream = TextIO.openIn filename
        val interactive = false
        val consumer = ErrorMsg.defaultConsumer ()
      in
        Source.newSource (filename, stream, interactive, consumer)
      end

  fun runTest filename =
      let
        val source = getSource filename
        val ast = SmlFile.parse source
        val comments =
            IntMap.foldli
                (fn (line, comment, acc) => (line, List.map (fn (x,_)=>x) comment)::acc)
                []
                (CommentParser.parse filename)
        val commentedAst =
            AddComments.convertDec {comments=ref comments, sourceMap=(#sourceMap source)} ast
        val () = print ("**OUTPUT**\n" ^ Format.formatDec {indent=0} commentedAst ^ "**END OUTPUT**\n")
      in
        (comments,
         commentedAst)
      end

end
