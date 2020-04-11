structure SmlFormat : SML_FORMAT = struct
  fun format filename =
      let
        val source =
          let
            val stream = TextIO.openIn filename
            val interactive = false
            val consumer = ErrorMsg.defaultConsumer ()
          in
            Source.newSource (filename, stream, interactive, consumer)
          end

        val ast = SmlFile.parse source
        val comments =
          IntMap.foldli
          (fn (line, comment, acc) => (line, List.map (fn (x, _) => x) comment) :: acc) []
          (CommentParser.parse filename)

        val commentedAst =
          AddComments.convertDec {comments = ref comments, sourceMap = (#sourceMap source)} ast
      in
        TextIO.output (TextIO.stdErr, (Format.formatDec {indent = 0} commentedAst))
      end
end
