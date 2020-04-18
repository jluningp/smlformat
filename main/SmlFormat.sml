structure SmlFormat : SML_FORMAT = struct
  fun formatted filename =
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
            (fn (line, comment, acc) => (line, List.map (fn (x, _) => x) comment) :: acc)
            []
            (CommentParser.parse filename)

        val elabAst =
          Elab.elaborate
            { comments = ref comments
            , sourceMap = (#sourceMap source)
            , fixity = ref Fixities.basis }
            ast
      in
        Format.formatDec { indent = 0 } elabAst
      end

  fun formatToFile infile outfile =
      let
        val out = TextIO.openOut outfile
      in
        (TextIO.output (out, formatted infile); TextIO.closeOut out)
      end

  fun format filename = TextIO.output (TextIO.stdOut, formatted filename)
end
