structure SMLParser = struct
  fun getSource filename =
      let
        val stream = TextIO.openIn filename
        val interactive = false
        val consumer = ErrorMsg.defaultConsumer ()
      in
        Source.newSource (filename, stream, interactive, consumer)
      end

  fun parse filename =
      let
        val stream = TextIO.openIn filename
        val interactive = false
        val consumer = ErrorMsg.defaultConsumer ()
        val source = Source.newSource (filename, stream, interactive, consumer)
      in
        SmlFile.parse source
      end
end
