(* : FORMAT *)
structure Format = struct
  open ElabAst
  val indentSize = 2
  val characters = 75
  type formatInfo = {indent : int}
  fun createIndent n = String.concat (List.tabulate (n, fn _ => " "))

  fun intercalate sep [] = []
    | intercalate sep [ x ] = [ x ]
    | intercalate sep (x :: xs) = x :: sep :: intercalate sep xs

  fun mapFixitem f { fixity, item, region } =
      { fixity = fixity, item = f item, region = region }

  fun mapSigconst f NoSig = NoSig
    | mapSigconst f (Opaque x) = Opaque (f x)
    | mapSigconst f (Transparent x) = Transparent (f x)

  fun shouldNewline formatted =
      String.isSubstring "\n" formatted orelse String.size formatted > characters

  fun pathToString path =
      String.concat (intercalate "." (List.map Symbol.name path))

  fun formatExp (formatInfo as { indent }) exp =
      case exp of
          AndalsoExp (e1, e2) =>
            let
              val e1 = formatExp formatInfo e1
              val e2 = formatExp formatInfo e2
              val oneLine = e1 ^ " andalso " ^ e2
            in
              if shouldNewline oneLine
              then e1 ^ " andalso\n" ^ (createIndent indent) ^ e2
              else oneLine
            end
        | OrelseExp (e1, e2) =>
            let
              val e1 = formatExp formatInfo e1
              val e2 = formatExp formatInfo e2
              val oneLine = e1 ^ " orelse " ^ e2
            in
              if shouldNewline oneLine
              then e1 ^ " orelse\n" ^ (createIndent indent) ^ e2
              else oneLine
            end
        | AppExp { argument : exp, function : exp } =>
            formatExp formatInfo function ^ " " ^ formatExp formatInfo argument
        | CaseExp { expr : exp, rules : rule list } =>
            "case " ^ formatExp formatInfo expr ^ " of\n"
            ^ (createIndent (indent + indentSize + 2))
            ^ formatRules { indent = indent + indentSize } rules
        | CharExp str => "#\"" ^ String.toString str ^ "\""
        | ConstraintExp { constraint : ty, expr : exp } =>
            formatExp formatInfo expr ^ " : " ^ formatTy formatInfo constraint
        | FixAppExp (FixityParser.FlatApp [ exp ]) => formatExp formatInfo exp
        | FixAppExp exps =>
            let
              fun format (FixityParser.FlatApp (exp :: exps)) =
                  let
                    val func = formatExp formatInfo exp
                    val args = List.map (formatExp { indent = indent + indentSize }) exps
                    val argNewline = "\n" ^ (createIndent (indent + indentSize))
                    val oneLine = String.concat (intercalate " " (func :: args))
                  in
                    if shouldNewline oneLine
                    then func ^ argNewline ^ String.concat (intercalate argNewline args)
                    else oneLine
                  end
                | format (FixityParser.FlatApp []) = ""
                | format (FixityParser.InfixApp (aL, exp, aR)) =
                  let
                    val func = formatExp formatInfo exp
                    val argsL = format aL
                    val argsR = format aR
                    val newline = "\n" ^ (createIndent indent)
                    val oneLine = argsL ^ " " ^ func ^ " " ^ argsR
                  in
                    if shouldNewline oneLine
                    then argsL ^ newline ^ func ^ " " ^ argsR
                    else oneLine
                  end
            in
              format exps
            end
        | FnExp rules =>
            "fn "
            ^ (String.concat
               (intercalate
                  ("\n" ^ (createIndent (indent + 1)) ^ "| ")
                  (List.map (formatRule { indent = indent + 1 }) rules)))

        | HandleExp { expr : exp, rules : rule list } =>
            formatExp formatInfo expr ^ " handle "
            ^ formatRules { indent = indent + 7 } rules
        | IfExp { elseCase : exp, test : exp, thenCase : exp } =>
            let
              fun formatPart part partNewlines =
                  if partNewlines
                  then "\n" ^ (createIndent (indent + indentSize)) ^ part
                  else part

              val test = formatExp { indent = indent + indentSize } test
              val testNewlines = shouldNewline test
              val elseCase = formatExp { indent = indent + indentSize } elseCase
              val elseNewlines = shouldNewline elseCase
              val thenCase = formatExp { indent = indent + indentSize } thenCase
              val thenNewlines = shouldNewline thenCase
              val test = formatPart test testNewlines
              val elseCase = formatPart elseCase elseNewlines
              val thenCase = formatPart thenCase thenNewlines

              val notOneLine =
                testNewlines orelse
                thenNewlines orelse
                elseNewlines orelse
                String.size test + String.size thenCase + String.size elseCase + 13
                > characters

              val sep = if notOneLine then "\n" ^ (createIndent indent) else " "
            in
              "if " ^ test ^ sep ^ "then " ^ thenCase ^ sep ^ "else " ^ elseCase
            end
        | IntExp literal => IntInf.toString literal
        | LetExp { dec : dec, expr : exp } =>
            let
              fun reduceSingletonSeq exp =
                  case exp of
                      MarkExp (exp, region) => MarkExp (reduceSingletonSeq exp, region)
                    | CommentExp (comment, exp) => CommentExp (comment, reduceSingletonSeq exp)
                    | FixAppExp (FixityParser.FlatApp [ exp ]) =>
                        FixAppExp (FixityParser.FlatApp [ reduceSingletonSeq exp ])
                    | SeqExp [ exp ] => reduceSingletonSeq exp
                    | _ => exp
            in
              "let" ^ "\n" ^ (createIndent (indent + indentSize))
              ^ formatDec { indent = indent + indentSize } dec
              ^ "\n"
              ^ (createIndent indent)
              ^ "in"
              ^ "\n"
              ^ (createIndent (indent + indentSize))
              ^ formatExp { indent = indent + indentSize } (reduceSingletonSeq expr)
              ^ "\n"
              ^ (createIndent indent)
              ^ "end"
            end
        | ListExp [] => "[]"
        | ListExp exps =>
            let
              val exps = List.map (formatExp formatInfo) exps

              val sep =
                if shouldNewline (String.concat exps)
                then "\n" ^ (createIndent indent) ^ ", "
                else ", "
            in
              "[ " ^ String.concat (intercalate sep exps) ^ " ]"
            end
        | MarkExp (exp, region) => formatExp formatInfo exp
        | RaiseExp e => "raise " ^ formatExp formatInfo e
        | RealExp s => s
        | RecordExp [] => "()"
        | RecordExp exps =>
            let
              val fields =
                (List.map
                   (fn (sym, exp) => Symbol.name sym ^ " = " ^ formatExp formatInfo exp)
                   exps)

              val sep =
                if shouldNewline (String.concat fields)
                then "\n" ^ (createIndent indent) ^ ", "
                else ", "
            in
              "{ " ^ String.concat (intercalate sep fields) ^ " }"
            end
        | SelectorExp sym => "#" ^ (Symbol.name sym)
        | SeqExp exps =>
            let
              val exps = List.map (formatExp { indent = indent + 1 }) exps
              val oneLine = "(" ^ (String.concat (intercalate "; " exps)) ^ ")"
            in
              if shouldNewline oneLine
              then
                "("
                ^ (String.concat (intercalate (";\n" ^ (createIndent (indent + 1))) exps))
                ^ ")"
              else oneLine
            end
        | StringExp str => "\"" ^ (String.toString str) ^ "\""
        | TupleExp exps =>
            let
              val exps = List.map (formatExp formatInfo) exps

              val sep =
                if shouldNewline (String.concat exps)
                then "\n" ^ (createIndent indent) ^ ", "
                else ", "
            in
              "(" ^ String.concat (intercalate sep exps) ^ ")"
            end
        | VarExp path => pathToString path
        | VectorExp exps =>
            (* TODO: Add newlines to large vectors *)
            "#["
            ^ String.concat (intercalate ", " (List.map (formatExp formatInfo) exps))
            ^ "]"
        | WhileExp { expr : exp, test : exp } =>
            let
              fun formatPart part partNewlines =
                  if partNewlines
                  then "\n" ^ (createIndent (indent + indentSize)) ^ part
                  else part

              val test = formatExp { indent = indent + indentSize } test
              val expr = formatExp { indent = indent + indentSize } expr
              val testNewlines = shouldNewline test
              val exprNewlines = shouldNewline expr

              val notOneLine =
                testNewlines orelse
                exprNewlines orelse String.size test + String.size expr + 8 > characters

              val sep = if notOneLine then "\n" ^ (createIndent indent) else " "
            in
              "while " ^ test ^ sep ^ "do " ^ expr
            end
        | WordExp literal => IntInf.toString literal
        | CommentExp (comment, exp) =>
            (String.concat (intercalate ("\n" ^ (createIndent indent)) comment)) ^ "\n"
            ^ (createIndent indent)
            ^ formatExp formatInfo exp

  and formatRules { indent } rules =
      String.concat
        (intercalate
           ("\n" ^ createIndent (Int.max (0, indent + indentSize - 2)) ^ "| ")
           (List.map (formatRule { indent = indent + indentSize }) rules))

  and formatRule (formatInfo as { indent }) (Rule { exp : exp, pat : pat }) =
      let
        val formattedPat = formatPat formatInfo false pat
        val formattedExp = formatExp { indent = indent + indentSize } exp

        val sep =
          if shouldNewline (formattedPat ^ " => " ^ formattedExp)
          then " =>\n" ^ (createIndent (indent + indentSize))
          else " => "
      in
        formattedPat ^ sep ^ formattedExp
      end

  and formatPat (formatInfo as { indent }) appNeedsParens pat =
      (* TODO: newlines in more patterns *)
      case pat of
          AppPat { argument : pat, constr : pat } =>
            formatPat formatInfo true constr ^ " " ^ formatPat formatInfo true argument
        | CharPat str => "#\"" ^ String.toString str ^ "\""
        | ConstraintPat { constraint : ty, pattern : pat } =>
            let
              val app =
                formatPat formatInfo appNeedsParens pattern ^ " : "
                ^ formatTy formatInfo constraint
            in
              if appNeedsParens then "(" ^ app ^ ")" else app
            end
        | FlatAppPat [ pat ] => formatPat formatInfo appNeedsParens (#item pat)
        | FlatAppPat pats =>
            let
              val app =
                String.concat
                  (intercalate
                     " "
                     (List.map (fn pat => formatPat formatInfo true (#item pat)) pats))
            in
              if appNeedsParens then "(" ^ app ^ ")" else app
            end
        | IntPat l => IntInf.toString l
        | LayeredPat { expPat : pat, varPat : pat } =>
            "(" ^ formatPat formatInfo false varPat ^ " as "
            ^ formatPat formatInfo false expPat
            ^ ")"
        | ListPat [] => "[]"
        | ListPat pats =>
            let
              val pats = List.map (formatPat formatInfo false) pats

              val sep =
                if shouldNewline (String.concat pats)
                then "\n" ^ (createIndent indent) ^ ", "
                else ", "
            in
              "[ " ^ String.concat (intercalate sep pats) ^ " ]"
            end
        | MarkPat (pat, region) => formatPat formatInfo appNeedsParens pat
        | OrPat pats =>
            "("
            ^ String.concat
              (intercalate " | " (List.map (formatPat formatInfo false) pats))
            ^ ")"
        | RecordPat { def = [], flexibility } => "()"
        | RecordPat { def : (symbol * pat) list, flexibility : bool } =>
            let
              fun reduceVarPat sym pat =
                  case pat of
                      CommentPat (comment, pat) =>
                        let
                          val (c0, constraints, isVar) = reduceVarPat sym pat
                        in
                          (comment :: c0, constraints, isVar)
                        end
                    | MarkPat (pat, region) => reduceVarPat sym pat
                    | FlatAppPat [ pat ] => reduceVarPat sym (#item pat)
                    | VarPat path => ([], [], Symbol.name sym = pathToString path)
                    | ConstraintPat { constraint, pattern } =>
                        let
                          val (comments, c0, isVar) = reduceVarPat sym pattern
                        in
                          (comments, constraint :: c0, isVar)
                        end
                    | _ => ([], [], false)

              fun mkConstraints [] var = var
                | mkConstraints (x :: xs) var =
                  (mkConstraints xs var) ^ " : " ^ (formatTy formatInfo x)

              val fields =
                (List.map
                   (fn (sym, pat) =>
                       let
                         val (comments, constraints, isVar) = reduceVarPat sym pat
                       in
                         if not isVar
                         then Symbol.name sym ^ " = " ^ formatPat formatInfo false pat
                         else
                           String.concat (List.concat comments)
                           ^ mkConstraints constraints (Symbol.name sym)
                       end)
                   def)

              val sep =
                if shouldNewline (String.concat fields)
                then "\n" ^ (createIndent indent) ^ ", "
                else ", "
            in
              "{ " ^ String.concat (intercalate sep fields) ^ " }"
            end
        | StringPat str => "\"" ^ (String.toString str) ^ "\""
        | TuplePat pats =>
            let
              val pats = List.map (formatPat formatInfo false) pats

              val sep =
                if shouldNewline (String.concat pats)
                then "\n" ^ (createIndent indent) ^ ", "
                else ", "
            in
              "(" ^ String.concat (intercalate sep pats) ^ ")"
            end
        | VarPat path => pathToString path
        | VectorPat pats =>
            "#["
            ^ String.concat
              (intercalate ", " (List.map (formatPat formatInfo false) pats))
            ^ "]"
        | WildPat => "_"
        | WordPat l => IntInf.toString l
        | CommentPat (comment, pat) =>
            (String.concat (intercalate ("\n" ^ (createIndent indent)) comment)) ^ "\n"
            ^ (createIndent indent)
            ^ formatPat formatInfo appNeedsParens pat

  and formatStrexp (formatInfo as { indent }) strexp =
      case strexp of
          AppStr (path, strexps) =>
            pathToString path ^ " ("
            ^ String.concat
              (intercalate " " (List.map (fn (s, _) => formatStrexp formatInfo s) strexps))
            ^ ")"
        | AppStrI (path, strexps) =>
            pathToString path ^ " ("
            ^ String.concat
              (intercalate " " (List.map (fn (s, _) => formatStrexp formatInfo s) strexps))
            ^ ")"
        | BaseStr dec =>
            let
              val dec = formatDec { indent = indent + indentSize } dec
            in
              if dec = ""
              then "struct end"
              else
                "struct\n" ^ (createIndent (indent + indentSize)) ^ dec ^ "\n"
                ^ (createIndent indent)
                ^ "end"
            end
        | ConstrainedStr (strexp, sigConst) =>
            formatStrexp formatInfo strexp ^ formatSigconst formatInfo sigConst
        | LetStr (dec, strexp) =>
            (* TODO: Figure out what a LetStr is. *)
            raise Fail "Let structure not supported"
        | MarkStr (strexp, region) => formatStrexp formatInfo strexp
        | VarStr path => pathToString path
        | CommentStr (comment, strexp) =>
            (String.concat (intercalate ("\n" ^ (createIndent indent)) comment)) ^ "\n"
            ^ (createIndent indent)
            ^ formatStrexp formatInfo strexp

  and formatFctexp (formatInfo as { indent }) fctexp =
      case fctexp of
          AppFct (path, [], sigConst) =>
            pathToString path ^ " ()" ^ formatFsigconst formatInfo sigConst
        | AppFct (path, [ (strexp, _) ], sigConst) =>
            let
              val strexp = formatStrexp formatInfo strexp

              val sep =
                if shouldNewline strexp
                then ("\n" ^ (createIndent (indent + indentSize)))
                else " "
            in
              pathToString path ^ sep ^ "(" ^ strexp ^ ")"
              ^ formatFsigconst formatInfo sigConst
            end
        | AppFct (path, strexps, sigConst) =>
            pathToString path ^ "\n" ^ (createIndent (indent + indentSize))
            ^ (String.concat
               (intercalate
                  ("\n" ^ (createIndent (indent + indentSize)))
                  (List.map
                     (fn (strexp, _) => formatStrexp { indent = indent + indentSize } strexp)
                     strexps)))

            ^ "\n"
            ^ (createIndent (indent + indentSize))
            ^ formatFsigconst { indent = indent + indentSize } sigConst
        | BaseFct { body : strexp
          , constraint : sigexp sigConst
          , params : (symbol option * sigexp) list } =>
            String.concat
              (intercalate
                 " "
                 (List.map
                    (fn (NONE, sigexp) =>
                        let
                          val sigexp = formatSigexp { indent = indent + 1 } true sigexp
                        in
                          if shouldNewline sigexp
                          then "\n" ^ (createIndent (indent + indentSize)) ^ "(" ^ sigexp ^ ")"
                          else "(" ^ sigexp ^ ")"
                        end
                      | (SOME sym, sigexp) =>
                        "(" ^ Symbol.name sym ^ " : " ^ formatSigexp formatInfo false sigexp ^ ")")
                    params))
            ^ formatSigconst { indent = indent + indentSize } constraint
            ^ " = "
            ^ formatStrexp formatInfo body
        | LetFct (dec, fctexp) =>
            (* TODO: Figure out what a LetFct is *)
            raise Fail "Let functor not supported"
        | MarkFct (fctexp, region) => formatFctexp formatInfo fctexp
        | VarFct (path, sigConst) =>
            pathToString path ^ formatFsigconst formatInfo sigConst
        | CommentFct (comment, fctexp) =>
            (String.concat (intercalate ("\n" ^ (createIndent indent)) comment)) ^ "\n"
            ^ (createIndent indent)
            ^ formatFctexp formatInfo fctexp

  and formatWherespec formatInfo (WhStruct (sym, def)) =
      pathToString sym ^ " = " ^ pathToString def
    | formatWherespec formatInfo (WhType (path, tyvars, ty)) =
      formatTyvars formatInfo tyvars ^ pathToString path ^ " = "
      ^ formatTy formatInfo ty

  and formatSigexp (formatInfo as { indent }) isFunctorArg sigexp =
      case sigexp of
          AugSig (sigexp, wherespecs) =>
            formatSigexp formatInfo false sigexp ^ " where type "
            ^ (String.concat
               (intercalate " and " (List.map (formatWherespec formatInfo) wherespecs)))

        | BaseSig [] => "sig end"
        | BaseSig specs =>
            let
              val body =
                let
                  fun formatSpecs NONE [] = ""
                    | formatSpecs (SOME prev) [] = prev
                    | formatSpecs NONE (spec :: specs) =
                      formatSpecs (SOME (formatSpec { indent = indent + indentSize } spec)) specs
                    | formatSpecs (SOME prev) (spec :: specs) =
                      let
                        val spec = formatSpec { indent = indent + indentSize } spec

                        val sep =
                          if shouldNewline prev orelse shouldNewline spec
                          then "\n\n" ^ (createIndent (indent + indentSize))
                          else "\n" ^ (createIndent (indent + indentSize))
                      in
                        prev ^ sep ^ formatSpecs (SOME spec) specs
                      end
                in
                  formatSpecs NONE specs
                end
            in
              if isFunctorArg
              then body
              else
                "sig\n" ^ (createIndent (indent + indentSize)) ^ body ^ "\n"
                ^ (createIndent indent)
                ^ "end"
            end
        | MarkSig (sigexp, region) => formatSigexp formatInfo isFunctorArg sigexp
        | VarSig sym => Symbol.name sym
        | CommentSig (comment, sigexp) =>
            (String.concat (intercalate ("\n" ^ (createIndent indent)) comment)) ^ "\n"
            ^ (createIndent indent)
            ^ formatSigexp formatInfo isFunctorArg sigexp

  and formatSigconst { indent } sigConst =
      case sigConst of
          NoSig => ""
        | Opaque sigexp =>
            " :> " ^ formatSigexp { indent = indent + indentSize } false sigexp
        | Transparent sigexp =>
            " : " ^ formatSigexp { indent = indent + indentSize } false sigexp

  and (* TODO: Support functor signatures *)
  formatFsigconst { indent } sigConst =
      case sigConst of
          NoSig => ""
        | Opaque sigexp =>
            " :> " ^ formatFsigexp { indent = indent + indentSize } sigexp
        | Transparent sigexp =>
            " : " ^ formatFsigexp { indent = indent + indentSize } sigexp

  and formatFsigexp formatInfo fsigexp =
      case fsigexp of
          BaseFsig { param : (symbol option * sigexp) list, result : sigexp } => ""
        | MarkFsig (fsigexp, region) => ""
        | VarFsig sym => ""
        | CommentFsig (comment, sigexp) => ""

  and formatFsigb formatInfo (Fsigb { def : fsigexp, name : symbol }) = ""
    | formatFsigb formatInfo (MarkFsigb (fsigb, region)) = ""
    | formatFsigb formatInfo (CommentFsigb (comment, fsigb)) = ""

  and formatSpec (formatInfo as { indent }) spec =
      case spec of
          DataReplSpec (symbol, path) =>
            "datatype " ^ Symbol.name symbol ^ " = " ^ pathToString path
        | DataSpec { datatycs : db list, withtycs : tb list } =>
            let
              val withtycs =
                case withtycs of
                    [] => ""
                  | _ =>
                      "\n" ^ (createIndent indent) ^ "withtype "
                      ^ (String.concat
                         (intercalate
                            ("\n" ^ (createIndent indent) ^ "and ")
                            (List.map (formatTb { indent = indent + indentSize }) withtycs)))

            in
              "datatype "
              ^ (String.concat
                 (intercalate
                    ("\n" ^ (createIndent indent) ^ "and ")
                    (List.map (formatDb { indent = indent + indentSize }) datatycs)))

              ^ withtycs
            end
        | ExceSpec exns =>
            "exception "
            ^ String.concat
              (intercalate
                 ("\n" ^ createIndent indent ^ "and ")
                 (List.map
                    (fn (sym, ty) =>
                        Symbol.name sym
                        ^ (case ty of
                             NONE => ""
                           | SOME ty => " of " ^ formatTy { indent = indent + indentSize } ty)
                          )
                    exns))
        | FctSpec fsigexps =>
            (* TODO: Support functor signatures *)
            raise Fail "Functor signatures not supported"
        | IncludeSpec sigexp => "include " ^ formatSigexp formatInfo false sigexp
        | MarkSpec (spec, region) => formatSpec formatInfo spec
        | ShareStrSpec paths =>
            "sharing "
            ^ String.concat (intercalate " = " (List.map pathToString paths))
        | ShareTycSpec paths =>
            "sharing type "
            ^ String.concat (intercalate " = " (List.map pathToString paths))
        | StrSpec strs =>
            "structure "
            ^ String.concat
              (intercalate
                 ("\n" ^ createIndent indent ^ "and ")
                 (List.map
                    (fn (sym, sigexp, path) =>
                        Symbol.name sym ^ " : " ^ formatSigexp formatInfo false sigexp
                        ^ (case path of
                             NONE => ""
                           | SOME str => " = " ^ pathToString str)
                          )
                    strs))
        | TycSpec (tys, b) =>
            "type "
            ^ String.concat
              (intercalate
                 ("\n" ^ createIndent indent ^ "and ")
                 (List.map
                    (fn (sym, tyvars, ty) =>
                        formatTyvars formatInfo tyvars ^ Symbol.name sym
                        ^ (case ty of
                             NONE => ""
                           | SOME ty => " = " ^ formatTy { indent = indent + indentSize } ty)
                          )
                    tys))
        | ValSpec syms =>
            "val "
            ^ String.concat
              (intercalate
                 ("\n" ^ createIndent indent ^ "and ")
                 (List.map
                    (fn (sym, ty) =>
                        Symbol.name sym ^ " : " ^ formatTy { indent = indent + indentSize } ty)
                    syms))
        | CommentSpec (comment, spec) =>
            (String.concat (intercalate ("\n" ^ (createIndent indent)) comment)) ^ "\n"
            ^ (createIndent indent)
            ^ formatSpec formatInfo spec

  and formatDec (formatInfo as { indent }) dec =
      case dec of
          AbsDec strbs =>
            (* TODO: Figure out what an absdec is *)
            raise Fail "Absdec not supported"
        | AbstypeDec { abstycs : db list, body : dec, withtycs : tb list } =>
            let
              val withtycs =
                case withtycs of
                    [] => ""
                  | _ =>
                      "\n" ^ (createIndent indent) ^ "withtype "
                      ^ (String.concat
                         (intercalate
                            ("\n" ^ (createIndent indent) ^ "and ")
                            (List.map (formatTb { indent = indent + indentSize }) withtycs)))

            in
              "abstype "
              ^ String.concat
                (intercalate
                   ("\n" ^ createIndent (indent + indentSize) ^ "and")
                   (List.map (formatDb { indent = indent + indentSize }) abstycs))
              ^ " with\n"
              ^ (createIndent (indent + indentSize))
              ^ formatDec { indent = indent + indentSize } body
              ^ "\n"
              ^ (createIndent indent)
              ^ "end"
              ^ withtycs
            end
        | DataReplDec (symbol, path) =>
            "datatype " ^ Symbol.name symbol ^ " = " ^ pathToString path
        | DatatypeDec { datatycs : db list, withtycs : tb list } =>
            let
              val withtycs =
                case withtycs of
                    [] => ""
                  | _ =>
                      "\n" ^ (createIndent indent) ^ "withtype "
                      ^ (String.concat
                         (intercalate
                            ("\n" ^ (createIndent indent) ^ "and ")
                            (List.map (formatTb { indent = indent + indentSize }) withtycs)))

            in
              "datatype "
              ^ (String.concat
                 (intercalate
                    ("\n" ^ (createIndent indent) ^ "and ")
                    (List.map (formatDb { indent = indent + indentSize }) datatycs)))

              ^ withtycs
            end
        | DoDec exp =>
            "do " ^ "\n" ^ (createIndent (indent + indentSize))
            ^ formatExp { indent = indent + indentSize } exp
        | ExceptionDec ebs =>
            "exception "
            ^ String.concat
              (intercalate
                 ("\n" ^ createIndent indent ^ "and ")
                 (List.map (formatEb { indent = indent + indentSize }) ebs))
        | FctDec fctbs =>
            (* TODO: The and should have an indent *)
            "functor "
            ^ String.concat
              (intercalate "\n\nand " (List.map (formatFctb formatInfo) fctbs))
        | SigDec sigbs =>
            (* TODO: The and should have an indent *)
            "signature "
            ^ String.concat
              (intercalate "\n\nand " (List.map (formatSigb formatInfo) sigbs))
        | StrDec strbs =>
            (* TODO: The and should have an indent *)
            "structure "
            ^ String.concat
              (intercalate "\n\nand " (List.map (formatStrb formatInfo) strbs))
        | FixDec { fixity : fixity, ops : symbol list } =>
            Fixity.fixityToString fixity ^ pathToString ops
        | FsigDec fsigbs =>
            (* TODO: Support functor signatures *)
            raise Fail "Functor signatures not supported"
        | FunDec (fbs, tyvars) =>
            let
              val fbs =
                case List.rev fbs of
                    [] => []
                  | x :: xs =>
                      List.rev
                        ((true, formatFb formatInfo x)
                         :: (List.map (fn x => (false, formatFb formatInfo x)) xs))
            in
              "fun " ^ formatTyvars formatInfo tyvars
              ^ (String.concat
                 (intercalate
                    ("\n" ^ (createIndent indent) ^ "and ")
                    (List.map
                       (fn (last, fb) => if not last andalso shouldNewline fb then fb ^ "\n" else fb)
                       fbs)))

            end
        | LocalDec (d1, d2) =>
            "local " ^ "\n " ^ (createIndent (indent + indentSize))
            ^ formatDec { indent = indent + indentSize } d1
            ^ "\n"
            ^ (createIndent indent)
            ^ "in"
            ^ "\n "
            ^ (createIndent (indent + indentSize))
            ^ formatDec { indent = indent + indentSize } d2
            ^ "\n"
            ^ (createIndent indent)
            ^ "end"
        | MarkDec (dec, region) => formatDec formatInfo dec
        | OpenDec paths =>
            "open " ^ (String.concat (intercalate " " (List.map pathToString paths)))
        | OvldDec (symbol, ty, exps) =>
            (* TODO: Figure out what this dec is *)
            raise Fail "OvldDec not supported"
        | SeqDec [] => ""
        | SeqDec decs =>
            let
              fun formatDecs NONE [] = ""
                | formatDecs (SOME prev) [] = prev
                | formatDecs NONE (dec :: decs) =
                  formatDecs (SOME (formatDec formatInfo dec)) decs
                | formatDecs (SOME prev) (dec :: decs) =
                  let
                    val d = formatDec formatInfo dec

                    val sep =
                      if shouldNewline prev orelse shouldNewline d
                      then "\n\n" ^ (createIndent indent)
                      else "\n" ^ (createIndent indent)
                  in
                    prev ^ sep ^ formatDecs (SOME d) decs
                  end
            in
              formatDecs NONE decs
            end
        | TypeDec tbs =>
            "type "
            ^ String.concat
              (intercalate
                 ("\n" ^ (createIndent indent) ^ "and ")
                 (List.map (formatTb formatInfo) tbs))
        | ValDec (vbs, tyvars) =>
            "val "
            ^ String.concat
              (intercalate
                 ("\n" ^ (createIndent indent) ^ "and ")
                 (List.map (formatVb formatInfo) vbs))
        | ValrecDec (rvbs, tyvars) =>
            "val rec " ^ formatTyvars formatInfo tyvars
            ^ String.concat
              (intercalate
                 ("\n\n" ^ (createIndent indent) ^ "and ")
                 (List.map (formatRvb formatInfo) rvbs))
        | CommentDec (comment, dec) =>
            (String.concat (intercalate ("\n" ^ (createIndent indent)) comment)) ^ "\n"
            ^ (createIndent indent)
            ^ formatDec formatInfo dec

  and formatVb (formatInfo as { indent }) vb =
      case vb of
          MarkVb (vb, region) => formatVb formatInfo vb
        | CommentVb (comment, vb) =>
            (String.concat (intercalate ("\n" ^ (createIndent indent)) comment)) ^ "\n"
            ^ (createIndent indent)
            ^ formatVb formatInfo vb
        | Vb { exp : exp, lazyp : bool, pat : pat } =>
            let
              val formattedExp = formatExp { indent = indent + indentSize } exp
              val formattedPat = formatPat formatInfo false pat
              val oneLine = formattedPat ^ " = " ^ formattedExp
              val newline = "\n" ^ createIndent (indent + indentSize)
            in
              if shouldNewline formattedPat
              then newline ^ formattedPat ^ newline ^ "=" ^ newline ^ formattedExp
              else
                if shouldNewline oneLine
                then formattedPat ^ " =\n" ^ (createIndent (indent + indentSize)) ^ formattedExp
                else oneLine
            end

  and formatRvb (formatInfo as { indent }) rvb =
      case rvb of
          CommentRvb (comment, rvb) =>
            (String.concat (intercalate ("\n" ^ (createIndent indent)) comment)) ^ "\n"
            ^ (createIndent indent)
            ^ formatRvb formatInfo rvb
        | MarkRvb (rvb, region) => formatRvb formatInfo rvb
        | Rvb { exp : exp
          , fixity : (symbol * region) option
          , lazyp : bool
          , resultty : ty option
          , var : symbol } =>
            let
              val ty =
                case resultty of
                    NONE => ""
                  | SOME ty => " : " ^ formatTy formatInfo ty

              val sym = Symbol.name var
              val exp = formatExp { indent = indent + indentSize } exp
              val dec = sym ^ ty
              val oneLine = dec ^ " = " ^ exp
            in
              if shouldNewline oneLine
              then dec ^ " =\n" ^ (createIndent (indent + indentSize)) ^ exp
              else oneLine
            end

  and formatFb (formatInfo as { indent }) (Fb (clauses, b)) =
      String.concat
        (intercalate
           ("\n" ^ (createIndent (indent + 2)) ^ "| ")
           (List.map (formatClause { indent = indent + indentSize }) clauses))
    | formatFb formatInfo (MarkFb (fb, region)) = formatFb formatInfo fb
    | formatFb (formatInfo as { indent }) (CommentFb (comment, fb)) =
      (String.concat (intercalate ("\n" ^ (createIndent indent)) comment)) ^ "\n"
      ^ (createIndent indent)
      ^ formatFb formatInfo fb

  and formatClause
    (formatInfo as { indent })
    (Clause { exp : exp, pats : pat fixitem list, resultty : ty option })
    =
    let
        val formattedExp = formatExp { indent = indent + indentSize } exp

        val formattedPats =
          List.map (fn pat => formatPat formatInfo true (#item pat)) pats

        val patNewlines = shouldNewline (String.concat formattedPats)
        val newline = "\n" ^ createIndent indent

        val formattedPats =
          let
            val sep = if patNewlines then newline else " "
          in
            String.concat
              (intercalate
                 sep
                 (List.map (fn pat => formatPat formatInfo true (#item pat)) pats))
          end

        val resultTy =
          case resultty of
              NONE => ""
            | SOME ty =>
                (if patNewlines then newline else "") ^ " : " ^ formatTy formatInfo ty

        val oneLine = formattedPats ^ resultTy ^ " = " ^ formattedExp
      in
        if shouldNewline formattedPats
        then formattedPats ^ resultTy ^ newline ^ "=" ^ newline ^ formattedExp
        else
          if shouldNewline oneLine
          then
            formattedPats ^ resultTy ^ " =\n" ^ createIndent (indent + indentSize)
            ^ formattedExp
          else oneLine
      end

  and formatTyvars formatInfo tyvars =
      case tyvars of
          [] => ""
        | [ tyvar ] => formatTyvar formatInfo tyvar ^ " "
        | _ =>
            "("
            ^ String.concat (intercalate ", " (List.map (formatTyvar formatInfo) tyvars))
            ^ ") "

  and formatTb formatInfo (MarkTb (tb, region)) = formatTb formatInfo tb
    | formatTb formatInfo (Tb { def : ty, tyc : symbol, tyvars : tyvar list }) =
      let
        val tyvars = formatTyvars formatInfo tyvars
      in
        (* TODO: Add a newline if this gets too large *)
        tyvars ^ Symbol.name tyc ^ " = " ^ formatTy formatInfo def
      end
    | formatTb (formatInfo as { indent }) (CommentTb (comment, tb)) =
      (String.concat (intercalate ("\n" ^ (createIndent indent)) comment)) ^ "\n"
      ^ (createIndent indent)
      ^ formatTb formatInfo tb

  and formatDb
    (formatInfo as { indent })
    (Db { lazyp : bool, rhs : (symbol * ty option) list, tyc : symbol, tyvars : tyvar list })
    =
    let
        fun formatVariant (sym, NONE) = Symbol.name sym
          | formatVariant (sym, SOME ty) =
            Symbol.name sym ^ " of " ^ formatTy { indent = indent + indentSize } ty
      in
        formatTyvars formatInfo tyvars ^ Symbol.name tyc ^ " = "
        ^ (case rhs of
             [ variant ] => formatVariant variant
           | _ =>
               "\n" ^ createIndent (indent + indentSize + 2)
               ^ String.concat
                 (intercalate
                    ("\n" ^ createIndent (indent + indentSize) ^ "| ")
                    (List.map formatVariant rhs)))

      end
    | formatDb formatInfo (MarkDb (db, region)) = formatDb formatInfo db
    | formatDb (formatInfo as { indent }) (CommentDb (comment, db)) =
      (String.concat (intercalate ("\n" ^ (createIndent indent)) comment)) ^ "\n"
      ^ (createIndent indent)
      ^ formatDb formatInfo db

  and formatEb (formatInfo as { indent }) eb =
      case eb of
          EbDef { edef : path, exn : symbol } =>
            Symbol.name exn ^ " = " ^ pathToString edef
        | EbGen { etype : ty option, exn : symbol } =>
            Symbol.name exn
            ^ (case etype of
                 NONE => ""
               | SOME ty => " of " ^ formatTy { indent = indent + indentSize } ty)

        | MarkEb (eb, region) => formatEb formatInfo eb
        | CommentEb (comment, eb) =>
            (String.concat (intercalate ("\n" ^ (createIndent indent)) comment)) ^ "\n"
            ^ (createIndent indent)
            ^ formatEb formatInfo eb

  and formatStrb formatInfo (MarkStrb (strb, region)) =
      formatStrb formatInfo strb
    | formatStrb
    (formatInfo as { indent })
    (Strb { constraint : sigexp sigConst, def : strexp, name : symbol })
    =
    let
        val constraint =
          case constraint of
              NoSig => ""
            | Opaque sigexp =>
                " :> " ^ formatSigexp { indent = indent + indentSize } false sigexp
            | Transparent sigexp =>
                " : " ^ formatSigexp { indent = indent + indentSize } false sigexp
      in
        Symbol.name name ^ constraint ^ " = " ^ formatStrexp formatInfo def
      end
    | formatStrb (formatInfo as { indent }) (CommentStrb (comment, strb)) =
      (String.concat (intercalate ("\n" ^ (createIndent indent)) comment)) ^ "\n"
      ^ (createIndent indent)
      ^ formatStrb formatInfo strb

  and formatFctb formatInfo (Fctb { def : fctexp, name : symbol }) =
      Symbol.name name ^ " " ^ formatFctexp formatInfo def
    | formatFctb formatInfo (MarkFctb (fctb, region)) =
      formatFctb formatInfo fctb
    | formatFctb (formatInfo as { indent }) (CommentFctb (comment, fctb)) =
      (String.concat (intercalate ("\n" ^ (createIndent indent)) comment)) ^ "\n"
      ^ (createIndent indent)
      ^ formatFctb formatInfo fctb

  and formatSigb formatInfo (MarkSigb (sigb, region)) =
      formatSigb formatInfo sigb
    | formatSigb formatInfo (Sigb { def : sigexp, name : symbol }) =
      Symbol.name name ^ " = " ^ formatSigexp formatInfo false def
    | formatSigb (formatInfo as { indent }) (CommentSigb (comment, sigb)) =
      (String.concat (intercalate ("\n" ^ (createIndent indent)) comment)) ^ "\n"
      ^ (createIndent indent)
      ^ formatSigb formatInfo sigb

  and formatTyvar formatInfo (MarkTyv (tyvar, region)) =
      formatTyvar formatInfo tyvar
    | formatTyvar formatInfo (Tyv sym) = Symbol.name sym
    | formatTyvar (formatInfo as { indent }) (CommentTyv (comment, tyvar)) =
      (String.concat (intercalate ("\n" ^ (createIndent indent)) comment)) ^ "\n"
      ^ (createIndent indent)
      ^ formatTyvar formatInfo tyvar

  and formatTy'
    (formatInfo as { indent })
    (parens : {con : bool, tuple : bool, arrow : bool})
    ty
    =
    case ty of
          ConTy (syms, []) => pathToString syms
        | ConTy (syms, [ arg ]) =>
            let
              val conty =
                formatTy' formatInfo { con = false, tuple = true, arrow = true } arg ^ " "
                ^ pathToString syms
            in
              if #con parens then "(" ^ conty ^ ")" else conty
            end
        | ConTy ([ sym ], tys) =>
            let
              fun formatArrowArgs [] = []
                | formatArrowArgs [ ty ] =
                  [ formatTy' formatInfo { con = false, tuple = false, arrow = false } ty ]
                | formatArrowArgs (ty :: tys) =
                  formatTy' formatInfo { con = false, tuple = false, arrow = true } ty
                  :: formatArrowArgs tys

              val needsParens = if Symbol.name sym = "->" then #arrow parens else #con parens

              val con =
                if Symbol.name sym = "->"
                then String.concat (intercalate " -> " (formatArrowArgs tys))
                else
                  "("
                  ^ String.concat
                    (intercalate
                       ", "
                       (List.map
                          (formatTy' formatInfo { con = false, tuple = false, arrow = false })
                          tys))
                  ^ ") "
                  ^ Symbol.name sym
            in
              if needsParens then "(" ^ con ^ ")" else con
            end
        | ConTy (syms, tys) =>
            let
              val conty =
                "("
                ^ String.concat
                  (intercalate
                     ", "
                     (List.map
                        (formatTy' formatInfo { con = false, tuple = false, arrow = false })
                        tys))
                ^ ") "
                ^ pathToString syms
            in
              if #con parens then "(" ^ conty ^ ")" else conty
            end
        | MarkTy (ty, region) => formatTy' formatInfo parens ty
        | RecordTy tys =>
            "{"
            ^ String.concat
              (intercalate
                 ", "
                 (List.map
                    (fn (sym, ty) =>
                        Symbol.name sym ^ " : "
                        ^ formatTy' formatInfo { con = false, tuple = false, arrow = false } ty)
                    tys))
            ^ "}"
        | TupleTy tys =>
            let
              val tuplety =
                String.concat
                  (intercalate
                     " * "
                     (List.map
                        (formatTy' formatInfo { con = false, tuple = true, arrow = true })
                        tys))
            in
              if #tuple parens then "(" ^ tuplety ^ ")" else tuplety
            end
        | VarTy tyvar => formatTyvar formatInfo tyvar
        | CommentTy (comment, ty) =>
            (String.concat (intercalate ("\n" ^ (createIndent indent)) comment)) ^ "\n"
            ^ (createIndent indent)
            ^ formatTy' formatInfo parens ty

  and formatTy formatInfo ty =
      formatTy' formatInfo { con = false, tuple = false, arrow = false } ty
end
