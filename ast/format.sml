structure Format = struct
  open CommentedAst

  val indentSize = 2
  val characters = 85

  type formatInfo = { indent : int }

  fun createIndent n = String.concat (List.tabulate (n, fn _ => " "))

  fun intercalate sep [] = []
    | intercalate sep [x] = [x]
    | intercalate sep (x::xs) = x :: sep :: intercalate sep xs

  fun mapFixitem f {fixity, item, region} =
      {fixity=fixity, item=f item, region=region}

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
            then e1 ^ " andalso\n" ^ (createIndent indent)  ^ e2
            else oneLine
          end
        | OrelseExp (e1, e2) =>
          let
            val e1 = formatExp formatInfo e1
            val e2 = formatExp formatInfo e2
            val oneLine = e1 ^ " orelse " ^ e2
          in
            if shouldNewline oneLine
            then e1 ^ "orelse\n" ^ (createIndent indent) ^ e2
            else oneLine
          end
        | AppExp {argument:exp, function:exp} =>
          formatExp formatInfo function ^ " " ^ formatExp formatInfo argument
        | CaseExp {expr:exp, rules:rule list} =>
          "case " ^ formatExp formatInfo expr ^ " of\n"
          ^ (createIndent (indent + indentSize + 2))
          ^ formatRules { indent = indent + indentSize } rules
        | CharExp str => "#\"" ^ str ^ "\""
        | ConstraintExp {constraint:ty, expr:exp} =>
          formatExp formatInfo expr ^ " : " ^ formatTy formatInfo constraint
        | FlatAppExp [ exp ] => formatExp formatInfo (#item exp)
        | FlatAppExp exps =>
          let
            (* Indentation on this is broken for the singleton case, since formatExp should
             * be called with indent, not indent + indentSize. However, in the singleton case,
             * it shouldn't matter, because it should be all on one line anyway  *)
            val exps = List.map (fn exp => formatExp { indent = indent + indentSize } (#item exp)) exps
            fun formatExpsFit [] = []
              | formatExpsFit (e::es) =
                let
                  val size = String.size e
                  val otherExps = formatExpsFit es
                in
                  case otherExps of
                      [] => [ e ]
                    | x::xs =>
                      if String.size x + size + 1 > characters
                      then e::x::xs
                      else (x ^ " " ^ e)::xs
                end
            val exps = formatExpsFit (List.rev exps)
          in
            case List.rev exps of
                [] => ""
              | [ x ] => x
              | x::xs => String.concat (intercalate ("\n" ^ (createIndent (indent))) (x::xs))
          end
        | FnExp [] => raise Fail "Invalid parse. Function expression has no clauses."
        | FnExp [rule] => "fn " ^ formatRule formatInfo rule
        | FnExp (rule::rules) =>
          let
            val sep = ("\n" ^ (createIndent (indent + 1)) ^ "| ")
          in
            "fn " ^ (formatRule formatInfo rule)
            ^ sep ^ (String.concat
                         (intercalate
                              sep
                              (List.map (formatRule {indent = indent + 1}) rules)))
          end
        | HandleExp {expr:exp, rules:rule list} =>
          formatExp formatInfo expr ^ " handle " ^ formatRules { indent = indent + 7 } rules
        | IfExp {elseCase:exp, test:exp, thenCase:exp} =>
          let
            fun formatPart part partNewlines =
                if partNewlines
                then "\n" ^ (createIndent (indent + indentSize)) ^ part
                else part
            val test = formatExp { indent = indent + indentSize } test
            val testNewlines = shouldNewline test
            val elseCase = formatExp { indent = indent + indentSize } elseCase
            val elseNewlines = shouldNewline elseCase
            val thenCase = formatExp { indent = indent + indentSize }  thenCase
            val thenNewlines = shouldNewline thenCase
            val test = formatPart test testNewlines
            val elseCase = formatPart elseCase elseNewlines
            val thenCase = formatPart thenCase thenNewlines
            val notOneLine =
                testNewlines orelse thenNewlines orelse elseNewlines
                orelse String.size test + String.size thenCase + String.size elseCase + 13 > characters
            val sep = if notOneLine then "\n" ^ (createIndent indent) else " "
          in
            "if " ^ test ^ sep ^ "then " ^ thenCase ^ sep ^ "else " ^ elseCase
          end
        | IntExp literal => IntInf.toString literal
        | LetExp {dec:dec, expr:exp} =>
          let
            fun reduceSingletonSeq exp =
                case exp of
                    MarkExp (exp, region) => MarkExp (reduceSingletonSeq exp, region)
                  | CommentExp (comment, exp) => CommentExp (comment, reduceSingletonSeq exp)
                  | FlatAppExp [ exp ] => FlatAppExp [ mapFixitem reduceSingletonSeq exp ]
                  | SeqExp [ exp ] => reduceSingletonSeq exp
                  | _ => exp
          in
            "let" ^ "\n" ^ (createIndent (indent + indentSize))
            ^ formatDec { indent = indent + indentSize } dec
            ^ "\n" ^ (createIndent indent) ^ "in"
            ^ "\n" ^ (createIndent (indent + indentSize))
            ^ formatExp {indent = indent + indentSize } (reduceSingletonSeq expr)
            ^ "\n" ^ (createIndent indent) ^ "end"
          end
        | ListExp exps => "[" ^ String.concat (intercalate ", " (List.map (formatExp formatInfo) exps)) ^ "]"
        | MarkExp (exp, region) => formatExp formatInfo exp
        | RaiseExp e => " raise " ^ formatExp formatInfo e
        | RealExp s => s
        | RecordExp [] => "()"
        | RecordExp exps =>
          (* This, lists, vectors, and tuples should have multi-line modes *)
          "{" ^
          String.concat
              (intercalate
                   ", "
                   (List.map (fn (sym, exp) => Symbol.name sym ^ " = " ^ formatExp formatInfo exp) exps))
          ^ "}"
        | SelectorExp sym => "#" ^ (Symbol.name sym)
        | SeqExp exps => "(" ^ String.concat (intercalate "; " (List.map (formatExp formatInfo) exps)) ^ ")"
        | StringExp str => "\"" ^  (String.toString str) ^ "\""
        | TupleExp exps => "(" ^ String.concat (intercalate ", " (List.map (formatExp formatInfo) exps)) ^ ")"
        | VarExp path => pathToString path
        | VectorExp exps => "#[" ^ String.concat (intercalate ", " (List.map (formatExp formatInfo) exps)) ^ "]"
        | WhileExp {expr:exp, test:exp} =>
          let
            fun formatPart part partNewlines =
                if partNewlines
                then "\n" ^ (createIndent (indent + indentSize)) ^ part
                else part
            val test = formatExp { indent = indent + indentSize } test
            val expr = formatExp { indent = indent + indentSize } expr
            val testNewlines = shouldNewline test
            val exprNewlines = shouldNewline expr
            val notOneLine = testNewlines orelse exprNewlines
                             orelse String.size test + String.size expr + 8 > characters
            val sep = if notOneLine then "\n" ^ (createIndent indent) else " "
          in
            "while " ^ test ^ sep ^ "do " ^ expr
          end
        | WordExp literal => IntInf.toString literal
        | CommentExp (comment, exp) => ""

  and formatRules { indent } rules =
      String.concat
          (intercalate
               ("\n" ^ createIndent (Int.max (0, indent + indentSize - 2)) ^ "| ")
               (List.map (formatRule { indent = indent + indentSize }) rules))


  and formatRule (formatInfo as {indent}) (Rule {exp:exp, pat:pat}) =
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

  and formatPat formatInfo appNeedsParens pat =
      case pat of
          AppPat {argument:pat, constr:pat} =>
          formatPat formatInfo true constr ^ " " ^ formatPat formatInfo true argument
        | CharPat str => "#" ^ str
        | ConstraintPat {constraint:ty, pattern:pat} =>
          let
            val app = formatPat formatInfo appNeedsParens pattern ^ " : " ^ formatTy formatInfo constraint
          in
            if appNeedsParens
            then "(" ^ app ^ ")"
            else app
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
            if appNeedsParens
            then "(" ^ app ^ ")"
            else app
          end
        | IntPat l => IntInf.toString l
        | LayeredPat {expPat:pat, varPat:pat} =>
          "(" ^ formatPat formatInfo false varPat ^ " as " ^ formatPat formatInfo false expPat ^ ")"
        | ListPat pats =>
          "[" ^ (String.concat (intercalate ", " (List.map (formatPat formatInfo false) pats))) ^ "]"
        | MarkPat (pat, region) => formatPat formatInfo appNeedsParens pat
        | OrPat pats => "(" ^ String.concat (intercalate " | " (List.map (formatPat formatInfo false) pats)) ^ ")"
        | RecordPat { def = [], flexibility } => "()"
        | RecordPat {def:(symbol * pat) list, flexibility:bool} =>
          let
            fun reduceVarPat sym pat =
                case pat of
                    CommentPat (comment, pat) =>
                    let
                      val (c0, constraints, isVar) = reduceVarPat sym pat
                    in
                      (comment::c0, constraints, isVar)
                    end
                  | MarkPat (pat, region) => reduceVarPat sym pat
                  | FlatAppPat [ pat ] => reduceVarPat sym (#item pat)
                  | VarPat path => ([], [], Symbol.name sym = pathToString path)
                  | ConstraintPat {constraint, pattern} =>
                    let
                      val (comments, c0, isVar) = reduceVarPat sym pattern
                    in
                      (comments, constraint::c0, isVar)
                    end
                  | _ => ([], [], false)

            fun mkConstraints [] var = var
              | mkConstraints (x::xs) var = (mkConstraints xs var) ^ " : " ^ (formatTy formatInfo x)
          in
            "{" ^
            String.concat
                (intercalate
                     ", "
                     (List.map (fn (sym, pat) =>
                                   let
                                     val (comments, constraints, isVar) = reduceVarPat sym pat
                                   in
                                     if not isVar
                                     then Symbol.name sym ^ " = " ^ formatPat formatInfo false pat
                                     else String.concat comments ^ mkConstraints constraints (Symbol.name sym)
                                   end
                                   ) def))
            ^ "}"
          end
        | StringPat str => "\"" ^  (String.toString str) ^ "\""
        | TuplePat pats => "(" ^ String.concat (intercalate ", " (List.map (formatPat formatInfo false) pats)) ^ ")"
        | VarPat path => pathToString path
        | VectorPat pats =>
          "#[" ^ String.concat (intercalate ", " (List.map (formatPat formatInfo false) pats)) ^ "]"
        | WildPat => "_"
        | WordPat l => IntInf.toString l
        | CommentPat (comment, pat) => ""

  and formatStrexp (formatInfo as { indent }) strexp =
      case strexp of
          AppStr (path, strexps) =>
          pathToString path
          ^ " ("
          ^ String.concat (intercalate " " (List.map (fn (s, _) => formatStrexp formatInfo s) strexps))
          ^ ")"
        | AppStrI (path, strexps) =>
          pathToString path
          ^ " ("
          ^ String.concat (intercalate " " (List.map (fn (s, _) => formatStrexp formatInfo s) strexps))
          ^ ")"
        | BaseStr dec =>
          let
            val dec = formatDec { indent = indent + indentSize } dec
          in
            if dec = ""
            then "struct end"
            else
              "struct\n" ^ (createIndent (indent + indentSize))
              ^ dec ^ "\n" ^ (createIndent indent) ^ "end"
          end
        | ConstrainedStr (strexp, sigConst) =>
          formatStrexp formatInfo strexp ^ formatSigconst formatInfo sigConst
        (* I don't know what this is supposed to look like *)
        | LetStr (dec, strexp) => raise Fail "Let structure not supported"
        | MarkStr (strexp, region) => formatStrexp formatInfo strexp
        | VarStr path => pathToString path
        | CommentStr (comment, strexp) => ""

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
            pathToString path ^ sep ^ "(" ^ strexp
            ^ ")" ^ formatFsigconst formatInfo sigConst
          end
        | AppFct (path, strexps, sigConst) =>
          pathToString path
          ^ "\n" ^ (createIndent (indent + indentSize)) ^
          (String.concat
               (intercalate
                    ("\n" ^ (createIndent (indent + indentSize)))
                    (List.map (fn (strexp, _) => formatStrexp {indent = indent + indentSize} strexp)
                              strexps)))
          ^ "\n" ^ (createIndent (indent + indentSize))
          ^ formatFsigconst { indent = indent + indentSize } sigConst
        | BaseFct {body:strexp, constraint:sigexp sigConst,
                   params:(symbol option * sigexp) list} =>
          String.concat
              (intercalate " "
                           (List.map (fn (NONE, sigexp) =>
                                         let
                                           val sigexp = formatSigexp {indent = indent + 1} true sigexp
                                         in
                                           if shouldNewline sigexp
                                           then "\n" ^ (createIndent (indent + indentSize)) ^ "(" ^ sigexp ^ ")"
                                           else "(" ^ sigexp ^ ")"
                                         end
                        | (SOME sym, sigexp) =>
                          "(" ^ Symbol.name sym ^ " : " ^ formatSigexp formatInfo false sigexp ^ ")")
                        params))
          ^ formatSigconst { indent = indent + indentSize } constraint
          ^ " = " ^ formatStrexp formatInfo body
        (* Wtf is this *)
        | LetFct (dec, fctexp) => ""
        | MarkFct (fctexp, region) => formatFctexp formatInfo fctexp
        | VarFct (path, sigConst) => pathToString path ^ formatFsigconst formatInfo sigConst
        | CommentFct (comment, fctexp) => ""

  and formatWherespec formatInfo (WhStruct (sym, def)) = pathToString sym ^ " = " ^ pathToString def
    | formatWherespec formatInfo (WhType (path, tyvars, ty)) =
      formatTyvars formatInfo tyvars ^ pathToString path ^ " = " ^ formatTy formatInfo ty

  and formatSigexp (formatInfo as { indent }) isFunctorArg sigexp =
      case sigexp of
          AugSig (sigexp, wherespecs) =>
          formatSigexp formatInfo false sigexp ^
          " where type "
          ^ (String.concat (intercalate " and " (List.map (formatWherespec formatInfo) wherespecs)))
        | BaseSig [] => "sig end"
        | BaseSig specs =>
          let
            val body =
                String.concat
                    (intercalate ("\n" ^ (createIndent (indent + indentSize)))
                                 (List.map (formatSpec { indent = indent + indentSize }) specs))
          in
            if isFunctorArg
            then body
            else "sig\n" ^ (createIndent (indent + indentSize)) ^ body
                 ^ "\n" ^ (createIndent indent) ^ "end"
          end
        | MarkSig (sigexp, region) => formatSigexp formatInfo isFunctorArg sigexp
        | VarSig sym => Symbol.name sym
        | CommentSig (comment, sigexp) => ""

  and formatSigconst { indent }  sigConst =
      case sigConst of
          NoSig => ""
        | Opaque sigexp => " :> " ^ formatSigexp { indent = indent + indentSize } false sigexp
        | Transparent sigexp => " : " ^ formatSigexp { indent = indent + indentSize } false sigexp

  and formatFsigconst { indent }  sigConst =
      case sigConst of
          NoSig => ""
        | Opaque sigexp => " :> " ^ formatFsigexp { indent = indent + indentSize } sigexp
        | Transparent sigexp => " : " ^ formatFsigexp { indent = indent + indentSize } sigexp

  and formatFsigexp formatInfo fsigexp =
      case fsigexp of
          BaseFsig {param:(symbol option * sigexp) list, result:sigexp} => ""
        | MarkFsig (fsigexp, region) => ""
        | VarFsig sym => ""
        | CommentFsig (comment, sigexp) => ""

  (*
    = DataReplSpec of symbol * path
    | DataSpec of {datatycs:db list, withtycs:tb list}
    | ExceSpec of (symbol * ty option) list
    | FctSpec of (symbol * fsigexp) list
    | IncludeSpec of sigexp
    | MarkSpec of spec * region
    | ShareStrSpec of path list
    | ShareTycSpec of path list
    | StrSpec of (symbol * sigexp * path option) list
    | TycSpec of (symbol * tyvar list * ty option) list * bool
    | ValSpec of (symbol * ty) list
    *)
  and formatSpec (formatInfo as { indent }) spec =
      case spec of
          DataReplSpec (symbol, path) => ""
        | DataSpec {datatycs:db list, withtycs:tb list} => ""
        | ExceSpec exns =>
          "exception "
          ^ String.concat
                (intercalate
                     ("\n" ^ createIndent indent ^ "and ")
                     (List.map
                          (fn (sym, ty) =>
                              Symbol.name sym ^
                              (case ty of
                                   NONE => ""
                                 | SOME ty => " : " ^ formatTy { indent = indent + indentSize } ty))
                          exns))
        | FctSpec fsigexps => ""
        | IncludeSpec sigexp => "include " ^ formatSigexp formatInfo false sigexp
        | MarkSpec (spec, region) => formatSpec formatInfo spec
        | ShareStrSpec paths => ""
        | ShareTycSpec paths => "sharing type "
                                ^ String.concat (intercalate " = " (List.map pathToString paths))
        | StrSpec strs =>
          "structure "
          ^ String.concat
                (intercalate
                     ("\n" ^ createIndent indent ^ "and ")
                     (List.map
                          (fn (sym, sigexp, path) =>
                              Symbol.name sym ^ " : " ^ formatSigexp formatInfo false sigexp ^
                              (case path of
                                   NONE => ""
                                 | SOME str => " = " ^ pathToString str))
                          strs))
        | TycSpec (tys, b) =>
          "type "
          ^ String.concat
                (intercalate
                     ("\n" ^ createIndent indent ^ "and ")
                     (List.map
                          (fn (sym, tyvars, ty) =>
                              formatTyvars formatInfo tyvars ^ Symbol.name sym ^
                              (case ty of
                                   NONE => ""
                                 | SOME ty => " = " ^ formatTy { indent = indent + indentSize } ty))
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
        | CommentSpec (comment, spec) => ""

  and formatDec (formatInfo as { indent }) dec =
      case dec of
          AbsDec strbs => ""
        | AbstypeDec {abstycs:db list, body:dec, withtycs:tb list} => ""
        | DataReplDec (sym, path) => ""
        | DatatypeDec {datatycs:db list, withtycs:tb list} => ""
        | DoDec exp => ""
        | ExceptionDec ebs => ""
        | FctDec fctbs =>
          (* Can you have multiple fctbs here?? *)
          "functor " ^ String.concat (intercalate "\n\nand " (List.map (formatFctb formatInfo) fctbs))
        | SigDec sigbs =>
          "signature " ^ String.concat (intercalate "\n\nand " (List.map (formatSigb formatInfo) sigbs))
        | StrDec strbs =>
          (* Can you have multiple strbs here?? *)
          "structure " ^ String.concat (intercalate "\n\nand " (List.map (formatStrb formatInfo) strbs))
        | FixDec {fixity:fixity, ops:symbol list} => ""
        | FsigDec fsigbs => ""
        | FunDec (fbs, tyvars) =>
          let
            val fbs =
                case List.rev fbs of
                    [] => []
                  | x::xs  =>
                    List.rev ((true, formatFb formatInfo x)::(List.map (fn x => (false, formatFb formatInfo x)) xs))
          in
            "fun "
            ^ (String.concat
                   (intercalate
                        ("\n" ^ (createIndent indent) ^ "and ")
                        (List.map
                             (fn (last, fb) => if not last andalso shouldNewline fb then fb ^ "\n" else fb)
                             fbs)))
          end
        | LocalDec (d1, d2) =>
          "local "
          ^ "\n " ^ (createIndent (indent + indentSize))
          ^ formatDec { indent = indent + indentSize } d1
          ^ "\n" ^ (createIndent indent) ^ "in"
          ^ "\n " ^ (createIndent (indent + indentSize))
          ^ formatDec { indent = indent + indentSize } d2
          ^ "\n" ^ (createIndent indent) ^ "end"
        | MarkDec (dec, region) => formatDec formatInfo dec
        | OpenDec paths => "open " ^ (String.concat (intercalate " " (List.map pathToString paths)))
        | OvldDec (symbol, ty, exps) => ""
        | SeqDec [] => ""
        | SeqDec decs =>
          let
            val decs =
                case List.rev decs of
                    [] => []
                 | x::xs =>
                   List.rev ((true, formatDec formatInfo x)::
                             (List.map (fn x => (false, formatDec formatInfo x)) xs))
          in
            String.concat
                (intercalate
                     ("\n" ^ (createIndent indent))
                     (List.map (fn (last, dec) => if not last andalso shouldNewline dec
                                                  then dec ^ "\n"
                                                  else dec)
                               decs))
          end
        | TypeDec tbs => "type " ^
                         String.concat
                             (intercalate
                                  ("\n" ^ (createIndent indent) ^ "and ")
                                  (List.map (formatTb formatInfo) tbs))
        | ValDec (vbs, tyvars) =>
          "val " ^ String.concat
                       (intercalate
                            ("\n" ^ (createIndent indent) ^ "and ")
                            (List.map (formatVb formatInfo) vbs))
        (* Deal with tyvars here and in fun *)
        | ValrecDec (rvbs, tyvars) => "val rec "
                                      ^ String.concat
                                            (intercalate
                                                 ("\n\n" ^ (createIndent indent) ^ "and ")
                                                 (List.map (formatRvb formatInfo) rvbs))
        | CommentDec (comment, dec) =>
          comment ^ "\n" ^ (createIndent indent) ^ formatDec formatInfo dec

  and formatVb (formatInfo as { indent }) vb =
      case vb of
          MarkVb (vb, region) => formatVb formatInfo vb
        | CommentVb (coment, vb) => ""
        | Vb {exp:exp, lazyp:bool, pat:pat} =>
          let
            val formattedExp = formatExp { indent = indent + indentSize } exp
            val formattedPat = formatPat formatInfo false pat
            val oneLine = formattedPat ^ " = " ^ formattedExp
          in
            if shouldNewline oneLine
            then formattedPat ^ " =\n" ^ (createIndent (indent + indentSize)) ^ formattedExp
            else oneLine
          end

  and formatRvb (formatInfo as { indent }) rvb =
      case rvb of
          CommentRvb (comment, rvb) => ""
        | MarkRvb (rvb, region) => formatRvb formatInfo rvb
        | Rvb {exp:exp, fixity:(symbol * region) option, lazyp:bool,
               resultty:ty option, var:symbol} =>
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
    | formatFb formatInfo (CommentFb (comment, fb)) = ""

  and formatClause (formatInfo as { indent }) (Clause {exp:exp, pats:pat fixitem list, resultty:ty option}) =
      let
        val formattedExp = formatExp { indent = indent + indentSize } exp
        val formattedPats =
            String.concat
                (intercalate
                     " "
                     (List.map (fn pat => formatPat formatInfo true (#item pat)) pats))
        val sep =
            (* Tecnically wrong but redo later *)
            if shouldNewline (formattedPats ^ formattedExp)
            then " =\n" ^ (createIndent (indent + indentSize))
            else " = "
        val resultTy =
            case resultty of
                NONE => ""
              | SOME ty => " : " ^ formatTy formatInfo ty
      in
        formattedPats ^ resultTy ^ sep ^ formattedExp
      end


  and formatTyvars formatInfo tyvars =
      case tyvars of
          [] => ""
        | [ tyvar ] => formatTyvar formatInfo tyvar ^ " "
        | _ =>  "(" ^ String.concat (intercalate ", " (List.map (formatTyvar formatInfo) tyvars)) ^ ") "

  and formatTb formatInfo (MarkTb (tb, region)) = formatTb formatInfo tb
    | formatTb formatInfo (Tb {def:ty, tyc:symbol, tyvars:tyvar list}) =
      let
        val tyvars = formatTyvars formatInfo tyvars
      in
        (* Add a shouldNewline *)
        tyvars ^ Symbol.name tyc ^ " = " ^ formatTy formatInfo def
      end
    | formatTb formatInfo (CommentTb (comment, tb)) = ""

  and formatDb formatInfo (Db {lazyp:bool, rhs:(symbol * ty option) list, tyc:symbol,
                               tyvars:tyvar list}) = ""
    | formatDb formatInfo (MarkDb (db, region)) = ""
    | formatDb formatInfo (CommentDb (comment, db)) = ""

  and formatEb formatInfo eb =
      case eb of
          EbDef {edef:path, exn:symbol} => ""
        | EbGen {etype:ty option, exn:symbol} => ""
        | MarkEb (eb, region) => ""
        | CommentEb (comment, eb) => ""

  and formatStrb formatInfo (MarkStrb (strb, region)) = formatStrb formatInfo strb
    | formatStrb (formatInfo as { indent }) (Strb {constraint:sigexp sigConst, def:strexp, name:symbol}) =
      let
        val constraint =
            case constraint of
                NoSig => ""
              | Opaque sigexp => " :> " ^ formatSigexp { indent = indent + indentSize } false sigexp
              | Transparent sigexp => " : " ^ formatSigexp { indent = indent + indentSize } false sigexp
      in
        Symbol.name name ^ constraint ^ " = " ^ formatStrexp { indent = indent + indentSize } def
      end
    | formatStrb formatInfo (CommentStrb (comment, strb)) = ""

  and formatFctb formatInfo (Fctb {def:fctexp, name:symbol}) =
      Symbol.name name ^ " " ^ formatFctexp formatInfo def
    | formatFctb formatInfo (MarkFctb (fctb, region)) = formatFctb formatInfo fctb
    | formatFctb formatInfo (CommentFctb (comment, strb)) = ""

  and formatSigb formatInfo (MarkSigb (sigb, region)) = formatSigb formatInfo sigb
    | formatSigb formatInfo (Sigb {def:sigexp, name:symbol}) =
      Symbol.name name ^ " = " ^ formatSigexp formatInfo false def
    | formatSigb formatInfo (CommentSigb (comment, sigb)) = ""

  and formatFsigb formatInfo (Fsigb {def:fsigexp, name:symbol}) = ""
    | formatFsigb formatInfo (MarkFsigb (fsigb, region)) = ""
    | formatFsigb formatInfo (CommentFsigb (comment, fsigb)) = ""

  and formatTyvar formatInfo (MarkTyv (tyvar, region)) = formatTyvar formatInfo tyvar
    | formatTyvar formatInfo (Tyv sym) = Symbol.name sym
    | formatTyvar formatInfo (CommentTyv (comment, tyvar)) = ""

  and formatTy formatInfo ty =
      case ty of
          ConTy (syms, []) => pathToString syms
        | ConTy (syms, [ arg ]) =>
          formatTy formatInfo arg ^ " " ^ pathToString syms
        | ConTy ([ sym ], tys) =>
          (* Fix the parens here *)
          if Symbol.name sym = "->"
          then "(" ^ String.concat (intercalate " -> " (List.map (formatTy formatInfo) tys)) ^ ")"
          else "(" ^ String.concat (intercalate ", " (List.map (formatTy formatInfo) tys)) ^ ") "
        | ConTy (syms, tys) =>
          "(" ^ String.concat (intercalate ", " (List.map (formatTy formatInfo) tys)) ^ ") "
          ^ pathToString syms
        | MarkTy (ty, region) => formatTy formatInfo ty
        | RecordTy tys =>
          "{" ^
          String.concat
              (intercalate
                   ", "
                   (List.map (fn (sym, ty) => Symbol.name sym ^ " : " ^ formatTy formatInfo ty) tys))
          ^ "}"
        | TupleTy tys =>
          "(" ^ String.concat (intercalate " * " (List.map (formatTy formatInfo) tys)) ^ ")"
        | VarTy tyvar => formatTyvar formatInfo tyvar
        | CommentTy (comment, ty) => ""

end
