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
        AndalsoExp (e1, e2) => formatExp formatInfo e1 ^ " andalso " ^ formatExp formatInfo e2
      | AppExp {argument:exp, function:exp} =>
          formatExp formatInfo function ^ " " ^ formatExp formatInfo argument
        | CaseExp {expr:exp, rules:rule list} =>
          "case " ^ formatExp formatInfo expr ^ " of\n"
          ^ (createIndent (indent + indentSize + 2))
          ^ formatRules { indent = indent + indentSize } rules
        | CharExp str => "#\"" ^ str ^ "\""
        | ConstraintExp {constraint:ty, expr:exp} =>
          formatExp formatInfo expr ^ " : " ^ formatTy formatInfo constraint
        | FlatAppExp exps =>
          (* If it's a single character thing or maybe a symbolic thing, or maybe its fixity is infix,
then  it should be same line as previous thing *)
          let

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
          "let" ^ "\n" ^ (createIndent (indent + indentSize))
          ^ formatDec { indent = indent + indentSize } dec
          ^ "\n" ^ (createIndent indent) ^ "in"
          ^ "\n" ^ (createIndent (indent + indentSize)) ^ formatExp {indent = indent + indentSize } expr
          ^ "\n" ^ (createIndent indent) ^ "end"
        | ListExp exps => "[" ^ String.concat (intercalate ", " (List.map (formatExp formatInfo) exps)) ^ "]"
        | MarkExp (exp, region) => formatExp formatInfo exp
        | OrelseExp (e1, e2) => formatExp formatInfo e1 ^ " orelse " ^ formatExp formatInfo e2
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
        val formattedPat = formatPat formatInfo pat
        val formattedExp = formatExp { indent = indent + indentSize } exp
        val sep =
            if shouldNewline (formattedPat ^ " => " ^ formattedExp)
            then " =>\n" ^ (createIndent (indent + indentSize))
            else " => "
      in
        formatPat formatInfo pat ^ sep ^ formattedExp
      end

  and formatPat formatInfo pat =
      case pat of
          AppPat {argument:pat, constr:pat} => formatPat formatInfo constr ^ " " ^ formatPat formatInfo argument
        | CharPat str => "#" ^ str
        | ConstraintPat {constraint:ty, pattern:pat} =>
          formatPat formatInfo pattern ^ " : " ^ formatTy formatInfo constraint
        | FlatAppPat [ pat ] => formatPat formatInfo (#item pat)
        | FlatAppPat pats =>
          "(" ^ String.concat (intercalate " " (List.map (fn pat => formatPat formatInfo (#item pat)) pats)) ^ ")"
        | IntPat l => IntInf.toString l
        | LayeredPat {expPat:pat, varPat:pat} =>
          "(" ^ formatPat formatInfo varPat ^ " as " ^ formatPat formatInfo expPat ^ ")"
        | ListPat pats => "[" ^ (String.concat (intercalate ", " (List.map (formatPat formatInfo) pats))) ^ "]"
        | MarkPat (pat, region) => formatPat formatInfo pat
        | OrPat pats => String.concat (intercalate " | " (List.map (formatPat formatInfo) pats))
        | RecordPat {def:(symbol * pat) list, flexibility:bool} =>
          let
            fun isVarPat sym pat =
                case pat of
                    CommentPat (_, pat) => isVarPat sym pat
                  | MarkPat (pat, _) => isVarPat sym pat
                  | FlatAppPat [ pat ] => isVarPat sym (#item pat)
                  | VarPat path => Symbol.name sym = pathToString path
                  | _ => false
          in
            "{" ^
            String.concat
                (intercalate
                     ", "
                     (List.map (fn (sym, pat) =>
                                   if isVarPat sym pat
                                   then Symbol.name sym
                                   else Symbol.name sym ^ " = " ^ formatPat formatInfo pat) def))
            ^ "}"
          end
        | StringPat str => "\"" ^  (String.toString str) ^ "\""
        | TuplePat pats => "(" ^ String.concat (intercalate ", " (List.map (formatPat formatInfo) pats)) ^ ")"
        | VarPat path => pathToString path
        | VectorPat pats => "#[" ^ String.concat (intercalate ", " (List.map (formatPat formatInfo) pats)) ^ "]"
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
            if shouldNewline dec
            then "struct\n" ^ (createIndent (indent + indentSize)) ^ dec
                 ^ "\n" ^ (createIndent indent) ^ "end"
            else "struct " ^ dec ^ " end"
          end
        | ConstrainedStr (strexp, sigConst) =>
          (case sigConst of
               NoSig => formatStrexp formatInfo strexp
             | Opaque sigexp => formatStrexp formatInfo strexp ^ " :> " ^ formatSigexp formatInfo sigexp
             | Transparent sigexp => formatStrexp formatInfo strexp ^ " : " ^ formatSigexp formatInfo sigexp)
        | LetStr (dec, strexp) => ""
        | MarkStr (strexp, region) => formatStrexp formatInfo strexp
        | VarStr path => pathToString path
        | CommentStr (comment, strexp) => ""

  and formatFctexp formatInfo fctexp =
      case fctexp of
          AppFct (path, strexps, sigConst) => ""
        | BaseFct {body:strexp, constraint:sigexp sigConst,
                       params:(symbol option * sigexp) list} => ""
        | LetFct (dec, fctexp) => ""
        | MarkFct (fctexp, region) => ""
        | VarFct (path, sigConst) => ""
        | CommentFct (comment, fctexp) => ""

  and formatWherespec formatInfo (WhStruct x) = ""
    | formatWherespec formatInfo (WhType (sym, tyvar, ty)) = ""

  and formatSigexp formatInfo sigexp =
      case sigexp of
          AugSig (sigexp, wherespecs) => ""
        | BaseSig specs => ""
        | MarkSig (sigexp, region) => ""
        | VarSig sym => ""
        | CommentSig (comment, sigexp) => ""

  and formatFsigexp formatInfo fsigexp =
      case fsigexp of
          BaseFsig {param:(symbol option * sigexp) list, result:sigexp} => ""
        | MarkFsig (fsigexp, region) => ""
        | VarFsig sym => ""
        | CommentFsig (comment, sigexp) => ""

  and formatSpec formatInfo spec =
      case spec of
          DataReplSpec (symbol, path) => ""
        | DataSpec {datatycs:db list, withtycs:tb list} => ""
        | ExceSpec tys => ""
        | FctSpec fsigexps => ""
        | IncludeSpec sigexp => ""
        | MarkSpec (spec, region) => ""
        | ShareStrSpec paths => ""
        | ShareTycSpec paths => ""
        | StrSpec strs => ""
        | TycSpec (tys, b) => ""
        | ValSpec tys => ""
        | CommentSpec (comment, spec) => ""

  and formatDec (formatInfo as { indent }) dec =
      case dec of
          AbsDec strbs => ""
        | AbstypeDec {abstycs:db list, body:dec, withtycs:tb list} => ""
        | DataReplDec (sym, path) => ""
        | DatatypeDec {datatycs:db list, withtycs:tb list} => ""
        | DoDec exp => ""
        | ExceptionDec ebs => ""
        | FctDec fctbs => ""
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
        | SigDec sigbs => ""
        | StrDec strbs =>
          (* Can you have multiple strbs here?? *)
          "structure " ^ String.concat (intercalate "\n\nand " (List.map (formatStrb formatInfo) strbs))
        | TypeDec tbs => "type " ^
                         String.concat
                             (intercalate
                                  ("\n" ^ (createIndent indent) ^ "and ")
                                  (List.map (formatTb formatInfo) tbs))
        | ValDec (vbs, tyvars) =>
          "val " ^ String.concat (intercalate "\n\nand " (List.map (formatVb formatInfo) vbs))
        | ValrecDec (rvbs, tyvars) => ""
        | CommentDec (comment, dec) =>
          comment ^ "\n" ^ (createIndent indent) ^ formatDec formatInfo dec

  and formatVb (formatInfo as { indent }) vb =
      case vb of
          MarkVb (vb, region) => formatVb formatInfo vb
        | CommentVb (coment, vb) => ""
        | Vb {exp:exp, lazyp:bool, pat:pat} =>
          let
            val formattedExp = formatExp { indent = indent + indentSize } exp
            val formattedPat = formatPat formatInfo pat
            val oneLine = formattedPat ^ " = " ^ formattedExp
          in
            if shouldNewline oneLine
            then formattedPat ^ " =\n" ^ (createIndent (indent + indentSize)) ^ formattedExp
            else oneLine
          end

  and formatRvb formatInfo rvb =
      case rvb of
          CommentRvb (comment, rvb) => ""
        | MarkRvb (rvb, region) => ""
        | Rvb {exp:exp, fixity:(symbol * region) option, lazyp:bool,
               resultty:ty option, var:symbol} => ""

  and formatFb (formatInfo as { indent }) (Fb (clauses, b)) =
      String.concat
          (intercalate
               ("\n" ^ (createIndent (indent + 2)) ^ "| ")
               (List.map (formatClause { indent = indent + indentSize }) clauses))
    | formatFb formatInfo (MarkFb (fb, region)) = formatFb formatInfo fb
    | formatFb formatInfo (CommentFb (comment, fb)) = ""

  and formatClause (formatInfo as { indent }) (Clause {exp:exp, pats:pat fixitem list, resultty:ty option}) =
      let
        val formattedExp = formatExp formatInfo exp
        val formattedPats =
            String.concat
                (intercalate
                     " "
                     (List.map (fn pat => formatPat formatInfo (#item pat)) pats))
        val sep =
            (* Tecnically wrong but redo later *)
            if shouldNewline (formattedPats ^ formattedExp)
            then " =\n" ^ (createIndent (#indent formatInfo + indentSize))
            else " = "
        val resultTy =
            case resultty of
                NONE => ""
              | SOME ty => " : " ^ formatTy formatInfo ty
      in
        formattedPats ^ resultTy ^ sep ^ formattedExp
      end


  and formatTb formatInfo (MarkTb (tb, region)) = formatTb formatInfo tb
    | formatTb formatInfo (Tb {def:ty, tyc:symbol, tyvars:tyvar list}) =
      let
        val tyvars =
            case tyvars of
                [] => ""
              | [ tyvar ] => formatTyvar formatInfo tyvar ^ " "
              | _ =>  "(" ^ String.concat (intercalate ", " (List.map (formatTyvar formatInfo) tyvars)) ^ ") "

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
              | Opaque sigexp => " :> " ^ formatSigexp { indent = indent + indentSize } sigexp
              | Transparent sigexp => " : " ^ formatSigexp { indent = indent + indentSize } sigexp
      in
        Symbol.name name ^ constraint ^ " = " ^ formatStrexp { indent = indent + indentSize } def
      end
    | formatStrb formatInfo (CommentStrb (comment, strb)) = ""

  and formatFctb formatInfo (Fctb {def:fctexp, name:symbol}) = ""
    | formatFctb formatInfo (MarkFctb (fctb, region)) = ""
    | formatFctb formatInfo (CommentFctb (comment, strb)) = ""

  and formatSigb formatInfo (MarkSigb (sigb, region)) = ""
    | formatSigb formatInfo (Sigb {def:sigexp, name:symbol}) = ""
    | formatSigb formatInfo (CommentSigb (comment, sigb)) = ""

  and formatFsigb formatInfo (Fsigb {def:fsigexp, name:symbol}) = ""
    | formatFsigb formatInfo (MarkFsigb (fsigb, region)) = ""
    | formatFsigb formatInfo (CommentFsigb (comment, fsigb)) = ""

  and formatTyvar formatInfo (MarkTyv (tyvar, region)) = formatTyvar formatInfo tyvar
    | formatTyvar formatInfo (Tyv sym) = Symbol.name sym
    | formatTyvar formatInfo (CommentTyv (comment, tyvar)) = ""

  and formatTy formatInfo ty =
      case ty of
          ConTy (syms, []) => String.concat (intercalate " " (List.map Symbol.name syms))
        | ConTy (syms, [ arg ]) =>
          formatTy formatInfo arg
          ^ " " ^ String.concat (intercalate " " (List.map Symbol.name syms))
        | ConTy (syms, tys) =>
          "(" ^ String.concat (intercalate ", " (List.map (formatTy formatInfo) tys)) ^ ") "
          ^ String.concat (intercalate " " (List.map Symbol.name syms))
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
