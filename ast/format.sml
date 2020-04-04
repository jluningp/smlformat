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

  fun formatExp (formatInfo as { indent }) exp =
      case exp of
        AndalsoExp (e1, e2) => formatExp formatInfo e1 ^ " andalso " ^ formatExp formatInfo e2
      | AppExp {argument:exp, function:exp} =>
          formatExp formatInfo function ^ " " ^ formatExp formatInfo argument
        | CaseExp {expr:exp, rules:rule list} =>
          "case " ^ formatExp formatInfo expr ^ " of\n"
          ^ (createIndent (indent + indentSize))
          ^ formatRules formatInfo rules
        | CharExp str => "#" ^ str
        | ConstraintExp {constraint:ty, expr:exp} =>
          formatExp formatInfo expr ^ " : " ^ formatTy formatInfo constraint
        | FlatAppExp exps =>
          String.concat (intercalate " " (List.map (fn exp => formatExp formatInfo (#item exp)) exps))
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
        | OrelseExp (e1, e2) => ""
        | RaiseExp e => ""
        | RealExp s => s
        | RecordExp record => ""
        | SelectorExp sym => ""
        | SeqExp exps => "(" ^ String.concat (intercalate "; " (List.map (formatExp formatInfo) exps)) ^ ")"
        | StringExp str => "\"" ^  str ^ "\""
        | TupleExp exps => "(" ^ String.concat (intercalate ", " (List.map (formatExp formatInfo) exps)) ^ ")"
        | VarExp path => String.concat (intercalate "." (List.map Symbol.name path))
        | VectorExp exps => ""
        | WhileExp {expr:exp, test:exp} => ""
        | WordExp literal => IntInf.toString literal

  and formatRules { indent } rules =
      String.concat
          (intercalate
               ("\n" ^ createIndent (Int.max (0, indent + indentSize - 2)) ^ "| ")
               (List.map (formatRule { indent = indent + indentSize }) rules))


  and formatRule formatInfo (Rule {exp:exp, pat:pat}) =
      let
        val formattedExp = formatExp formatInfo exp
        val sep =
            if String.isSubstring "\n" formattedExp orelse String.size formattedExp > characters
            then " =>\n" ^ (createIndent (#indent formatInfo + indentSize))
            else " => "
      in
        formatPat formatInfo pat ^ sep ^ formattedExp
      end

  and formatPat formatInfo pat =
      case pat of
          AppPat {argument:pat, constr:pat} => formatPat formatInfo constr ^ " " ^ formatPat formatInfo argument
        | CharPat str => "#" ^ str
        | ConstraintPat {constraint:ty, pattern:pat} => ""
        | FlatAppPat pats =>
          String.concat (intercalate " " (List.map (fn pat => formatPat formatInfo (#item pat)) pats))
        | IntPat l => IntInf.toString l
        | LayeredPat {expPat:pat, varPat:pat} => ""
        | ListPat pats => "[" ^ (String.concat (intercalate ", " (List.map (formatPat formatInfo) pats))) ^ "]"
        | MarkPat (pat, region) => formatPat formatInfo pat
        | OrPat pats => ""
        | RecordPat {def:(symbol * pat) list, flexibility:bool} => ""
        | StringPat str => "\"" ^  str ^ "\""
        | TuplePat pats => "(" ^ String.concat (intercalate ", " (List.map (formatPat formatInfo) pats)) ^ ")"
        | VarPat path => String.concat (intercalate "." (List.map Symbol.name path))
        | VectorPat pats => ""
        | WildPat => "_"
        | WordPat l => IntInf.toString l

  and formatStrexp formatInfo strexp =
      case strexp of
          AppStr (path, strexps) => ""
        | AppStrI (path, strexps) => ""
        | BaseStr dec => ""
        | ConstrainedStr (strexp, sigConst) => ""
        | LetStr (dec, strexp) => ""
        | MarkStr (strexp, region) => ""
        | VarStr path => ""

  and formatFctexp formatInfo fctexp =
      case fctexp of
          AppFct (path, strexps, sigConst) => ""
        | BaseFct {body:strexp, constraint:sigexp sigConst,
                       params:(symbol option * sigexp) list} => ""
        | LetFct (dec, fctexp) => ""
        | MarkFct (fctexp, region) => ""
        | VarFct (path, sigConst) => ""

  and formatWherespec formatInfo (WhStruct x) = ""
    | formatWherespec formatInfo (WhType (sym, tyvar, ty)) = ""

  and formatSigexp formatInfo sigexp =
      case sigexp of
          AugSig (sigexp, wherespecs) => ""
        | BaseSig specs => ""
        | MarkSig (sigexp, region) => ""
        | VarSig sym => ""

  and formatFsigexp formatInfo fsigexp =
      case fsigexp of
          BaseFsig {param:(symbol option * sigexp) list, result:sigexp} => ""
        | MarkFsig (fsigexp, region) => ""
        | VarFsig sym => ""

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
        | FunDec (fbs, tyvars) => ""
        | LocalDec (d1, d2) => ""
        | MarkDec (dec, region) => formatDec formatInfo dec
        | OpenDec paths => ""
        | OvldDec (symbol, ty, exps) => ""
        | SeqDec decs =>
          String.concat (intercalate
                             ("\n" ^ (createIndent indent))
                             (List.map (formatDec formatInfo) decs))
        | SigDec sigbs => ""
        | StrDec strbs => ""
        | TypeDec tbs => ""
        | ValDec (vbs, tyvars) =>
          "val " ^ String.concat (intercalate "\n\nand" (List.map (formatVb formatInfo) vbs))
        | ValrecDec (rvbs, tyvars) => ""
        | CommentDec (comment, dec) => comment ^ "\n" ^ formatDec formatInfo dec

  and formatVb formatInfo vb =
      case vb of
          MarkVb (vb, region) => formatVb formatInfo vb
        | Vb {exp:exp, lazyp:bool, pat:pat} =>
          let
            val formattedExp = formatExp formatInfo exp
            val sep =
                if String.isSubstring "\n" formattedExp orelse String.size formattedExp > characters
                then " =\n" ^ (createIndent (#indent formatInfo + indentSize))
                else " = "
          in
            formatPat formatInfo pat ^ sep ^ formatExp { indent = (#indent formatInfo) + indentSize}  exp
          end

  and formatRvb formatInfo rvb =
      case rvb of
          MarkRvb (rvb, region) => ""
        | Rvb {exp:exp, fixity:(symbol * region) option, lazyp:bool,
               resultty:ty option, var:symbol} => ""

  and formatFb formatInfo (Fb (clauses, b)) = ""
    | formatFb formatInfo (MarkFb (fb, region)) = ""

  and formatClause formatInfo (Clause {exp:exp, pats:pat fixitem list, resultty:ty option}) = ""

  and formatTb formatInfo (MarkTb (tb, region)) = ""
    | formatTb formatInfo (Tb {def:ty, tyc:symbol, tyvars:tyvar list}) = ""

  and formatDb formatInfo (Db {lazyp:bool, rhs:(symbol * ty option) list, tyc:symbol,
                               tyvars:tyvar list}) = ""
    | formatDb formatInfo (MarkDb (db, region)) = ""

  and formatEb formatInfo eb =
      case eb of
          EbDef {edef:path, exn:symbol} => ""
        | EbGen {etype:ty option, exn:symbol} => ""
        | MarkEb (eb, region) => ""

  and formatStrb formatInfo (MarkStrb (strb, region)) = ""
    | formatStrb formatInfo (Strb {constraint:sigexp sigConst, def:strexp, name:symbol}) = ""

  and formatFctb formatInfo (Fctb {def:fctexp, name:symbol}) = ""
    | formatFctb formatInfo (MarkFctb (fctb, region)) = ""

  and formatSigb formatInfo (MarkSigb (sigb, region)) = ""
    | formatSigb formatInfo (Sigb {def:sigexp, name:symbol}) = ""

  and formatFsigb formatInfo (Fsigb {def:fsigexp, name:symbol}) = ""
    | formatFsigb formatInfo (MarkFsigb (fsigb, region)) = ""

  and formatTyvar formatInfo (MarkTyv (tyvar, region)) = ""
    | formatTyvar formatInfo (Tyv sym) = ""

  and formatTy formatInfo ty =
      case ty of
          ConTy (syms, tys) => ""
        | MarkTy (ty, region) => ""
        | RecordTy tys => ""
        | TupleTy tys => ""
        | VarTy tyvar => ""

end
