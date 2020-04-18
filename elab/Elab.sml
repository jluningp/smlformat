structure Elab : ELAB = struct
  open ElabAst

  type conversionInfo =
    { sourceMap : SourceMap.sourcemap
    , comments : (int * string list) list ref
    , fixity : Fixity.fixity StringMap.map ref }

  (* Comments should be sorted in descending order by line *)
  fun getLine sourceMap (left, right) =
      let
        val regions = SourceMap.fileregion sourceMap (left, right)
        val lines = List.map (fn (left, _) => #line left) regions
      in
        List.foldl Int.max 0 lines
      end

  fun mapFixitem f { fixity, item, region } =
      { fixity = fixity, item = f item, region = region }

  fun mapSigconst f Ast.NoSig = NoSig
    | mapSigconst f (Ast.Opaque x) = Opaque (f x)
    | mapSigconst f (Ast.Transparent x) = Transparent (f x)

  fun intercalate sep [] = []
    | intercalate sep [ x ] = [ x ]
    | intercalate sep (x :: xs) = x :: sep :: intercalate sep xs

  fun addComment (conversionInfo : conversionInfo) value convert comment mark region
      =
      let
        val line = getLine (#sourceMap conversionInfo) region
        val comments = #comments conversionInfo

        val attachedComments =
          List.filter
            (fn (commentLine, comments) => line >= commentLine)
            (! comments)

        val remainingComments =
          List.filter (fn (commentLine, comments) => line < commentLine) (! comments)

        val () = comments := remainingComments
        val converted = convert conversionInfo value
      in
        case attachedComments of
            [] => mark (converted, region)
          | comments =>
              comment
                (List.rev (List.map (fn (_, x) => String.concat x) comments)
                , mark (converted, region))
      end

  fun convertExp (conversionInfo : conversionInfo) exp =
      case exp of
          Ast.AndalsoExp (e1, e2) =>
            AndalsoExp (convertExp conversionInfo e1, convertExp conversionInfo e2)
        | Ast.AppExp { argument : Ast.exp, function : Ast.exp } =>
            AppExp
              { argument = convertExp conversionInfo argument
              , function = convertExp conversionInfo function }
        | Ast.CaseExp { expr : Ast.exp, rules : Ast.rule list } =>
            CaseExp
              { expr = convertExp conversionInfo expr
              , rules = List.map (convertRule conversionInfo) rules }
        | Ast.CharExp str => CharExp str
        | Ast.ConstraintExp { constraint : Ast.ty, expr : Ast.exp } =>
            ConstraintExp
              { constraint = convertTy conversionInfo constraint
              , expr = convertExp conversionInfo expr }
        | Ast.FlatAppExp exps =>
            FixAppExp
              (FixityParser.map
                 (fn exp => convertExp conversionInfo (#item exp))
                 (FixityParser.parse (! (#fixity conversionInfo)) exps))
        | Ast.FnExp rules => FnExp (List.map (convertRule conversionInfo) rules)
        | Ast.HandleExp { expr : Ast.exp, rules : Ast.rule list } =>
            HandleExp
              { expr = convertExp conversionInfo expr
              , rules = List.map (convertRule conversionInfo) rules }
        | Ast.IfExp { elseCase : Ast.exp, test : Ast.exp, thenCase : Ast.exp } =>
            IfExp
              { elseCase = convertExp conversionInfo elseCase
              , test = convertExp conversionInfo test
              , thenCase = convertExp conversionInfo thenCase }
        | Ast.IntExp literal => IntExp (ExtractLiteral.extractLiteral literal)
        | Ast.LetExp { dec : Ast.dec, expr : Ast.exp } =>
            LetExp
              { dec = convertDec conversionInfo dec, expr = convertExp conversionInfo expr }
        | Ast.ListExp exps => ListExp (List.map (convertExp conversionInfo) exps)
        | Ast.OrelseExp (e1, e2) =>
            OrelseExp (convertExp conversionInfo e1, convertExp conversionInfo e2)
        | Ast.RaiseExp e => RaiseExp (convertExp conversionInfo e)
        | Ast.RealExp s => RealExp (ExtractLiteral.extractRealLiteral s)
        | Ast.RecordExp record =>
            RecordExp (List.map (fn (s, e) => (s, convertExp conversionInfo e)) record)
        | Ast.SelectorExp sym => SelectorExp sym
        | Ast.SeqExp exps => SeqExp (List.map (convertExp conversionInfo) exps)
        | Ast.StringExp str => StringExp str
        | Ast.TupleExp exps => TupleExp (List.map (convertExp conversionInfo) exps)
        | Ast.VarExp path => VarExp path
        | Ast.VectorExp exps => VectorExp (List.map (convertExp conversionInfo) exps)
        | Ast.WhileExp { expr : Ast.exp, test : Ast.exp } =>
            WhileExp
              { expr = convertExp conversionInfo expr, test = convertExp conversionInfo expr }
        | Ast.WordExp literal => WordExp (ExtractLiteral.extractLiteral literal)
        | Ast.MarkExp (exp, region) =>
            addComment conversionInfo exp convertExp CommentExp MarkExp region

  and convertRule conversionInfo (Ast.Rule { exp : Ast.exp, pat : Ast.pat }) =
      Rule
        { exp = convertExp conversionInfo exp, pat = convertPat conversionInfo pat }

  and convertPat conversionInfo pat =
      case pat of
          Ast.AppPat { argument : Ast.pat, constr : Ast.pat } =>
            AppPat
              { argument = convertPat conversionInfo argument
              , constr = convertPat conversionInfo constr }
        | Ast.CharPat str => CharPat str
        | Ast.ConstraintPat { constraint : Ast.ty, pattern : Ast.pat } =>
            ConstraintPat
              { constraint = convertTy conversionInfo constraint
              , pattern = convertPat conversionInfo pattern }
        | Ast.FlatAppPat pats =>
            FlatAppPat (List.map (mapFixitem (convertPat conversionInfo)) pats)
        | Ast.IntPat l => IntPat (ExtractLiteral.extractLiteral l)
        | Ast.LayeredPat { expPat : Ast.pat, varPat : Ast.pat } =>
            LayeredPat
              { expPat = convertPat conversionInfo expPat
              , varPat = convertPat conversionInfo varPat }
        | Ast.ListPat pats => ListPat (List.map (convertPat conversionInfo) pats)
        | Ast.OrPat pats => OrPat (List.map (convertPat conversionInfo) pats)
        | Ast.RecordPat { def : (symbol * Ast.pat) list, flexibility : bool } =>
            RecordPat
              { def = List.map (fn (sym, pat) => (sym, convertPat conversionInfo pat)) def
              , flexibility = flexibility }
        | Ast.StringPat str => StringPat str
        | Ast.TuplePat pats => TuplePat (List.map (convertPat conversionInfo) pats)
        | Ast.VarPat path => VarPat path
        | Ast.VectorPat pats => VectorPat (List.map (convertPat conversionInfo) pats)
        | Ast.WildPat => WildPat
        | Ast.WordPat l => WordPat (ExtractLiteral.extractLiteral l)
        | Ast.MarkPat (pat, region) =>
            addComment conversionInfo pat convertPat CommentPat MarkPat region

  and convertStrexp conversionInfo strexp =
      case strexp of
          Ast.AppStr (path, strexps) =>
            AppStr
              (path
              , List.map
                (fn (strexp, bool) => (convertStrexp conversionInfo strexp, bool))
                strexps)
        | Ast.AppStrI (path, strexps) =>
            AppStrI
              (path
              , List.map
                (fn (strexp, bool) => (convertStrexp conversionInfo strexp, bool))
                strexps)
        | Ast.BaseStr dec => BaseStr (convertDec conversionInfo dec)
        | Ast.ConstrainedStr (strexp, sigConst) =>
            ConstrainedStr
              (convertStrexp conversionInfo strexp
              , mapSigconst (convertSigexp conversionInfo) sigConst)
        | Ast.LetStr (dec, strexp) =>
            LetStr (convertDec conversionInfo dec, convertStrexp conversionInfo strexp)
        | Ast.MarkStr (strexp, region) =>
            addComment conversionInfo strexp convertStrexp CommentStr MarkStr region
        | Ast.VarStr path => VarStr path

  and convertFctexp conversionInfo fctexp =
      case fctexp of
          Ast.AppFct (path, strexps, sigConst) =>
            AppFct
              (path
              , List.map
                (fn (strexp, bool) => (convertStrexp conversionInfo strexp, bool))
                strexps
              , mapSigconst (convertFsigexp conversionInfo) sigConst)
        | Ast.BaseFct { body : Ast.strexp
          , constraint : Ast.sigexp Ast.sigConst
          , params : (symbol option * Ast.sigexp) list } =>
            BaseFct
              { body = convertStrexp conversionInfo body
              , constraint = mapSigconst (convertSigexp conversionInfo) constraint
              , params = List.map
                (fn (sym, sigexp) => (sym, convertSigexp conversionInfo sigexp))
                params }
        | Ast.LetFct (dec, fctexp) =>
            LetFct (convertDec conversionInfo dec, convertFctexp conversionInfo fctexp)
        | Ast.MarkFct (fctexp, region) =>
            addComment conversionInfo fctexp convertFctexp CommentFct MarkFct region
        | Ast.VarFct (path, sigConst) =>
            VarFct (path, mapSigconst (convertFsigexp conversionInfo) sigConst)

  and convertWherespec conversionInfo (Ast.WhStruct x) = WhStruct x
    | convertWherespec conversionInfo (Ast.WhType (sym, tyvar, ty)) =
      WhType
        (sym, List.map (convertTyvar conversionInfo) tyvar, convertTy conversionInfo ty)

  and convertSigexp conversionInfo sigexp =
      case sigexp of
          Ast.AugSig (sigexp, wherespecs) =>
            AugSig
              (convertSigexp conversionInfo sigexp
              , List.map (convertWherespec conversionInfo) wherespecs)
        | Ast.BaseSig specs => BaseSig (List.map (convertSpec conversionInfo) specs)
        | Ast.MarkSig (sigexp, region) =>
            addComment conversionInfo sigexp convertSigexp CommentSig MarkSig region
        | Ast.VarSig sym => VarSig sym

  and convertFsigexp conversionInfo fsigexp =
      case fsigexp of
          Ast.BaseFsig { param : (symbol option * Ast.sigexp) list, result : Ast.sigexp } =>
            BaseFsig
              { param = List.map
                (fn (sym, sigexp) => (sym, convertSigexp conversionInfo sigexp))
                param
              , result = convertSigexp conversionInfo result }
        | Ast.MarkFsig (fsigexp, region) =>
            addComment
              conversionInfo
              fsigexp
              convertFsigexp
              CommentFsig
              MarkFsig
              region
        | Ast.VarFsig sym => VarFsig sym

  and convertSpec conversionInfo spec =
      case spec of
          Ast.DataReplSpec (symbol, path) => DataReplSpec (symbol, path)
        | Ast.DataSpec { datatycs : Ast.db list, withtycs : Ast.tb list } =>
            DataSpec
              { datatycs = List.map (convertDb conversionInfo) datatycs
              , withtycs = List.map (convertTb conversionInfo) withtycs }
        | Ast.ExceSpec tys =>
            ExceSpec
              (List.map
                 (fn (sym, ty) => (sym, Option.map (convertTy conversionInfo) ty))
                 tys)
        | Ast.FctSpec fsigexps =>
            FctSpec
              (List.map
                 (fn (sym, fsigexp) => (sym, convertFsigexp conversionInfo fsigexp))
                 fsigexps)
        | Ast.IncludeSpec sigexp => IncludeSpec (convertSigexp conversionInfo sigexp)
        | Ast.MarkSpec (spec, region) =>
            addComment conversionInfo spec convertSpec CommentSpec MarkSpec region
        | Ast.ShareStrSpec paths => ShareStrSpec paths
        | Ast.ShareTycSpec paths => ShareTycSpec paths
        | Ast.StrSpec strs =>
            StrSpec
              (List.map
                 (fn (sym, sigexp, path) => (sym, convertSigexp conversionInfo sigexp, path))
                 strs)
        | Ast.TycSpec (tys, b) =>
            TycSpec
              (List.map
                (fn (sym, tyvars, ty) =>
                    (sym
                    , List.map (convertTyvar conversionInfo) tyvars
                    , Option.map (convertTy conversionInfo) ty))
                tys
              , b)
        | Ast.ValSpec tys =>
            ValSpec (List.map (fn (sym, ty) => (sym, convertTy conversionInfo ty)) tys)

  and convertDec conversionInfo dec =
      case dec of
          Ast.AbsDec strbs => AbsDec (List.map (convertStrb conversionInfo) strbs)
        | Ast.AbstypeDec { abstycs : Ast.db list, body : Ast.dec, withtycs : Ast.tb list } =>
            AbstypeDec
              { abstycs = List.map (convertDb conversionInfo) abstycs
              , body = convertDec conversionInfo dec
              , withtycs = List.map (convertTb conversionInfo) withtycs }
        | Ast.DataReplDec (sym, path) => DataReplDec (sym, path)
        | Ast.DatatypeDec { datatycs : Ast.db list, withtycs : Ast.tb list } =>
            DatatypeDec
              { datatycs = List.map (convertDb conversionInfo) datatycs
              , withtycs = List.map (convertTb conversionInfo) withtycs }
        | Ast.DoDec exp => DoDec (convertExp conversionInfo exp)
        | Ast.ExceptionDec ebs =>
            ExceptionDec (List.map (convertEb conversionInfo) ebs)
        | Ast.FctDec fctbs => FctDec (List.map (convertFctb conversionInfo) fctbs)
        | Ast.FixDec { fixity : fixity, ops : symbol list } =>
            let
              val fixitymap = #fixity conversionInfo
            in
              ((List.app
                  (fn sym =>
                      Ref.modify
                        (fn map => StringMap.insert (map, Symbol.name sym, fixity))
                        fixitymap)
                  ops);
               print
               FixDec { fixity = fixity, ops = ops })
            end
        | Ast.FsigDec fsigbs =>
            FsigDec (List.map (convertFsigb conversionInfo) fsigbs)
        | Ast.FunDec (fbs, tyvars) =>
            FunDec
              (List.map (convertFb conversionInfo) fbs
              , List.map (convertTyvar conversionInfo) tyvars)
        | Ast.LocalDec (d1, d2) =>
            LocalDec (convertDec conversionInfo d1, convertDec conversionInfo d2)
        | Ast.OpenDec paths => OpenDec paths
        | Ast.OvldDec (symbol, ty, exps) =>
            OvldDec
              (symbol, convertTy conversionInfo ty, List.map (convertExp conversionInfo) exps)
        | Ast.SeqDec decs =>
            let
              (* We make a new ref here so that fixity declarations don't escape their scope. *)
              val conversionInfo =
                { sourceMap = #sourceMap conversionInfo
                , comments = #comments conversionInfo
                , fixity = ref (! (#fixity conversionInfo)) }
            in
              SeqDec (List.map (convertDec conversionInfo) decs)
            end
        | Ast.SigDec sigbs => SigDec (List.map (convertSigb conversionInfo) sigbs)
        | Ast.StrDec strbs => StrDec (List.map (convertStrb conversionInfo) strbs)
        | Ast.TypeDec tbs => TypeDec (List.map (convertTb conversionInfo) tbs)
        | Ast.ValDec (vbs, tyvars) =>
            ValDec
              (List.map (convertVb conversionInfo) vbs
              , List.map (convertTyvar conversionInfo) tyvars)
        | Ast.ValrecDec (rvbs, tyvars) =>
            ValrecDec
              (List.map (convertRvb conversionInfo) rvbs
              , List.map (convertTyvar conversionInfo) tyvars)
        | Ast.MarkDec (dec, region) =>
            addComment conversionInfo dec convertDec CommentDec MarkDec region

  and convertVb conversionInfo vb =
      case vb of
          Ast.MarkVb (vb, region) =>
            addComment conversionInfo vb convertVb CommentVb MarkVb region
        | Ast.Vb { exp : Ast.exp, lazyp : bool, pat : Ast.pat } =>
            Vb
              { exp = convertExp conversionInfo exp
              , lazyp = lazyp
              , pat = convertPat conversionInfo pat }

  and convertRvb conversionInfo rvb =
      case rvb of
          Ast.MarkRvb (rvb, region) =>
            addComment conversionInfo rvb convertRvb CommentRvb MarkRvb region
        | Ast.Rvb { exp : Ast.exp
          , fixity : (symbol * region) option
          , lazyp : bool
          , resultty : Ast.ty option
          , var : symbol } =>
            Rvb
              { exp = convertExp conversionInfo exp
              , fixity = fixity
              , lazyp = lazyp
              , resultty = Option.map (convertTy conversionInfo) resultty
              , var = var }

  and convertFb conversionInfo (Ast.Fb (clauses, b)) =
      Fb (List.map (convertClause conversionInfo) clauses, b)
    | convertFb conversionInfo (Ast.MarkFb (fb, region)) =
      addComment conversionInfo fb convertFb CommentFb MarkFb region

  and convertClause
      conversionInfo
      (Ast.Clause { exp : Ast.exp, pats : Ast.pat Ast.fixitem list, resultty : Ast.ty option })
      =
      Clause
        { exp = convertExp conversionInfo exp
        , pats = List.map (mapFixitem (convertPat conversionInfo)) pats
        , resultty = Option.map (convertTy conversionInfo) resultty }

  and convertTb conversionInfo (Ast.MarkTb (tb, region)) =
      addComment conversionInfo tb convertTb CommentTb MarkTb region
    | convertTb
      conversionInfo
      (Ast.Tb { def : Ast.ty, tyc : symbol, tyvars : Ast.tyvar list })
      =
      Tb
        { def = convertTy conversionInfo def
        , tyc = tyc
        , tyvars = List.map (convertTyvar conversionInfo) tyvars }

  and convertDb
      conversionInfo
      (Ast.Db { lazyp : bool
    , rhs : (symbol * Ast.ty option) list
    , tyc : symbol
    , tyvars : Ast.tyvar list })
      =
      Db
        { lazyp = lazyp
        , rhs = List.map
          (fn (sym, ty) => (sym, Option.map (convertTy conversionInfo) ty))
          rhs
        , tyc = tyc
        , tyvars = List.map (convertTyvar conversionInfo) tyvars }
    | convertDb conversionInfo (Ast.MarkDb (db, region)) =
      addComment conversionInfo db convertDb CommentDb MarkDb region

  and convertEb conversionInfo eb =
      case eb of
          Ast.EbDef { edef : path, exn : symbol } => EbDef { edef = edef, exn = exn }
        | Ast.EbGen { etype : Ast.ty option, exn : symbol } =>
            EbGen { etype = Option.map (convertTy conversionInfo) etype, exn = exn }
        | Ast.MarkEb (eb, region) =>
            addComment conversionInfo eb convertEb CommentEb MarkEb region

  and convertStrb conversionInfo (Ast.MarkStrb (strb, region)) =
      addComment conversionInfo strb convertStrb CommentStrb MarkStrb region
    | convertStrb
      conversionInfo
      (Ast.Strb { constraint : Ast.sigexp Ast.sigConst, def : Ast.strexp, name : symbol })
      =
      Strb
        { constraint = mapSigconst (convertSigexp conversionInfo) constraint
        , def = convertStrexp conversionInfo def
        , name = name }

  and convertFctb conversionInfo (Ast.Fctb { def : Ast.fctexp, name : symbol }) =
      Fctb { def = convertFctexp conversionInfo def, name = name }
    | convertFctb conversionInfo (Ast.MarkFctb (fctb, region)) =
      addComment conversionInfo fctb convertFctb CommentFctb MarkFctb region

  and convertSigb conversionInfo (Ast.MarkSigb (sigb, region)) =
      addComment conversionInfo sigb convertSigb CommentSigb MarkSigb region
    | convertSigb conversionInfo (Ast.Sigb { def : Ast.sigexp, name : symbol }) =
      Sigb { def = convertSigexp conversionInfo def, name = name }

  and convertFsigb conversionInfo (Ast.Fsigb { def : Ast.fsigexp, name : symbol })
      =
      Fsigb { def = convertFsigexp conversionInfo def, name = name }
    | convertFsigb conversionInfo (Ast.MarkFsigb (fsigb, region)) =
      addComment conversionInfo fsigb convertFsigb CommentFsigb MarkFsigb region

  and convertTyvar conversionInfo (Ast.MarkTyv (tyvar, region)) =
      addComment conversionInfo tyvar convertTyvar CommentTyv MarkTyv region
    | convertTyvar conversionInfo (Ast.Tyv sym) = Tyv sym

  and convertTy conversionInfo ty =
      case ty of
          Ast.ConTy (syms, tys) =>
            ConTy (syms, List.map (convertTy conversionInfo) tys)
        | Ast.MarkTy (ty, region) =>
            addComment conversionInfo ty convertTy CommentTy MarkTy region
        | Ast.RecordTy tys =>
            RecordTy
              (List.map (fn (sym, ty) => (sym, convertTy conversionInfo ty)) tys)
        | Ast.TupleTy tys => TupleTy (List.map (convertTy conversionInfo) tys)
        | Ast.VarTy tyvar => VarTy (convertTyvar conversionInfo tyvar)

  val elaborate = convertDec
end
