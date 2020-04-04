structure AddComments = struct
  open CommentedAst

  fun mapFixitem f {fixity, item, region} =
      {fixity=fixity, item=f item, region=region}

  fun mapSigconst f Ast.NoSig = NoSig
    | mapSigconst f (Ast.Opaque x) = Opaque (f x)
    | mapSigconst f (Ast.Transparent x) = Transparent (f x)

  fun convertExp exp =
      case exp of
          Ast.AndalsoExp (e1, e2) => AndalsoExp (convertExp e1, convertExp e2)
        | Ast.AppExp {argument:Ast.exp, function:Ast.exp} => AppExp {argument=convertExp argument
                                                            , function=convertExp function}
        | Ast.CaseExp {expr:Ast.exp, rules:Ast.rule list} => CaseExp {expr=convertExp expr,
                                                          rules=List.map convertRule rules}
        | Ast.CharExp str => CharExp str
        | Ast.ConstraintExp {constraint:Ast.ty, expr:Ast.exp} => ConstraintExp {constraint=convertTy constraint
                                                                       , expr=convertExp expr}
        | Ast.FlatAppExp exps => FlatAppExp (List.map (mapFixitem convertExp) exps)
        | Ast.FnExp rules => FnExp (List.map convertRule rules)
        | Ast.HandleExp {expr:Ast.exp, rules:Ast.rule list} => HandleExp {expr=convertExp expr
                                                                 , rules=List.map convertRule rules}
        | Ast.IfExp {elseCase:Ast.exp, test:Ast.exp, thenCase:Ast.exp} => IfExp {elseCase=convertExp elseCase
                                                                    , test=convertExp test
                                                                    , thenCase=convertExp thenCase}
        | Ast.IntExp literal => IntExp literal
        | Ast.LetExp {dec:Ast.dec, expr:Ast.exp} => LetExp {dec=convertDec dec
                                                   , expr = convertExp expr}
        | Ast.ListExp exps => ListExp (List.map convertExp exps)
        | Ast.MarkExp (exp, region) => MarkExp (convertExp exp, region)
        | Ast.OrelseExp (e1, e2) => OrelseExp (convertExp e1, convertExp e2)
        | Ast.RaiseExp e => RaiseExp (convertExp e)
        | Ast.RealExp s => RealExp s
        | Ast.RecordExp record => RecordExp (List.map (fn (s, e) => (s, convertExp e)) record)
        | Ast.SelectorExp sym => SelectorExp sym
        | Ast.SeqExp exps => SeqExp (List.map convertExp exps)
        | Ast.StringExp str => StringExp str
        | Ast.TupleExp exps => TupleExp (List.map convertExp exps)
        | Ast.VarExp path => VarExp path
        | Ast.VectorExp exps => VectorExp (List.map convertExp exps)
        | Ast.WhileExp {expr:Ast.exp, test:Ast.exp} => WhileExp {expr=convertExp expr, test=convertExp expr}
        | Ast.WordExp literal => WordExp literal

  and convertRule (Ast.Rule {exp:Ast.exp, pat:Ast.pat}) =
      Rule {exp=convertExp exp, pat=convertPat pat}

  and convertPat pat =
      case pat of
          Ast.AppPat {argument:Ast.pat, constr:Ast.pat} =>
          AppPat {argument=convertPat argument, constr=convertPat constr}
        | Ast.CharPat str => CharPat str
        | Ast.ConstraintPat {constraint:Ast.ty, pattern:Ast.pat} =>
          ConstraintPat {constraint=convertTy constraint
                        , pattern=convertPat pattern}
        | Ast.FlatAppPat pats => FlatAppPat (List.map (mapFixitem convertPat) pats)
        | Ast.IntPat l => IntPat l
        | Ast.LayeredPat {expPat:Ast.pat, varPat:Ast.pat} =>
          LayeredPat {expPat=convertPat expPat, varPat=convertPat varPat}
        | Ast.ListPat pats => ListPat (List.map convertPat pats)
        | Ast.MarkPat (pat, region) => MarkPat (convertPat pat, region)
        | Ast.OrPat pats => OrPat (List.map convertPat pats)
        | Ast.RecordPat {def:(symbol * Ast.pat) list, flexibility:bool} =>
          RecordPat {def = List.map (fn (sym, pat) => (sym, convertPat pat)) def
                    , flexibility = flexibility}
        | Ast.StringPat str => StringPat str
        | Ast.TuplePat pats => TuplePat (List.map convertPat pats)
        | Ast.VarPat path => VarPat path
        | Ast.VectorPat pats => VectorPat (List.map convertPat pats)
        | Ast.WildPat => WildPat
        | Ast.WordPat l => WordPat l

  and convertStrexp strexp =
      case strexp of
          Ast.AppStr (path, strexps) => AppStr (path, List.map (fn (strexp, bool) =>
                                                                   (convertStrexp strexp, bool)) strexps)
        | Ast.AppStrI (path, strexps) => AppStrI (path, List.map (fn (strexp, bool) =>
                                                                     (convertStrexp strexp, bool)) strexps)
        | Ast.BaseStr dec => BaseStr (convertDec dec)
        | Ast.ConstrainedStr (strexp, sigConst) =>
          ConstrainedStr (convertStrexp strexp, mapSigconst convertSigexp sigConst)
        | Ast.LetStr (dec, strexp) => LetStr (convertDec dec, convertStrexp strexp)
        | Ast.MarkStr (strexp, region) => MarkStr (convertStrexp strexp, region)
        | Ast.VarStr path => VarStr path

  and convertFctexp fctexp =
      case fctexp of
          Ast.AppFct (path, strexps, sigConst) =>
          AppFct (path, List.map (fn (strexp, bool) => (convertStrexp strexp, bool)) strexps,
                  mapSigconst convertFsigexp sigConst)
        | Ast.BaseFct {body:Ast.strexp, constraint:Ast.sigexp Ast.sigConst,
                       params:(symbol option * Ast.sigexp) list} =>
          BaseFct { body = convertStrexp body, constraint = mapSigconst convertSigexp constraint
                    , params = List.map (fn (sym, sigexp) => (sym, convertSigexp sigexp)) params}
        | Ast.LetFct (dec, fctexp) => LetFct (convertDec dec, convertFctexp fctexp)
        | Ast.MarkFct (fctexp, region) => MarkFct (convertFctexp fctexp, region)
        | Ast.VarFct (path, sigConst) => VarFct (path, mapSigconst convertFsigexp sigConst)

  and convertWherespec (Ast.WhStruct x) = WhStruct x
    | convertWherespec (Ast.WhType (sym, tyvar, ty)) = WhType (sym, List.map convertTyvar tyvar, convertTy ty)

  and convertSigexp sigexp =
      case sigexp of
          Ast.AugSig (sigexp, wherespecs) => AugSig (convertSigexp sigexp, List.map convertWherespec wherespecs)
        | Ast.BaseSig specs => BaseSig (List.map convertSpec specs)
        | Ast.MarkSig (sigexp, region) => MarkSig (convertSigexp sigexp, region)
        | Ast.VarSig sym => VarSig sym

  and convertFsigexp fsigexp =
      case fsigexp of
          Ast.BaseFsig {param:(symbol option * Ast.sigexp) list, result:Ast.sigexp} =>
          BaseFsig {param=List.map (fn (sym, sigexp) => (sym, convertSigexp sigexp)) param
                   , result = convertSigexp result}
        | Ast.MarkFsig (fsigexp, region) => MarkFsig (convertFsigexp fsigexp, region)
        | Ast.VarFsig sym => VarFsig sym

  and convertSpec spec =
      case spec of
          Ast.DataReplSpec (symbol, path) => DataReplSpec (symbol, path)
        | Ast.DataSpec {datatycs:Ast.db list, withtycs:Ast.tb list} =>
          DataSpec {datatycs=List.map convertDb datatycs, withtycs=List.map convertTb withtycs}
        | Ast.ExceSpec tys => ExceSpec (List.map (fn (sym, ty) => (sym, Option.map convertTy ty)) tys)
        | Ast.FctSpec fsigexps => FctSpec (List.map (fn (sym, fsigexp) => (sym, convertFsigexp fsigexp)) fsigexps)
        | Ast.IncludeSpec sigexp => IncludeSpec (convertSigexp sigexp)
        | Ast.MarkSpec (spec, region) => MarkSpec (convertSpec spec, region)
        | Ast.ShareStrSpec paths => ShareStrSpec paths
        | Ast.ShareTycSpec paths => ShareTycSpec paths
        | Ast.StrSpec strs => StrSpec (List.map (fn (sym, sigexp, path) => (sym, convertSigexp sigexp, path)) strs)
        | Ast.TycSpec (tys, b) =>
          TycSpec (List.map
                       (fn (sym, tyvars, ty) =>
                           (sym, List.map convertTyvar tyvars, Option.map convertTy ty)) tys
                  , b)
        | Ast.ValSpec tys => ValSpec (List.map (fn (sym, ty) => (sym, convertTy ty)) tys)

  and convertDec dec =
      case dec of
          Ast.AbsDec strbs => AbsDec (List.map convertStrb strbs)
        | Ast.AbstypeDec {abstycs:Ast.db list, body:Ast.dec, withtycs:Ast.tb list} =>
          AbstypeDec {abstycs=List.map convertDb abstycs
                     , body = convertDec dec
                     , withtycs = List.map convertTb withtycs}
        | Ast.DataReplDec (sym, path) => DataReplDec (sym, path)
        | Ast.DatatypeDec {datatycs:Ast.db list, withtycs:Ast.tb list} =>
          DatatypeDec {datatycs=List.map convertDb datatycs
                      , withtycs = List.map convertTb withtycs}
        | Ast.DoDec exp => DoDec (convertExp exp)
        | Ast.ExceptionDec ebs => ExceptionDec (List.map convertEb ebs)
        | Ast.FctDec fctbs => FctDec (List.map convertFctb fctbs)
        | Ast.FixDec {fixity:fixity, ops:symbol list} => FixDec {fixity=fixity, ops=ops}
        | Ast.FsigDec fsigbs => FsigDec (List.map convertFsigb fsigbs)
        | Ast.FunDec (fbs, tyvars) => FunDec (List.map convertFb fbs, List.map convertTyvar tyvars)
        | Ast.LocalDec (d1, d2) => LocalDec (convertDec d1, convertDec d2)
        | Ast.MarkDec (dec, region) => MarkDec (convertDec dec, region)
        | Ast.OpenDec paths => OpenDec paths
        | Ast.OvldDec (symbol, ty, exps) => OvldDec (symbol, convertTy ty, List.map convertExp exps)
        | Ast.SeqDec decs => SeqDec (List.map convertDec decs)
        | Ast.SigDec sigbs => SigDec (List.map convertSigb sigbs)
        | Ast.StrDec strbs => StrDec (List.map convertStrb strbs)
        | Ast.TypeDec tbs => TypeDec (List.map convertTb tbs)
        | Ast.ValDec (vbs, tyvars) => ValDec (List.map convertVb vbs, List.map convertTyvar tyvars)
        | Ast.ValrecDec (rvbs, tyvars) => ValrecDec (List.map convertRvb rvbs, List.map convertTyvar tyvars)

  and convertVb vb =
      case vb of
          Ast.MarkVb (vb, region) => MarkVb (convertVb vb, region)
        | Ast.Vb {exp:Ast.exp, lazyp:bool, pat:Ast.pat} =>
          Vb {exp=convertExp exp, lazyp=lazyp, pat=convertPat pat}

  and convertRvb rvb =
      case rvb of
          Ast.MarkRvb (rvb, region) => MarkRvb (convertRvb rvb, region)
        | Ast.Rvb {exp:Ast.exp, fixity:(symbol * region) option, lazyp:bool,
               resultty:Ast.ty option, var:symbol} =>
          Rvb {exp=convertExp exp, fixity=fixity, lazyp=lazyp
               , resultty=Option.map convertTy resultty, var=var}

  and convertFb (Ast.Fb (clauses, b)) = Fb (List.map convertClause clauses, b)
    | convertFb (Ast.MarkFb (fb, region)) = MarkFb (convertFb fb, region)

  and convertClause (Ast.Clause {exp:Ast.exp, pats:Ast.pat Ast.fixitem list, resultty:Ast.ty option}) =
      Clause {exp=convertExp exp, pats=List.map (mapFixitem convertPat) pats
              , resultty=Option.map convertTy resultty}

  and convertTb (Ast.MarkTb (tb, region)) = MarkTb (convertTb tb, region)
    | convertTb (Ast.Tb {def:Ast.ty, tyc:symbol, tyvars:Ast.tyvar list}) = Tb {def=convertTy def, tyc=tyc,
                                                                   tyvars=List.map convertTyvar tyvars}

  and convertDb (Ast.Db {lazyp:bool, rhs:(symbol * Ast.ty option) list, tyc:symbol,
                     tyvars:Ast.tyvar list}) =
      Db {lazyp=lazyp, rhs=List.map (fn (sym, ty) => (sym, Option.map convertTy ty)) rhs
          , tyc=tyc, tyvars = List.map convertTyvar tyvars}
    | convertDb (Ast.MarkDb (db, region)) = MarkDb (convertDb db, region)

  and convertEb eb =
      case eb of
          Ast.EbDef {edef:path, exn:symbol} => EbDef {edef=edef, exn=exn}
        | Ast.EbGen {etype:Ast.ty option, exn:symbol} => EbGen {etype=Option.map convertTy etype, exn=exn}
        | Ast.MarkEb (eb, region) => MarkEb (convertEb eb, region)

  and convertStrb (Ast.MarkStrb (strb, region)) = MarkStrb (convertStrb strb, region)
    | convertStrb (Ast.Strb {constraint:Ast.sigexp Ast.sigConst, def:Ast.strexp, name:symbol}) =
      Strb {constraint=mapSigconst convertSigexp constraint, def = convertStrexp def, name=name}

  and convertFctb (Ast.Fctb {def:Ast.fctexp, name:symbol}) = Fctb {def=convertFctexp def, name=name}
    | convertFctb (Ast.MarkFctb (fctb, region)) = MarkFctb (convertFctb fctb, region)

  and convertSigb (Ast.MarkSigb (sigb, region)) = MarkSigb (convertSigb sigb, region)
    | convertSigb (Ast.Sigb {def:Ast.sigexp, name:symbol}) = Sigb {def=convertSigexp def, name=name}

  and convertFsigb (Ast.Fsigb {def:Ast.fsigexp, name:symbol}) = Fsigb {def=convertFsigexp def, name=name}
    | convertFsigb (Ast.MarkFsigb (fsigb, region)) = MarkFsigb (convertFsigb fsigb, region)

  and convertTyvar (Ast.MarkTyv (tyvar, region)) = MarkTyv (convertTyvar tyvar, region)
    | convertTyvar (Ast.Tyv sym) = Tyv sym

  and convertTy ty =
      case ty of
          Ast.ConTy (syms, tys) => ConTy (syms, List.map convertTy tys)
        | Ast.MarkTy (ty, region) => MarkTy (convertTy ty, region)
        | Ast.RecordTy tys => RecordTy (List.map (fn (sym, ty) => (sym, convertTy ty)) tys)
        | Ast.TupleTy tys => TupleTy (List.map convertTy tys)
        | Ast.VarTy tyvar => VarTy (convertTyvar tyvar)

end
