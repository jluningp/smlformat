structure AddComments = struct
  open CommentedAst

  fun mapFixitem f {fixity; item; region} =>
      {fixity=fixity; item=f item; region=region}

  fun mapSigconst f Nosig = NoSig
    | mapSigconst f (Opaque x) = Opaque (f x)
    | mapSigconst f (Transparent x) = Transparent (f x)

  fun convertExp exp =
      case exp of
          Ast.AndalsoExp (e1, e2) => AndalsoExp (convertExp e1, convertExp e2)
        | Ast.AppExp {argument:exp, function:exp} => AppExp {argument=convertExp argument
                                                            , function=convertExp function}
        | CaseExp {expr:exp, rules:rule list} => CaseExp {expr=convertExp expr,
                                                          rules=List.map convertRules rules}
        | CharExp str => CharExp str
        | ConstraintExp {constraint:ty, expr:exp} => ConstraintExp {constraint=convertTy ty
                                                                   , expr=convertExp expr}
    | FlatAppExp exps => FlatAppExp (List.map (mapFixitem convertExp) exps)
    | FnExp rules => FnExp (List.map convertRule rules)
    | HandleExp {expr:exp, rules:rule list} => HandleExp {expr=convertExp expr
                                                         , rules=List.map convertRules rules}
    | IfExp {elseCase:exp, test:exp, thenCase:exp} => IfExp {elseCase=convertExp elseCase
                                                            , test=convertExp test
                                                            , thenCase=convertExp thenCase}
    | IntExp literal => IntExp literal
    | LetExp {dec:dec, expr:exp} => LetExp {dec=convertDec dec
                                           ; expr = convertExp expr}
    | ListExp exps => ListExp (List.map convertExp exps)
    | MarkExp (exp, region) => MarkExp (convertExp, region)
    | OrelseExp (e1, e2) => OrelseExp (convertExp e1, convertExp e2)
    | RaiseExp e => RaiseExp (convertExp e)
    | RealExp s => RealExp s
    | RecordExp record => RecordExp (List.map (fn (s, e) => (s, convertExp e)) record)
    | SelectorExp sym => SelectorExp sym
    | SeqExp exps => SeqExp (List.map convertExp exps)
    | StringExp str => StringExp str
    | TupleExp exps => TupleExp (List.map convertExp exps)
    | VarExp path => VarExp path
    | VectorExp exps => VectorExp (List.map convertExp exps)
    | WhileExp {expr:exp, test:exp} => WhileExp {expr=convertExp expr, test=converExp expr}
    | WordExp literal => WordExp literal

  and convertRule (Ast.Rule {exp:exp, pat:pat}) =
      Rule {exp=convertExp exp, pat=convertPat pat}

  and convertPat pat =
      case pat of
          Ast.AppPat {argument:pat, constr:pat} => {argument=convertPat argument, constr=convertPat constr}
        | CharPat str => CharPat str
        | ConstraintPat {constraint:ty, pattern:pat} => ConstraintPat {constraint=convertTy constraint
                                                        , pattern=convertPat pattern}
    | FlatAppPat pats => FlatAppPat (List.map (mapFixitem convertPat) pats)
    | IntPat l => IntPat l
    | LayeredPat {expPat:pat, varPat:pat} => {expPat=convertPat expPat, varPat=convertPat varPat}
    | ListPat pats => ListPat (List.map convertPat pats)
    | MarkPat (pat, region) => MarkPat (convertPat pat, region)
    | OrPat pats => OrPat (List.map convertPat pats)
    | RecordPat {def:(symbol * pat) list, flexibility:bool} =>
      RecordPat {def = List.map (fun (sym, pat) => (sym, convertPat pat)) def
                , flexibility = flexibility}
    | StringPat str => StringPat str
    | TuplePat pats => TuplePat (List.map convertPat pats)
    | VarPat path => VarPat path
    | VectorPat pats => VectorPat (List.map convertPat pats)
    | WildPat => WildPat
    | WordPat l => WordPat l

  and convertStrexp strexp =
      case strexp of
          AppStr (path, strexps) => AppStr (path, List.map (fun (strexp, bool) =>
                                                                (convertStrexp strexp, bool)) strexps)
        | AppStrI (path, strexps) => AppStrI (path, List.map (fun (strexp, bool) =>
                                                                  (convertStrexp strexp, bool)) strexps)
    | BaseStr dec => BaseStr (convertDec dec)
    | ConstrainedStr (strexp, sigConst) => ConstraintStr (convertStrexp strexp, mapSigconst convertSigexp sigConst)
    | LetStr (dec, strexp) => LetStr (convertDec dec, convertStrexp strexp)
    | MarkStr (strexp, region) => MarkStr (convertStrexp strexp, region)
    | VarStr path => VarStr path

  and convertFctexp fctexp =
      case fctexp of
          AppFct (path, strexps, sigConst) =>
          AppFct (path, List.map (fun (strexp, bool) => (convertStrexp strexp, bool)) strexps,
                  mapSigConst convertFsigexp sigConst)
        | BaseFct {body:strexp, constraint:sigexp sigConst,
                   params:(symbol option * sigexp) list} =>
          BaseFct { body = convertStrexp body, constraint = mapSigConst convertSigexp constraint
                    , params = List.map (fun (sym, sigexp) => (sym, convertSigexp sigexp)) params}
    | LetFct (dec, fctexp) => LetFct (convertDec dec, convertFctexp fctexp)
    | MarkFct (fctexp, region) => MarkFct (convertFctexp fctexp, region)
    | VarFct (path, sigConst) => VarFct (path, mapSigConst convertFsigexp sigConst)

  and convertWherespec (Whstruct x) = Whstruct x
    | convertWherespec (WhType x) = WhType x

  and convertSigexp sigexp =
      case sigexp of
          AugSig (sigexp, wherespecs) => AugSig (convertSigexp sigexp, List.map convertWherespec wherespecs)
        | BaseSig specs => BaseSig (List.map converSpec specs)
        | MarkSig (sigexp, region) => MarkSig (convertSigexp sigexp, region)
        | VarSig sym => VarSig sym

  and convertFsigexp
          BaseFsig {param:(symbol option * sigexp) list, result:sigexp} =>
      BaseFsig {param=List.map (fun (sym, sigexp) => (sym, convertSigexp sigexp)) param
               ; result = convertSigexp sigexp}
    | MarkFsig (fsigexp, region) => MarkFsig (convertFsigexp fsigexp, region)
    | VarFsig sym => VarFsig sym

  and convertSpec
    = DataReplSpec (symbol, path) => DataReplSpec (symbol, path)
    | DataSpec {datatycs:db list, withtycs:tb list} =>
      DataSpec {datatycs=List.map convertDb datatycs, withtycs=List.map convertTb withtycs}
    | ExceSpec tys => ExceSpec (List.map (fun (sym, ty) => (sym, Option.map convertTy ty)) tys)
    | FctSpec fsigexps => FctSpec (List.map (fun (sym, fsigexp) => (sym, convertFsigexp fsigexp)) fsigexps)
    | IncludeSpec sigexp => IncludeSpec (convertSigexp sigexp)
    | MarkSpec (spec, region) => MarkSpec (convertSpec spec, region)
    | ShareStrSpec paths => ShareStrSpec paths
    | ShareTycSpec paths => ShareTycSpec paths
    | StrSpec strs => List.map (fun (sym, sigexp, path) => (sym, convertSigexp sigexp, path)) strs
    | TycSpec (tys, b) =>
      TycSpec (List.map
                   (fun (sym, tyvars, ty) =>
                        (sym, List.map convertTyvar tyvars, Option.map convertTy ty)) tys
              , b)
    | ValSpec tys => ValSpec (List.map (fun (sym, ty) => (sym, convertTy ty)) tys)

  and convertDec dec =
      case dec of
          AbsDec strbs => Absdec (convertStrb strb)
        | AbstypeDec {abstycs:db list, body:dec, withtycs:tb list} =>
          AbstypeDec {abstycs=List.map convertDb abstycs
                     , body = convertDec dec
                     , withtycs = List.map convertTb withtycs}
    | DataReplDec (sym, path) => DataReplDec (sym, path)
    | DatatypeDec {datatycs:db list, withtycs:tb list} => DatatypeDec {datatycs=List.map convertDb datatycs
                                                                      , withtycs = List.map converTb withtycs}
    | DoDec exp => DoDec (convertExp exp)
    | ExceptionDec ebs => ExceptionDec (List.map convertEb ebs)
    | FctDec fctbs => FctDec (List.map convertFctbs fctbs)
    | FixDec {fixity:fixity, ops:symbol list} => FixDec {fixity=fixity, ops=ops}
    | FsigDec fsigbs => Fsigdec (List.map convertFsigb fsigbs)
    | FunDec (fbs, tyvars) => FunDec (List.map convertFb fbs, List.map convertTyvar tyvars)
    | LocalDec (d1, d2) => LocalDec (convertDec d1, convertDec d2)
    | MarkDec (dec, region) => MarkDec (convertDec dec, region)
    | OpenDec paths => OpenDec paths
    | OvldDec (symbol, ty, exps) => OvldDec (symbol, convertTy ty, List.map convertExp exps)
    | SeqDec decs => SeqDec (List.map converDec decs)
    | SigDec sigbs => SigDec (List.map convertSigb sigbs)
    | StrDec strbs => StrDec (List.map convertStrb strbs)
    | TypeDec tbs => TypeDec (List.map convertTb tbs)
    | ValDec (vbs, tyvars) => ValDec (List.map convertVb vbs, List.map convertTyvars tyvars)
    | ValrecDec (rvbs, tyvars) => ValDec (List.map convertRvb rvbs, List.map convertTyvars tyvars)

  and convertVb vb =
      case vb of
          MarkVb (vb, region) => MarkVb (convertVb vb, region)
        | Vb {exp:exp, lazyp:bool, pat:pat} => {exp=convertExp exp, lazyp=lazyp, pat=convertPat pat}

  and convertRvb rvb =
      case rvb of
          MarkRvb (rvb, region) => MarkRvb (convertRvb rvb, region)
        | Rvb {exp:exp, fixity:(symbol * region) option, lazyp:bool,
               resultty:ty option, var:symbol} =>
          Rvb {exp=convertExp exp, fixity=fixity, lazyp=lazyp
               , resultty=Option.map convertTy resultty, var=var}

  and convertFb (Fb (clauses, b)) = Fb (List.map convertClause clauses, b)
    | convertFb (MarkFb (fb, region)) = MarkFb (convertFb fb, region)

  and convertClause (Clause {exp:exp, pats:pat fixitem list, resultty:ty option}) =
      Clause {exp=convertExp exp, pats=List.map (mapFixitem convertPat) pats
              , resultty=Option.map convertTy resultty}

  and convertTb (MarkTb (tb, region)) = MarkTb (convertTb tb, region)
    | convertTb (Tb {def:ty, tyc:symbol, tyvars:tyvar list}) = Tb {def=convertTy ty, tyc=tyc,
                                                                   tyvars=List.map converTyvar tyvars}

  and convertDb (Db {lazyp:bool, rhs:(symbol * ty option) list, tyc:symbol,
                     tyvars:tyvar list}) =
      Db {lazyp=lazyp, rhs=List.map (fun (sym, ty) => (sym, Option.map convertTy ty)) rhs
          , tyc=tyc, tyvars = List.map convertTyvar tyvars}
    | convertDb (Markdb (db, region)) = Markdb (convertDb db, region)

  and convertEb eb =
      case eb of
          EbDef {edef:path, exn:symbol} => Ebdef {edef=edef, exn=exn}
        | EbGen {etype:ty option, exn:symbol} => Ebgen {etype=Option.map convertTy etype, exn=exn}
        | MarkEb (eb, region) => MarkEb (convertEb eb, region)

  and convertStrb (MarkStrb (strb, region)) = MarkStrb (convertStrb strb, region)
    | convertStrb (Strb {constraint:sigexp sigConst, def:strexp, name:symbol}) =
      Strb {constraint=mapSigConst convertSigexp constraint, def = convertStrexp def, name=name}

  and convertFctb (Fctb {def:fctexp, name:symbol}) = Fctb {def=convertFctexp def, name=name}
    | convertFctb (MarkFctb (fctb, region)) = MarkFctb (convertFctb, region)

  and convertSigb (MarkSigb (sigb, region)) = MarkSigb (convertSigb sigb, region)
    | convertSigb (Sigb {def:sigexp, name:symbol}) = Sigb {def=convertSigexp def, name=name}

  and convertFsigb (Fsigb {def:fsigexp, name:symbol}) = Fsigb {def=convertFsigexp def, name=name}
    | convertFsigb (MarkFsigb (fsigb, region)) = MarkFsigb (convertFsigb fsigb, region)

  and convertTyvar (MarkTyvar (tyvar, region)) = MarkTyvar (convertTyvar tyvar, region)
    | convertTyvar (Tyv sym) = Tyv sym

  and convertTy ty =
      case ty of
          ConTy (syms, tys) => ConTy (syms, List.map converTy tys)
    | MarkTy (ty, region) => MarkTy (converTy ty, region)
    | RecordTy tys => RecordTy (List.map (fun (sym, ty) => (sym, converTy ty)) tys
    | TupleTy tys => TupleTy (List.map converTy tys)
    | VarTy tyvar => VarTy (converTyvar tyvar)

end
