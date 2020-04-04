structure Format = struct
  open CommentedAst

  fun mapFixitem f {fixity, item, region} =
      {fixity=fixity, item=f item, region=region}

  fun mapSigconst f Ast.NoSig = NoSig
    | mapSigconst f (Ast.Opaque x) = Opaque (f x)
    | mapSigconst f (Ast.Transparent x) = Transparent (f x)

  fun convertExp exp =
      case exp of
          Ast.AndalsoExp (e1, e2) =>
        | Ast.AppExp {argument:Ast.exp, function:Ast.exp} =>
        | Ast.CaseExp {expr:Ast.exp, rules:Ast.rule list} =>
        | Ast.CharExp str =>
        | Ast.ConstraintExp {constraint:Ast.ty, expr:Ast.exp} =>
        | Ast.FlatAppExp exps =>
        | Ast.FnExp rules =>
        | Ast.HandleExp {expr:Ast.exp, rules:Ast.rule list} =>
        | Ast.IfExp {elseCase:Ast.exp, test:Ast.exp, thenCase:Ast.exp} =>
        | Ast.IntExp literal =>
        | Ast.LetExp {dec:Ast.dec, expr:Ast.exp} =>
        | Ast.ListExp exps =>
        | Ast.MarkExp (exp, region) =>
        | Ast.OrelseExp (e1, e2) =>
        | Ast.RaiseExp e =>
        | Ast.RealExp s =>
        | Ast.RecordExp record =>
        | Ast.SelectorExp sym =>
        | Ast.SeqExp exps =>
        | Ast.StringExp str =>
        | Ast.TupleExp exps =>
        | Ast.VarExp path =>
        | Ast.VectorExp exps =>
        | Ast.WhileExp {expr:Ast.exp, test:Ast.exp} =>
        | Ast.WordExp literal =>

  and convertRule (Ast.Rule {exp:Ast.exp, pat:Ast.pat}) =

  and convertPat pat =
      case pat of
          Ast.AppPat {argument:Ast.pat, constr:Ast.pat} =>
        | Ast.CharPat str =>
        | Ast.ConstraintPat {constraint:Ast.ty, pattern:Ast.pat} =>
        | Ast.FlatAppPat pats =>
        | Ast.IntPat l =>
        | Ast.LayeredPat {expPat:Ast.pat, varPat:Ast.pat} =>
        | Ast.ListPat pats =>
        | Ast.MarkPat (pat, region) =>
        | Ast.OrPat pats =>
        | Ast.RecordPat {def:(symbol * Ast.pat) list, flexibility:bool} =>
        | Ast.StringPat str =>
        | Ast.TuplePat pats =>
        | Ast.VarPat path =>
        | Ast.VectorPat pats =>
        | Ast.WildPat =>
        | Ast.WordPat l =>

  and convertStrexp strexp =
      case strexp of
          Ast.AppStr (path, strexps) =>
        | Ast.AppStrI (path, strexps) =>
        | Ast.BaseStr dec =>
        | Ast.ConstrainedStr (strexp, sigConst) =>
        | Ast.LetStr (dec, strexp) =>
        | Ast.MarkStr (strexp, region) =>
        | Ast.VarStr path =>

  and convertFctexp fctexp =
      case fctexp of
          Ast.AppFct (path, strexps, sigConst) =>
        | Ast.BaseFct {body:Ast.strexp, constraint:Ast.sigexp Ast.sigConst,
                       params:(symbol option * Ast.sigexp) list} =>
        | Ast.LetFct (dec, fctexp) =>
        | Ast.MarkFct (fctexp, region) =>
        | Ast.VarFct (path, sigConst) =>

  and convertWherespec (Ast.WhStruct x) =
    | convertWherespec (Ast.WhType (sym, tyvar, ty)) =

  and convertSigexp sigexp =
      case sigexp of
          Ast.AugSig (sigexp, wherespecs) =>
        | Ast.BaseSig specs =>
        | Ast.MarkSig (sigexp, region) =>
        | Ast.VarSig sym =>

  and convertFsigexp fsigexp =
      case fsigexp of
          Ast.BaseFsig {param:(symbol option * Ast.sigexp) list, result:Ast.sigexp} =>
        | Ast.MarkFsig (fsigexp, region) =>
        | Ast.VarFsig sym =>

  and convertSpec spec =
      case spec of
          Ast.DataReplSpec (symbol, path) =>
        | Ast.DataSpec {datatycs:Ast.db list, withtycs:Ast.tb list} =>
        | Ast.ExceSpec tys =>
        | Ast.FctSpec fsigexps =>
        | Ast.IncludeSpec sigexp =>
        | Ast.MarkSpec (spec, region) =>
        | Ast.ShareStrSpec paths =>
        | Ast.ShareTycSpec paths =>
        | Ast.StrSpec strs =>
        | Ast.TycSpec (tys, b) =>
        | Ast.ValSpec tys =>

  and convertDec dec =
      case dec of
          Ast.AbsDec strbs =>
        | Ast.AbstypeDec {abstycs:Ast.db list, body:Ast.dec, withtycs:Ast.tb list} =>
        | Ast.DataReplDec (sym, path) =>
        | Ast.DatatypeDec {datatycs:Ast.db list, withtycs:Ast.tb list} =>
        | Ast.DoDec exp =>
        | Ast.ExceptionDec ebs =>
        | Ast.FctDec fctbs =>
        | Ast.FixDec {fixity:fixity, ops:symbol list} =>
        | Ast.FsigDec fsigbs =>
        | Ast.FunDec (fbs, tyvars) =>
        | Ast.LocalDec (d1, d2) =>
        | Ast.MarkDec (dec, region) =>
        | Ast.OpenDec paths =>
        | Ast.OvldDec (symbol, ty, exps) =>
        | Ast.SeqDec decs =>
        | Ast.SigDec sigbs =>
        | Ast.StrDec strbs =>
        | Ast.TypeDec tbs =>
        | Ast.ValDec (vbs, tyvars) =>
        | Ast.ValrecDec (rvbs, tyvars) =>

  and convertVb vb =
      case vb of
          Ast.MarkVb (vb, region) =>
        | Ast.Vb {exp:Ast.exp, lazyp:bool, pat:Ast.pat} =>

  and convertRvb rvb =
      case rvb of
          Ast.MarkRvb (rvb, region) =>
        | Ast.Rvb {exp:Ast.exp, fixity:(symbol * region) option, lazyp:bool,
               resultty:Ast.ty option, var:symbol} =>

  and convertFb (Ast.Fb (clauses, b)) =
    | convertFb (Ast.MarkFb (fb, region)) =

  and convertClause (Ast.Clause {exp:Ast.exp, pats:Ast.pat Ast.fixitem list, resultty:Ast.ty option}) =

  and convertTb (Ast.MarkTb (tb, region)) =
    | convertTb (Ast.Tb {def:Ast.ty, tyc:symbol, tyvars:Ast.tyvar list}) =

  and convertDb (Ast.Db {lazyp:bool, rhs:(symbol * Ast.ty option) list, tyc:symbol,
                     tyvars:Ast.tyvar list}) =
    | convertDb (Ast.MarkDb (db, region)) =

  and convertEb eb =
      case eb of
          Ast.EbDef {edef:path, exn:symbol} =>
        | Ast.EbGen {etype:Ast.ty option, exn:symbol} =>
        | Ast.MarkEb (eb, region) =>

  and convertStrb (Ast.MarkStrb (strb, region)) =
    | convertStrb (Ast.Strb {constraint:Ast.sigexp Ast.sigConst, def:Ast.strexp, name:symbol}) =

  and convertFctb (Ast.Fctb {def:Ast.fctexp, name:symbol}) =
    | convertFctb (Ast.MarkFctb (fctb, region)) =

  and convertSigb (Ast.MarkSigb (sigb, region)) =
    | convertSigb (Ast.Sigb {def:Ast.sigexp, name:symbol}) =

  and convertFsigb (Ast.Fsigb {def:Ast.fsigexp, name:symbol}) =
    | convertFsigb (Ast.MarkFsigb (fsigb, region)) =

  and convertTyvar (Ast.MarkTyv (tyvar, region)) =
    | convertTyvar (Ast.Tyv sym) =

  and convertTy ty =
      case ty of
          Ast.ConTy (syms, tys) =>
        | Ast.MarkTy (ty, region) =>
        | Ast.RecordTy tys =>
        | Ast.TupleTy tys =>
        | Ast.VarTy tyvar =>

end
