{-# LANGUAGE UndecidableInstances, FunctionalDependencies, FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards, FlexibleInstances, CPP #-}
-- |
-- Module      : Language.Haskell.BuildWrapper.Find
-- Copyright   : (c) Thomas Schilling 2008, JP Moresmau 2011
-- License     : BSD-style
--
-- Maintainer  : jpmoresmau@gmail.com
-- Stability   : beta
-- Portability : portable
--
-- Find things in a syntax tree.
--
module Language.Haskell.BuildWrapper.Find
  ( findHsThing, SearchResult(..), SearchResults, Search
  , PosTree(..), PosForest, deepestLeaf, pathToDeepest, searchBindBag
  , surrounds, overlaps, haddockType, prettyResult, qualifiedResult, typeOf, start, end
#ifdef SCION_DEBUG
  , prop_invCmpOverlap
#endif
  ) 
where

import GHC
import Outputable
import Name hiding (varName)

import BasicTypes ( IPName(..) )
import Bag
import Var ( varName,varType )

#if __GLASGOW_HASKELL__ < 702
import TypeRep ( Type(..), PredType(..) )
#else 
import TypeRep ( Type(..), Pred(..) )
#endif

import Data.Monoid ( mempty, mappend, mconcat )
import Data.Foldable as F ( maximumBy )
import Data.Ord    ( comparing )
import qualified Data.Set as S

------------------------------------------------------------------------------


isRecStmt :: StmtLR idL idR -> Bool

#if __GLASGOW_HASKELL__ < 611

isRecStmt (RecStmt _ _ _ _ _) = True
isRecStmt _ = False

#else

isRecStmt (RecStmt _ _ _ _ _ _ _ _) = True
isRecStmt _ = False

#endif

-- | Pretty-print a search result.
prettyResult :: OutputableBndr id => SearchResult id -> SDoc
prettyResult (FoundId i) = ppr i
prettyResult (FoundName n) = ppr n
prettyResult (FoundCon _ c) = ppr c
prettyResult r = ppr r

-- | Pretty-print a search result as qualified name 
qualifiedResult :: OutputableBndr id => SearchResult id -> SDoc
qualifiedResult (FoundId i) = qualifiedName $ getName  i
qualifiedResult (FoundName n) = qualifiedName n
qualifiedResult (FoundCon _ c) = qualifiedName $ getName c
qualifiedResult r = ppr r

qualifiedName :: Name -> SDoc
qualifiedName n = maybe empty  (\x-> (ppr x) <> dot) (nameModule_maybe n) <> (ppr n)

typeOf :: (SearchResult Id, [SearchResult Id]) -> Maybe Type
typeOf (FoundId ident, path) =
  let -- Unwrap a HsWrapper and its associated type
      unwrap WpHole t            = t
      unwrap (WpCompose w1 w2) t = unwrap w1 (unwrap w2 t)
      unwrap (WpCast _) t        = t -- XXX: really?
      unwrap (WpTyApp t') t      = AppTy t t'
      unwrap (WpTyLam tv) t      = ForAllTy tv t
      -- do something else with coercion/dict vars?
#if __GLASGOW_HASKELL__ < 700
      unwrap (WpApp v) t         = AppTy t (TyVarTy v)
      unwrap (WpLam v) t         = ForAllTy v t
#else
      -- unwrap (WpEvApp v) t       = AppTy t (TyVarTy v)
      unwrap (WpEvLam v) t       = ForAllTy v t
      unwrap (WpEvApp _) t       = t
#endif
      unwrap (WpLet _) t       = t
#ifdef WPINLINE
      unwrap WpInline t          = t
#endif
  in  case path of
        FoundExpr _ (HsVar _) : FoundExpr _ (HsWrap wr _) : _ ->
            Just $ reduce_type $ unwrap wr (varType ident)
        _ -> Just (varType ident)

-- All other search results produce no type information
typeOf _ = Nothing

-- | Reduce a top-level type application if possible.  That is, we perform the
-- following simplification step:
-- @
--     (forall v . t) t'   ==>   t [t'/v]
-- @
-- where @[t'/v]@ is the substitution of @t'@ for @v@.
--
reduce_type :: Type -> Type
reduce_type (AppTy (ForAllTy tv b) t) =
    reduce_type (subst_type tv t b)
reduce_type t = t

subst_type :: TyVar -> Type -> Type -> Type
subst_type v t' t0 = go t0
  where
    go t = case t of
      TyVarTy tv 
        | tv == v   -> t'
        | otherwise -> t
      AppTy t1 t2   -> AppTy (go t1) (go t2)
      TyConApp c ts -> TyConApp c (map go ts)
      FunTy t1 t2   -> FunTy (go t1) (go t2)
      ForAllTy v' bt 
        | v == v'   -> t
        | otherwise -> ForAllTy v' (go bt)
      PredTy pt     -> PredTy (go_pt pt)

   -- XXX: this is probably not right
    go_pt (ClassP c ts)  = ClassP c (map go ts)
    go_pt (IParam i t)   = IParam i (go t)
    go_pt (EqPred t1 t2) = EqPred (go t1) (go t2)

data PosTree a = Node { val :: a, children :: PosForest a }
               deriving (Eq, Ord)
type PosForest a = S.Set (PosTree a)

-- | Lookup all the things in a certain region.
findHsThing :: Search id a => (SrcSpan -> Bool) -> a -> SearchResults id
findHsThing p a = search p noSrcSpan a

deepestLeaf :: Ord a => PosTree a -> a
deepestLeaf t = snd $ go (0::Int) t
  where
    go n (Node x xs)
      | S.null xs = (n,x)
      | otherwise = maximumBy (comparing fst) (S.map (go (n+1)) xs)

-- | Returns the deepest leaf, together with the path to this leaf.  For
-- example, for the following tree with root @A@:
--
-- @
--     A -+- B --- C
--        '- D --- E --- F
-- @
--
-- this function will return:
--
-- @
--    (F, [E, D, A])
-- @
--
-- If @F@ were missing the result is either @(C, [B,A])@ or @(E, [D,A])@.
-- 
pathToDeepest :: Ord a => PosForest a -> Maybe (a, [a])
pathToDeepest forest 
  | S.null forest = Nothing
  | otherwise = Just $ ptl3 $ go_many (0::Int) [] forest
  where
    go n path (Node x xs)
      | S.null xs = (n, x, path)
      | otherwise = go_many (n+1) (x:path) xs
    go_many n path xs = 
      maximumBy (comparing fst3) (S.map (go n path) xs)
    fst3 (x,_,_) = x
    ptl3 (_,x,y) = (x,y)

haddockType  :: SearchResult a -> String
haddockType (FoundName n)
        | isValOcc (nameOccName n)="v"
        | otherwise= "t"
haddockType (FoundId i)
        | isValOcc (nameOccName (Var.varName i))="v"
        | otherwise= "t"
haddockType _="t"

data SearchResult id
  = FoundBind SrcSpan (HsBind id)
  | FoundPat  SrcSpan (Pat id)
  | FoundType SrcSpan (HsType id)
  | FoundExpr SrcSpan (HsExpr id)
  | FoundStmt SrcSpan (Stmt id)
  | FoundId   Id
  | FoundName Name
  | FoundCon  SrcSpan DataCon
  | FoundLit  SrcSpan HsLit

resLoc :: SearchResult id -> SrcSpan
resLoc (FoundId i) = nameSrcSpan (varName i)
resLoc (FoundName n) = nameSrcSpan n
resLoc (FoundBind s _) = s
resLoc (FoundPat s _)  = s
resLoc (FoundType s _) = s
resLoc (FoundExpr s _) = s
resLoc (FoundStmt s _) = s
resLoc (FoundCon s _)  = s
resLoc (FoundLit s _)  = s

instance Eq (SearchResult id) where
  a == b = resLoc a == resLoc b   -- TODO: sufficient?

instance Ord (SearchResult id) where
  compare a b = compare (resLoc a) (resLoc b)

type SearchResults id = PosForest (SearchResult id)

-- | Given two good SrcSpans (see 'SrcLoc.isGoodSrcSpan'), returns 'EQ' if the
-- spans overlap or, if not, the relative ordering of both.
cmpOverlap :: SrcSpan -> SrcSpan -> Ordering
cmpOverlap sp1 sp2
  | not (isGoodSrcSpan sp1) = LT
  | not (isGoodSrcSpan sp2) = GT
  | end1 < start2 = LT
  | end2 < start1 = GT
  | otherwise     = EQ
 where
   -- At this point we assume that both spans are good.  We also ignore the
   -- file names since faststrings seem to be rather unreliable.
   start1 = start sp1
   end1   = end sp1
   start2 = start sp2
   end2   = end sp2
   
start, end :: SrcSpan -> (Int,Int)   
#if __GLASGOW_HASKELL__ < 702   
start ss= (srcSpanStartLine ss, srcSpanStartCol ss)
end ss= (srcSpanEndLine ss, srcSpanEndCol ss)
#else 
start (RealSrcSpan ss)= (srcSpanStartLine ss, srcSpanStartCol ss)
start (UnhelpfulSpan _)=error "UnhelpfulSpan in cmpOverlap start"
end (RealSrcSpan ss)= (srcSpanEndLine ss, srcSpanEndCol ss)   
end (UnhelpfulSpan _)=error "UnhelpfulSpan in cmpOverlap start"   
#endif

surrounds :: SrcSpan -> SrcSpan -> Bool
surrounds outer inner = start1 <= start2 && end2 <= end1
  where
   start1 = srcSpanStart outer
   end1   = srcSpanEnd outer
   start2 = srcSpanStart inner
   end2   = srcSpanEnd inner

overlaps :: SrcSpan -> SrcSpan -> Bool
overlaps sp1 sp2 = cmpOverlap sp1 sp2 == EQ
    

#ifdef SCION_DEBUG

prop_invCmpOverlap :: SrcSpan -> SrcSpan -> Bool
prop_invCmpOverlap s1 s2 =
  case cmpOverlap s1 s2 of
    LT -> cmpOverlap s2 s1 == GT
    EQ -> cmpOverlap s2 s1 == EQ
    GT -> cmpOverlap s2 s1 == LT

-- prop_sane : if overlap -> there is some point which is in both spans

#endif


------------------------------------------------------------------------------

instance (OutputableBndr id, Outputable id)
    => Outputable (SearchResult id) where
  ppr (FoundBind s b) = text "bind:" <+> ppr s $$ nest 4 (ppr b)
  ppr (FoundPat s b)  = text "pat: " <+> ppr s $$ nest 4 (ppr b)
  ppr (FoundType s t) = text "type:" <+> ppr s $$ nest 4 (ppr t)
  ppr (FoundExpr s e) = text "expr:" <+> ppr s $$ nest 4 (ppr e)
  ppr (FoundStmt s t) = text "stmt:" <+> ppr s $$ nest 4 (ppr t)
  ppr (FoundId i)     = text "id:  " <+> ppr i
  ppr (FoundName n)   = text "name:" <+> ppr n
  ppr (FoundCon s c)  = text "con: " <+> ppr s $$ nest 4 (ppr c)
  ppr (FoundLit s l)  = text "lit: " <+> ppr s $$ nest 4 (ppr l)

instance Outputable a => Outputable (PosTree a) where
  ppr (Node v cs) = ppr v $$ nest 2 (vcat (map ppr (S.toList cs)))

class Search id a | a -> id where
  search :: (SrcSpan -> Bool) -> SrcSpan -> a -> SearchResults id

only :: SearchResult id -> SearchResults id
only r = S.singleton (Node r S.empty)

above :: SearchResult id -> SearchResults id -> SearchResults id
above r rest = S.singleton (Node r rest)

instance Search id Id where
  search _ _ i = only (FoundId i)

instance Search Name Name where
  search _ _ i = only (FoundName i)

instance Search id DataCon where
  search _ s d = only (FoundCon s d)

instance Search id HsLit where
  search _ s l = only (FoundLit s l)

instance Search id id => Search id (IPName id) where
  search p s (IPName i) = search p s i

--instance Search id id => Search id (Located (HsBindLR id id)) where
-- search p s (L _ a@AbsBinds{})= search p s a
-- search p _ (L s a)
--    | p s   = search p s a
--    | otherwise = mempty

-- at least in GHC7 if you have a AbsBind with a type signature the SrcSpan of the AbsBind covers only the type signature...
searchBindBag :: Search id id => (SrcSpan -> Bool) -> SrcSpan -> Bag (Located (HsBindLR id id)) -> SearchResults id
searchBindBag p s bs = mconcat $ fmap (searchBinds p s) (bagToList bs)
    
searchBinds :: Search id id => (SrcSpan -> Bool) -> SrcSpan -> (Located (HsBindLR id id)) -> SearchResults id
searchBinds p s (L _ a@AbsBinds{})= search p s a -- ignore location of the absbinds
searchBinds p _ (L s a)
    | p s   = search p s a
    | otherwise = mempty
    
instance Search id a => Search id (Located a) where
  search p _ (L s a)
    | p s   = search p s a
    | otherwise = mempty

instance Search id a => Search id (Bag a) where
  search p s bs = mconcat $ fmap (search p s) (bagToList bs)

instance Search id a => Search id [a] where
  search p s bs = mconcat $ fmap (search p s) bs

instance Search id a => Search id (Maybe a) where
  search _ _ Nothing = mempty
  search p s (Just a) = search p s a

instance (Search id id) => Search id (HsGroup id) where
  search p s grp =
      search p s (hs_valds grp)
      -- TODO

instance (Search id id) => Search id (HsBindLR id id) where
  search p s b = FoundBind s b `above` search_inside
    where
      search_inside = 
        case b of
          FunBind { fun_id = i, fun_matches = ms } -> 
              search p s i `mappend` search p s ms
          AbsBinds { abs_binds = bs }  -> searchBindBag p s bs
          PatBind { pat_lhs = lhs, pat_rhs = rhs } ->
              search p s lhs `mappend` search p s rhs
          VarBind { var_rhs = rhs } -> search p s rhs    


instance (Search id id) => Search id (MatchGroup id) where
  search p s (MatchGroup ms _) = search p s ms

instance (Search id id) => Search id (Match id) where
  search p s (Match pats tysig rhss) =
    search p s pats `mappend` search p s tysig `mappend` search p s rhss
    
instance (Search id id) => Search id (Pat id) where
  search p s pat0 = FoundPat s pat0 `above` search_inside
    where
      search_inside = 
        case pat0 of
          VarPat i              -> search p s i
#if __GLASGOW_HASKELL__ < 702          
          VarPatOut i _         -> search p s i
          TypePat t             -> search p s t
#endif          
          LitPat pat            -> search p s pat
          LazyPat pat           -> search p s pat
          AsPat i pat           -> search p s i `mappend` search p s pat
          ParPat pat            -> search p s pat
          BangPat pat           -> search p s pat
          ListPat ps _          -> search p s ps
          TuplePat ps _ _       -> search p s ps
          PArrPat ps _          -> search p s ps
          ConPatIn i d          -> search p s i `mappend` search p s d
          ConPatOut c _ _ _ d _ -> search p s c `mappend` search p s d
          ViewPat e pt _        -> search p s e `mappend` search p s pt
          SigPatIn pt t         -> search p s pt `mappend` search p s t
          SigPatOut pt _        -> search p s pt
          NPlusKPat n _ _ _     -> search p s n
          QuasiQuotePat qq      -> search p s qq
          CoPat _ pt _          -> search p s pt
          _ -> mempty

-- type HsConPatDetails id = HsConDetails (LPat id) (HsRecFields id (LPat id))
instance (Search id arg, Search id rec) => Search id (HsConDetails arg rec) where
  search p s (PrefixCon args) = search p s args
  search p s (RecCon rec)     = search p s rec
  search p s (InfixCon a1 a2) = search p s a1 `mappend` search p s a2

--instance (Search id id) => Search id  (HsModule id) where
--  search p s m =search p s (hsmodDecls m)

instance Search Name (RenamedSource) where
  search p s (b,_,_,_) = search p s b

instance (Search id id) => Search id (HsType id) where
  search _ s t = only (FoundType s t)

instance (Search id id) => Search id (GRHSs id) where
  search p s (GRHSs rhss local_binds) =
    search p s rhss `mappend` search p s local_binds

instance (Search id id) => Search id (GRHS id) where
  search p s (GRHS _guards rhs) =
    -- guards look like statements, but we should probably treat them
    -- differently
    search p s _guards `mappend` search p s rhs

instance (Search id id) => Search id (HsExpr id) where
  search p s e0 = FoundExpr s e0 `above` search_inside
    where
      search_inside = 
        case e0 of
          HsVar i -> search p s i
          HsIPVar i -> search p s i
          HsLit l -> search p s l
          ExprWithTySigOut e _t -> search p s e --`mappend` search p s t
          HsBracketOut _b _ -> mempty -- search p s b
          HsLam mg -> search p s mg
          HsApp l r -> search p s l `mappend` search p s r
          OpApp l o _ r -> search p s l `mappend` search p s o 
                                        `mappend` search p s r
          NegApp e n    -> search p s e `mappend` search p s n
          HsPar e       -> search p s e
          SectionL e o  -> search p s e `mappend` search p s o
          SectionR o e  -> search p s o `mappend` search p s e
          HsCase e mg   -> search p s e `mappend` search p s mg
#if __GLASGOW_HASKELL__ < 700
          HsIf c t e    -> search p s c `mappend` search p s t 
                                        `mappend` search p s e
#else
          HsIf _ c t e  -> search p s c `mappend` search p s t 
                                        `mappend` search p s e
#endif
          HsLet bs e    -> search p s bs `mappend` search p s e
#if  __GLASGOW_HASKELL__ < 702
          HsDo _ ss e _ -> search p s ss `mappend` search p s e
#else 
          HsDo _ ss _ -> search p s ss
#endif          
          ExplicitList _ es     -> search p s es
          ExplicitPArr _ es     -> search p s es
          ExplicitTuple es _    -> search p s es
          RecordCon _ _ bs      -> search p s bs
          RecordUpd es bs _ _ _ -> search p s es `mappend` search p s bs
          ExprWithTySig e t     -> search p s e `mappend` search p s t
          --ExprWithTySigOut e t  -> mempty
          ArithSeq _ i          -> search p s i
          PArrSeq _ i           -> search p s i
          HsSCC _ e             -> search p s e
          HsCoreAnn _ e         -> search p s e
          HsBracket b      -> search p s b
          --HsBracketOut b _ -> search p s b --
          HsSpliceE sp     -> search p s sp
          HsQuasiQuoteE _  -> mempty
          HsProc pat ct        -> search p s pat `mappend` search p s ct
          HsArrApp f arg _ _ _ -> search p s f `mappend` search p s arg
          HsArrForm e _ cmds   -> search p s e `mappend` search p s cmds
          HsTick _ _ e     -> search p s e
          HsBinTick _ _ e  -> search p s e
          HsTickPragma _ e -> search p s e 
          HsWrap _ e       -> search p s e
          _ -> mempty

#if __GLASGOW_HASKELL__ > 610
instance (Search id id) => Search id (HsTupArg id) where
  search p s (Present e) = search p s e
  search _ _ _ = mempty
#endif

instance (Search id id) => Search id (HsLocalBindsLR id id) where
  search p s (HsValBinds val_binds) = search p s val_binds
  search _ _ _ = mempty

instance (Search id id) => Search id (HsValBindsLR id id) where
  search p s (ValBindsOut rec_binds _) =
      mconcat $ fmap (searchBindBag p s . snd) rec_binds
  search _ _ _ = mempty

instance (Search id id) => Search id (HsCmdTop id) where
  search p s (HsCmdTop c _ _ _) = search p s c

instance (Search id id) => Search id (StmtLR id id) where
  search p s st 
    | isRecStmt st = search_inside -- see Note [SearchRecStmt]
    | otherwise               = FoundStmt s st `above` search_inside
    where
      search_inside =
        case st of
          BindStmt pat e _ _ -> search p s pat `mappend` search p s e
#if __GLASGOW_HASKELL__ < 702          
          ExprStmt e _ _     -> search p s e
          ParStmt ss         -> search p s (concatMap fst ss)
#else
          ExprStmt e _ _ _   -> search p s e
          ParStmt ss _ _ _   -> search p s (concatMap fst ss)
#endif
          LetStmt bs         -> search p s bs
          
#if __GLASGOW_HASKELL__ < 700
          TransformStmt (ss,_) f e -> search p s ss `mappend` search p s f
                                                    `mappend` search p s e
          GroupStmt (ss, _) g -> search p s ss `mappend` search p s g
#elif __GLASGOW_HASKELL__ < 702
          TransformStmt ss _ f e -> search p s ss `mappend` search p s f
                                                  `mappend` search p s e
          GroupStmt ss _ g gg -> search p s ss `mappend` search p s g
                                               `mappend` either (search p s) (const mempty) gg
#else
          LastStmt e _ -> search p s e
          TransStmt _ ss _ f e _ _ _-> search p s ss `mappend` search p s f
                                                  `mappend` search p s e
#endif
          RecStmt{recS_stmts=sts} -> search p s sts

--
-- Note [SearchRecStmt]
-- --------------------
--
-- We only return children of a RecStmt but not the RecStmt itself, even
-- though a RecStmt may occur in the source code (under very rare
-- circumstances).  The reasons are:
--
--  * We have no way of knowing whether the RecStmt actually occured in the
--    source code.  We could add a flag in GHC, but its probably not
--    worthwhile due to the other reason.
--
--  * GHC may move things out of the recursive group if it detects that these
--    things are in fact not recursive at all.  Source locations are
--    preserved, so this is fine.
--

#if __GLASGOW_HASKELL__ < 700
instance (Search id id) => Search id (GroupByClause id) where
  search p s (GroupByNothing f) = search p s f
  search p s (GroupBySomething using_f e) =
      either (search p s) (const mempty) using_f `mappend` search p s e
#endif

instance (Search id id) => Search id (ArithSeqInfo id) where
  search p s (From e)         = search p s e
  search p s (FromThen e1 e2) = search p s e1 `mappend` search p s e2
  search p s (FromTo e1 e2)   = search p s e1 `mappend` search p s e2
  search p s (FromThenTo e1 e2 e3) = 
      search p s e1 `mappend` search p s e2 `mappend` search p s e3

-- type HsRecordBinds id = HsRecFields id (LHsExpr id)
instance Search id e => Search id (HsRecFields id e) where
  search p s (HsRecFields flds _) = search p s flds

instance Search id e => Search id (HsRecField id e) where
  search p s (HsRecField _lid a _) = search p s a

instance (Search id id) => Search id (HsBracket id) where
  search p s (ExpBr  e) = search p s e
  search p s (PatBr  q) = search p s q
#if __GLASGOW_HASKELL__ < 700
  search p s (DecBr g) = search p s g
#else
  search p s (DecBrL g) = search p s g
  search p s (DecBrG g) = search p s g
#endif
  search p s (TypBr  t) = search p s t
  search _ _ (VarBr  _) = mempty

instance (Search id id) => Search id (HsSplice id) where
  search p s (HsSplice _ e) = search p s e

#if __GLASGOW_HASKELL__ >= 700
instance (Search id id) => Search id ([id], [id]) where
  search p s (a, b) = search p s a `mappend` search p s b

instance (Search id id) => Search id (HsDecl id) where
  search p s (TyClD e)       = search p s e
  search p s (InstD e)       = search p s e
  search p s (DerivD e)      = search p s e
  search p s (ValD e)        = search p s e
  search p s (SigD e)        = search p s e
  search p s (DefD e)        = search p s e
  search p s (ForD e)        = search p s e
  search p s (WarningD e)    = search p s e
  search p s (AnnD e)        = search p s e
  search p s (RuleD e)       = search p s e
  search p s (SpliceD e)     = search p s e
  search p s (DocD e)        = search p s e
  search p s (QuasiQuoteD e) = search p s e

instance (Search id id) => Search id (TyClDecl id) where
  search p s (ForeignType n _)   = search p s n
  search p s (TyFamily _ n ns _) = search p s n `mappend` search p s ns
  search p s (TyData _ ct n v pt _ c d) = search p s ct `mappend` search p s n
                                                        `mappend` search p s v
                                                        `mappend` search p s pt
                                                        `mappend` search p s c
                                                        `mappend` search p s d
  search p s (TySynonym n v pt r) = search p s n `mappend` search p s v
                                                 `mappend` search p s pt
                                                 `mappend` search p s r
  search p s (ClassDecl ct n v fd sg m tt dc) = search p s ct `mappend` search p s n
                                                              `mappend` search p s v
                                                              `mappend` search p s fd
                                                              `mappend` search p s sg
                                                              `mappend` searchBindBag p s m
                                                              `mappend` search p s tt
                                                              `mappend` search p s dc

instance (Search id id) => Search id (InstDecl id) where
  search p s (InstDecl t b sg dc) = search p s t `mappend` searchBindBag p s b
                                                 `mappend` search p s sg
                                                 `mappend` search p s dc

instance (Search id id) => Search id (DerivDecl id) where
  search p s (DerivDecl t) = search p s t

instance (Search id id) => Search id (Sig id) where
  search p s (TypeSig n t)   = search p s n `mappend` search p s t
  search p s (IdSig i)       = search p s i
  search p s (FixSig n)      = search p s n
  search p s (InlineSig n _) = search p s n
  search p s (SpecSig n t _) = search p s n `mappend` search p s t
  search p s (SpecInstSig t) = search p s t

instance (Search id id) => Search id (FixitySig id) where
  search p s (FixitySig n _) = search p s n

instance (Search id id) => Search id (DefaultDecl id) where
  search p s (DefaultDecl n) = search p s n

instance (Search id id) => Search id (ForeignDecl id) where
  search p s (ForeignImport n t _) = search p s n `mappend` search p s t
  search p s (ForeignExport n t _) = search p s n `mappend` search p s t

instance (Search id id) => Search id (WarnDecl id) where
  search p s (Warning n _) = search p s n

instance (Search id id) => Search id (AnnDecl id) where
  search p s (HsAnnotation pr n) = search p s pr `mappend` search p s n

instance (Search id id) => Search id (AnnProvenance id) where
  search p s (ValueAnnProvenance n) = search p s n
  search p s (TypeAnnProvenance n)  = search p s n
  search _ _ (ModuleAnnProvenance)  = mempty

instance (Search id id) => Search id (RuleDecl id) where
  search p s (HsRule _ _ bn e1 _ e2 _) = search p s bn `mappend` search p s e1
                                                       `mappend` search p s e2

instance (Search id id) => Search id (RuleBndr id) where
  search p s (RuleBndr n)      = search p s n
  search p s (RuleBndrSig n t) = search p s n `mappend` search p s t

instance (Search id id) => Search id (SpliceDecl id) where
  search p s (SpliceDecl n _) = search p s n

instance (Search id id) => Search id DocDecl where
  search _ _ _ = mempty

instance (Search id id) => Search id (HsQuasiQuote id) where
  search _ _ _ = mempty

instance (Search id id) => Search id (ConDecl id) where
  search p s (ConDecl n _ qv ct dt res _ _) = search p s n `mappend` search p s qv
                                                           `mappend` search p s ct
                                                           `mappend` search p s dt
                                                           `mappend` search p s res

instance (Search id id) => Search id (ConDeclField id) where
  search p s (ConDeclField n t _) = search p s n `mappend` search p s t

instance (Search id id) => Search id (HsPred id) where
  search p s (HsClassP n t)   = search p s n  `mappend` search p s t
  search p s (HsEqualP n1 n2) = search p s n1 `mappend` search p s n2
  search p s (HsIParam i n)   = search p s i  `mappend` search p s n

instance (Search id id) => Search id (HsTyVarBndr id) where
  search p s (UserTyVar n _)   = search p s n
  search p s (KindedTyVar n _) = search p s n

instance (Search id id) => Search id (ResType id) where
  search _ _ (ResTyH98)    = mempty
  search p s (ResTyGADT n) = search p s n
#endif
