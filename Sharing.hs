{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}

module Sharing where

import Control.Applicative
import Control.Monad.Fix
import Data.HashTable                           as Hash
import qualified Data.IntMap                    as IntMap
import Debug.Trace
import System.IO.Unsafe                         (unsafePerformIO)
import Data.List
import Data.Maybe
import Data.Typeable
import System.Mem.StableName

import HOAS                                     (Level)
import qualified HOAS as HOAS


-- Occurences maps
-- ---------------

-- Opaque stable name for term nodes. It is being used to key the occurence map; hence, we need to
-- hide the type parameter.
--
data StableTermName where
  StableTermName :: Typeable t => StableName (HOAS.Term t) -> StableTermName

instance Show StableTermName where
  show (StableTermName sn) = show $ hashStableName sn

instance Eq StableTermName where
  StableTermName sn1 == StableTermName sn2
    | Just sn1' <- gcast sn1 = sn1' == sn2
    | otherwise              = False

makeStableTerm :: HOAS.Term t -> IO (StableName (HOAS.Term t))
makeStableTerm e = e `seq` makeStableName e

-- Mutable occurence map

-- Mutable hashtable version of the occurrence map keyed on the stable names of terms. It associates
-- each term node with an occurence count and the height of the AST.
--
type OccMapHash = Hash.HashTable StableTermName (Int, Int)

-- Create a new hash table keyed on AST nodes.
--
newOccMapHashTable :: IO OccMapHash
newOccMapHashTable = Hash.new (==) hashStableTerm
  where
    hashStableTerm (StableTermName sn) = fromIntegral (hashStableName sn)

-- Enter one term node occurrence into an occurrence map.  Returns 'Just h' if this is a repeated
-- occurence and the height of the repeatedly occuring term is 'h'.
--
-- If this is the first occurence, the 'height' *argument* must provide the height of the term;
-- otherwise, the height will be *extracted* from the occurence map.  In the latter case, this
-- function yields the term height.
--
enterOcc :: OccMapHash -> StableTermName -> Int -> IO (Maybe Int)
enterOcc occMap sa height
  = do
      entry <- Hash.lookup occMap sa
      case entry of
        Nothing           -> Hash.insert occMap sa (1    , height)  >> return Nothing
        Just (n, heightS) -> Hash.update occMap sa (n + 1, heightS) >> return (Just heightS)    

-- Immutable occurence map

-- Immutable version of the occurence map (storing the occurence count only, not the height).  We
-- use the 'StableName' hash to index an 'IntMap' and disambiguate 'StableName's with identical
-- hashes explicitly, storing them in a list in the 'IntMap'.
--
type OccMap = IntMap.IntMap [(StableTermName, Int)]

-- Turn a mutable into an immutable occurence map.
--
freezeOccMap :: OccMapHash -> IO OccMap
freezeOccMap oc
  = do
      kvs <- map dropHeight <$> Hash.toList oc
      return . IntMap.fromList . map (\kvs -> (key (head kvs), kvs)). groupBy sameKey $ kvs
  where
    key (StableTermName sn, _) = hashStableName sn
    sameKey kv1 kv2            = key kv1 == key kv2
    dropHeight (k, (cnt, _))   = (k, cnt)

-- Look up the occurence map keyed by array computations using a stable name.  If a the key does
-- not exist in the map, return an occurence count of '1'.
--
lookupWithTermName :: OccMap -> StableTermName -> Int
lookupWithTermName oc sa@(StableTermName sn) 
  = fromMaybe 1 $ IntMap.lookup (hashStableName sn) oc >>= Prelude.lookup sa

-- Look up the occurence map keyed by array computations using a sharing array computation.  If an
-- the key does not exist in the map, return an occurence count of '1'.
--
lookupWithSharingTerm :: OccMap -> StableSharingTerm -> Int
lookupWithSharingTerm oc (StableSharingTerm (StableTermHeight sn _) _) 
  = lookupWithTermName oc (StableTermName sn)


-- Term structure for sharing recovery
-- -----------------------------------

-- Terms for sharing recovery consist of two mutually recursive datatypes. The one hear hold the
-- actual lambda term forms. 'SharingTerm' keeps track of stable names (for identification) as well
-- as where variables and let bindings need to be introduced.
--
data Term binder t where
    -- for conversion to de Bruijn
  Tag :: Typeable t                               
      => Level                                                -> Term binder t    

  Con :: (Typeable t, Show t)                     
      => t                                                    -> Term binder t
  Lam :: (Typeable s, Typeable t, Show s, Show t) 
      => binder -> SharingTerm binder t                       -> Term binder (s -> t)
  App :: (Typeable s, Typeable t, Show s, Show t) 
      => SharingTerm binder (s -> t) -> SharingTerm binder  s -> Term binder t

showTermOp :: Term binder t -> String
showTermOp (Tag lvl) = "Tag " ++ show lvl
showTermOp (Con v)   = "Con " ++ show v
showTermOp (Lam {})  = "Lam"
showTermOp (App {})  = "App"


-- Stable term nodes
-- -----------------

-- Stable name for a term node including the height of the term.
--
data StableTermHeight t = StableTermHeight (StableName (HOAS.Term t)) Int

instance Eq (StableTermHeight t) where
  (StableTermHeight sn1 _) == (StableTermHeight sn2 _) = sn1 == sn2

higherSTH :: StableTermHeight t1 -> StableTermHeight t2 -> Bool
StableTermHeight _ h1 `higherSTH` StableTermHeight _ h2 = h1 > h2

hashStableTermHeight :: StableTermHeight t -> Int
hashStableTermHeight (StableTermHeight sn _) = hashStableName sn


-- Sharing information in terms
-- ----------------------------

-- Interleave sharing annotations into a term.  Subterms can be marked as being represented by
-- variable (binding a shared subtree) using 'VarSharing' and as being prefixed by a let binding
-- (for a shared subtree) using 'LetSharing'.
--
data SharingTerm binder t where
  VarSharing  :: Typeable t
              => StableTermHeight t                        -> SharingTerm binder t
  LetSharing  :: StableSharingTerm -> SharingTerm binder t -> SharingTerm binder t
  TermSharing :: Typeable t
              => StableTermHeight t -> Term binder t       -> SharingTerm binder t

-- Stable name for a term associated with its sharing-annotated version.
--
data StableSharingTerm where
  StableSharingTerm :: Typeable t => StableTermHeight t -> SharingTermFloated t -> StableSharingTerm

instance Show StableSharingTerm where
  show (StableSharingTerm sn _) = show $ hashStableTermHeight sn

instance Eq StableSharingTerm where
  StableSharingTerm sn1 _ == StableSharingTerm sn2 _
    | Just sn1' <- gcast sn1 = sn1' == sn2
    | otherwise              = False

higherSST :: StableSharingTerm -> StableSharingTerm -> Bool
StableSharingTerm sn1 _ `higherSST` StableSharingTerm sn2 _ = sn1 `higherSTH` sn2

-- Test whether the given stable names matches an array computation with sharing.
--
matchStableTerm :: Typeable t => StableTermHeight t -> StableSharingTerm -> Bool
matchStableTerm sn1 (StableSharingTerm sn2 _)
  | Just sn1' <- gcast sn1 = sn1' == sn2
  | otherwise              = False

-- Dummy entry for environments to be used for unused variables.
--
noStableTermName :: StableTermHeight t
noStableTermName = unsafePerformIO $ StableTermHeight <$> makeStableName undefined <*> pure 0


-- Sharing recovery
-- ----------------

-- Sharing terms after computing the occurence map and pruning repeated subtrees.
--
-- Lambdas are annotated with the level of their binder.
--
type SharingTermPruned  t = SharingTerm Level t

-- Sharing terms after sharing recovery and shared subtrees have been floated to their let-binding
-- positions.
--
-- Lambdas are annotated by the 'StableSharingTerm' identifying their binder.
--
type SharingTermFloated t = SharingTerm StableSharingTerm t

-- |Recover sharing information and annotate the HOAS AST with variable and let binding annotations.
--
-- NB: Strictly speaking, this function is not deterministic, as it uses stable pointers to
--     determine the sharing of subterms.  The stable pointer API does not guarantee its
--     completeness; i.e., it may miss some equalities, which implies that we may fail to discover
--     some sharing.  However, sharing does not affect the denotational meaning of a term; hence,
--     we do not compromise denotational correctness.
--
--     There is one caveat: We currently rely on 'Tag' leaves representing free variables to be
--       shared if any of them is used more than once.  If one is duplicated, the environment for
--       de Bruijn conversion will have a duplicate entry, and hence, be of the wrong size, which
--       is fatal. (The 'lookupStableTag' function will already bail out.)
--
recoverSharing :: Typeable t => HOAS.Term t -> SharingTermFloated t
{-# NOINLINE recoverSharing #-}
recoverSharing term
  = let (term', occMap) =
          unsafePerformIO $ do        -- to enable stable pointers; it's safe as explained above
          { (term', occMap) <- makeOccMap 0 term
          ; frozenOccMap    <- freezeOccMap occMap
          ; return (term', frozenOccMap)
          }
    in 
    determineScopes occMap term'

-- Compute the term occurence map, mark all nodes with stable names, and drop repeated occurences
-- of shared subterms (Phase One).
--
-- Note [Traversing functions and side effects]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- We need to descent into function bodies to build the 'OccMap' with all occurences in the
-- function bodies.  Due to the side effects in the construction of the occurence map and, more
-- importantly, the dependence of the second phase on /global/ occurence information, we may not
-- delay the body traversals by putting them under a lambda.  Hence, we apply each function, to
-- traverse its body and use a /dummy abstraction/ of the result.
--
-- For example, given a function 'f', we traverse 'f (Tag 0)', which yields a transformed body 'e'.
-- As the result of the traversal of the overall function, we use 'const e'.  Hence, it is crucial
-- that the 'Tag' supplied during the initial traversal is already the one required by the HOAS to
-- de Bruijn conversion in 'Convert.convertSharing' — any subsequent application of 'const e' will
-- only yield 'e' with the embedded 'Tag 0' of the original application.  During sharing recovery,
-- we float /all/ free variables ('Tag' terms) out to construct the initial environment for producing
-- de Bruijn indices, which replaces them by 'VarSharing' nodes.  Hence, the tag values only serve
-- the purpose of determining the ordering in that initial environment. They are /not/ directly used
-- to compute the de Brujin indices.
--
makeOccMap :: Typeable t => Level -> HOAS.Term t -> IO (SharingTermPruned t, OccMapHash)
makeOccMap lvl rootTerm
  = do
    { occMap <- newOccMapHashTable
    ; (rootTerm', _) <- traverse lvl occMap rootTerm
    ; return (rootTerm', occMap)
    }
  where
    traverse :: forall t. Typeable t
                => Level -> OccMapHash -> HOAS.Term t -> IO (SharingTermPruned t, Int)
    traverse lvl occMap term
      = mfix $ \ ~(_, height) -> do 
        {   -- Compute stable name and enter it into the occurence map
        ; sn                        <- makeStableTerm term
        ; heightIfRepeatedOccurence <- enterOcc occMap (StableTermName sn) height
          
        ; traceLine (HOAS.showTermOp term) $
            case heightIfRepeatedOccurence of
              Just height -> "REPEATED occurence (sn = " ++ show (hashStableName sn) ++ 
                             "; height = " ++ show height ++ ")"
              Nothing     -> "first occurence (sn = " ++ show (hashStableName sn) ++ ")"

            -- Reconstruct the term in shared form.
            --
            -- In case of a repeated occurence, the height comes from the occurence map; otherwise,
            -- it is computed by the traversal function passed in 'newTerm'.  See also 'enterOcc'.
            --
            -- NB: This function can only be used in the case alternatives below; outside of the
            --     case we cannot discharge the 'Typeable t' constraint.
        ; let reconstruct :: Typeable t
                          => IO (Term        Level t, Int)
                          -> IO (SharingTerm Level t, Int)
              reconstruct newTerm
                = case heightIfRepeatedOccurence of 
                    Just height 
                      -> return (VarSharing (StableTermHeight sn height), height)
                    _ -> do
                         { (term, height) <- newTerm
                         ; return (TermSharing (StableTermHeight sn height) term, height)
                         }

        ; case term of
            HOAS.Tag i   -> reconstruct $ return (Tag i, 0)           -- height is 0!
            HOAS.Con v   -> reconstruct $ return (Con v, 1)
            HOAS.Lam f   -> reconstruct $ do
                            {   -- see Note [Traversing functions and side effects]
                            ; (body, h) <- traverse (lvl + 1) occMap (f (HOAS.Tag lvl))
                            ; return (Lam lvl body, h + 1)
                            }
            HOAS.App f a -> reconstruct $ do
                            { (f', h1) <- traverse lvl occMap f
                            ; (a', h2) <- traverse lvl occMap a
                            ; return (App f' a', h1 `max` h2 + 1)
                            }
        }

-- Type used to maintain how often each shared subterm, so far, occured during a bottom-up sweep.
--
--   Invariants: 
--   - If one shared term 's' is itself a subterm of another shared term 't', then 's' must occur
--     *after* 't' in the 'NodeCounts'.
--   - No shared term occurs twice.
--   - A term may have a final occurence count of only 1 iff it is either a free variable ('Tag')
--     or an array computation listed out of an expression.
--
-- We determine the subterm property by using the tree height in 'StableTermHeight'.  Trees get
-- smaller towards the end of a 'NodeCounts' list.  The height of free variables ('Tag') is 0,
-- whereas other leaves have height 1.  This guarantees that all free variables are at the end
-- of the 'NodeCounts' list.
--
-- To ensure the invariant is preserved over merging node counts from sibling subterms, the
-- function '(+++)' must be used.
--
type NodeCounts = [NodeCount]

data NodeCount = NodeCount StableSharingTerm Int
               deriving Show

-- Empty node counts
--
noNodeCounts :: NodeCounts
noNodeCounts = []

-- Singleton node counts for 'Acc'
--
nodeCount :: StableSharingTerm -> Int -> NodeCounts
nodeCount sst n = [NodeCount sst n]

-- Combine node counts that belong to the same node.
--
-- * We assume that the node counts invariant —subterms follow their parents— holds for both
--   arguments and guarantee that it still holds for the result.
--
(+++) :: NodeCounts -> NodeCounts -> NodeCounts
us +++ vs = foldr insert us vs
  where
    insert x               []                         = [x]
    insert x@(NodeCount st1 count1) ys@(y@(NodeCount st2 count2) : ys') 
      | st1 == st2          = NodeCount (st1 `pickNoneVar` st2) (count1 + count2) : ys'
      | st1 `higherSST` st2 = x : ys
      | otherwise           = y : insert x ys'

    (StableSharingTerm _ (VarSharing _))  `pickNoneVar`  st2  = st2
    st1                                   `pickNoneVar`  _st2 = st1
    
-- Determine whether a 'NodeCount' is for a 'Tag', which represent free variables.
--
isFreeVar :: NodeCount -> Bool
isFreeVar (NodeCount (StableSharingTerm _ (TermSharing _ (Tag  _))) _) = True
isFreeVar _                                                            = False

-- Determine the scopes of all variables representing shared subterms (Phase Two) in a bottom-up
-- sweep.
--
-- Precondition: there are only 'VarSharing' and 'TermSharing' nodes in the argument.
--
determineScopes :: Typeable t
                => OccMap -> SharingTermPruned t -> SharingTermFloated t
determineScopes occMap rootTerm 
  = let
      (sharingTerm, counts) = scopes rootTerm
    in
    if null counts
    then sharingTerm
    else error $ "determineScopes: unbound shared subtrees: " ++ show counts
  where
    scopes :: forall t. SharingTermPruned t -> (SharingTermFloated t, NodeCounts)
    scopes (LetSharing _ _)
      = error $ "determineScopes: scopesAcc: unexpected 'LetSharing'"
    scopes (VarSharing sn)
      = (VarSharing sn, StableSharingTerm sn (VarSharing sn) `nodeCount` 1)
    scopes (TermSharing sn pterm)
      = case pterm of
          Tag i     -> reconstruct (Tag i) noNodeCounts
          Con v     -> reconstruct (Con v) noNodeCounts
          Lam lvl f -> let
                         (f', count)         = scopes f
                         (stableTag, count') = lookupStableTag lvl count
                       in
                       reconstruct (Lam stableTag f') count'
          App f a   -> let
                         (f', count1) = scopes f
                         (a', count2) = scopes a
                       in 
                       reconstruct (App f' a') (count1 +++ count2)
      where
          -- Occurence count of the currently processed node
        occCount = let StableTermHeight sn' _ = sn
                   in
                   lookupWithTermName occMap (StableTermName sn')

        -- Reconstruct the current tree node.
        --
        -- * If the current node is being shared ('occCount > 1'), replace it by a 'VarSharing'
        --   node and float the shared subtree out wrapped in a 'NodeCounts' value.
        -- * If the current node is not shared, reconstruct it in place.
        -- * Special case for free variables ('Tag'): Replace the tree by a sharing variable and
        --   float the 'Tag' out in a 'NodeCounts' value.  This is idependent of the number of
        --   occurences.
        --
        -- In either case, any completed 'NodeCounts' are injected as bindings using 'LetSharing'
        -- node.
        -- 
        reconstruct :: Typeable t 
                    => Term StableSharingTerm t -> NodeCounts -> (SharingTermFloated t, NodeCounts)
        reconstruct newTerm@(Tag _) _subCount
              -- free variable => replace by a sharing variable regardless of the number of occ.s
          = let thisCount = StableSharingTerm sn (TermSharing sn newTerm) `nodeCount` 1
            in
            tracePure "FREE" (show thisCount) $
            (VarSharing sn, thisCount)
        reconstruct newTerm subCount
              -- shared subtree => replace by a sharing variable
          | occCount > 1
          = let allCount = (StableSharingTerm sn sharingTerm `nodeCount` 1) +++ newCount
            in
            tracePure ("SHARED" ++ completed) (show allCount) $
            (VarSharing sn, allCount)
              -- neither shared nor free variable => leave it as it is
          | otherwise
          = tracePure ("Normal" ++ completed) (show newCount) $
            (sharingTerm, newCount)
          where
              -- Determine the bindings that needs to be attached to the current node...
            (newCount, bindHere) = filterCompleted subCount
    
              -- ...and wrap them in 'LetSharing' constructors
            lets        = foldl (flip (.)) id . map LetSharing $ bindHere
            sharingTerm = lets $ TermSharing sn newTerm
    
              -- trace support
            completed | null bindHere = ""
                      | otherwise     = "(" ++ show (length bindHere) ++ " lets)"
    
        -- Extract *leading* nodes that have a complete node count (i.e., their node count is equal
        -- to the number of occurences of that node in the overall expression).
        -- 
        -- Nodes with a completed node count should be let bound at the currently processed node.
        --
        -- NB: Only extract leading nodes (i.e., the longest run at the *front* of the list that is
        --     complete).  Otherwise, we would let-bind subterms before their parents, which leads
        --     to scope errors.
        --
        filterCompleted :: NodeCounts -> (NodeCounts, [StableSharingTerm])
        filterCompleted counts
          = let (completed, counts') = break notComplete counts
            in (counts', [sa | NodeCount sa _ <- completed])
          where
            -- a node is not yet complete while the node count 'n' is below the overall number
            -- of occurences for that node in the whole program, with the exception that free
            -- variables are never complete
            notComplete nc@(NodeCount st n) | not . isFreeVar $ nc = lookupWithSharingTerm occMap st > n
            notComplete _                                          = True

-- Find the stable tag representing the binder at the given level (which equals the tag value).
--
-- If there is not such tag, the binder has no usage occurence.
--
-- if the tag occurs multiple times, sharing of the binder tag was not preserved and we cannot
-- continue (c.f., comments at 'determineScopes').
-- 
lookupStableTag :: Level -> NodeCounts -> (StableSharingTerm, NodeCounts)
lookupStableTag lvl counts 
  = case partition hasLevel counts of
      ([], _)                     -> (noStableSharing, counts)  -- tag not used in expression
      ([NodeCount st _], counts') -> (st, counts')              -- tag has a unique occurence
      (counts', _)    -> 
        error $ "lookupStableTag: duplicate 'Tag's\n  " ++ 
                intercalate ", " [showST st | NodeCount st _ <- counts']
  where
    hasLevel (NodeCount (StableSharingTerm _ (TermSharing _ (Tag lvl'))) _) = lvl == lvl'
    hasLevel (NodeCount st _)                                               = False
        
    noStableSharing :: StableSharingTerm
    noStableSharing = StableSharingTerm noStableTermName (undefined :: SharingTermFloated ())

    showST (StableSharingTerm _ (TermSharing  sn term)) = show (hashStableTermHeight sn) ++ ": " ++ 
                                                          showTermOp term
    showST (StableSharingTerm _ (VarSharing sn))        = "VarSharing " ++ show (hashStableTermHeight sn)
    showST (StableSharingTerm _ (LetSharing st _ ))     = "LetSharing " ++ show st ++ "..."


-- Debugging
-- ---------

traceOn :: Bool
traceOn = False

tracePure :: String -> String -> a -> a
tracePure header msg val | traceOn   = trace (header ++ ": " ++ msg) val
                         | otherwise = val

traceLine :: String -> String -> IO ()
traceLine header msg = tracePure header msg $ return ()
