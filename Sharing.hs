{-# LANGUAGE GADTs, StandaloneDeriving, DeriveDataTypeable #-}

module Sharing where

import Control.Applicative                      hiding (Const)
import Data.HashTable                           as Hash
import qualified Data.IntMap                    as IntMap
import System.IO.Unsafe                         (unsafePerformIO)
import Data.List
import Data.Maybe
import Data.Typeable
import System.Mem.StableName

import HOAS


-- Opaque stable name for term nodes — used to key the occurence map.
--
data StableTermName where
  StableTermName :: Typeable t => StableName (Term t) -> StableTermName

instance Show StableTermName where
  show (StableTermName sn) = show $ hashStableName sn

instance Eq StableTermName where
  StableTermName sn1 == StableTermName sn2
    | Just sn1' <- gcast sn1 = sn1' == sn2
    | otherwise              = False

makeStableTerm :: Term t -> IO (StableName (Term t))
makeStableTerm e = e `seq` makeStableName e

-- Stable name for a term node including the height of the term.
--
data StableNameHeight t = StableNameHeight (StableName t) Int

instance Eq (StableNameHeight t) where
  (StableNameHeight sn1 _) == (StableNameHeight sn2 _) = sn1 == sn2

higherSNH :: StableNameHeight t1 -> StableNameHeight t2 -> Bool
StableNameHeight _ h1 `higherSNH` StableNameHeight _ h2 = h1 > h2

hashStableNameHeight :: StableNameHeight t -> Int
hashStableNameHeight (StableNameHeight sn _) = hashStableName sn

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
lookupWithSharingTerm oc (StableSharingTerm (StableNameHeight sn _) _) 
  = lookupWithTermName oc (StableTermName sn)

-- Stable term nodes

-- Stable name for term nodes including the height of the term.
--
type StableTermNameHeight t = StableNameHeight (Term t)

-- Interleave sharing annotations into a term.  Subterms can be marked as being represented by
-- variable (binding a shared subtree) using 'AvarSharing' and as being prefixed by a let binding
-- (for a shared subtree) using 'AletSharing'.
--
data SharingTerm t where
  AvarSharing :: StableTermNameHeight t                          -> SharingTerm t
  AletSharing :: StableSharingTerm -> SharingTerm t              -> SharingTerm t
  AccSharing  :: StableTermNameHeight t -> PreTerm SharingTerm t -> SharingTerm t

-- Stable name for a term associated with its sharing-annotated version.
--
data StableSharingTerm where
  StableSharingTerm :: Typeable t => StableTermNameHeight t -> SharingTerm t -> StableSharingTerm

instance Show StableSharingTerm where
  show (StableSharingTerm sn _) = show $ hashStableNameHeight sn

instance Eq StableSharingTerm where
  StableSharingTerm sn1 _ == StableSharingTerm sn2 _
    | Just sn1' <- gcast sn1 = sn1' == sn2
    | otherwise              = False

higherSST :: StableSharingTerm -> StableSharingTerm -> Bool
StableSharingTerm sn1 _ `higherSST` StableSharingTerm sn2 _ = sn1 `higherSNH` sn2

-- Test whether the given stable names matches an array computation with sharing.
--
matchStableTerm :: Typeable t => StableTermNameHeight t -> StableSharingTerm -> Bool
matchStableTerm sn1 (StableSharingTerm sn2 _)
  | Just sn1' <- gcast sn1 = sn1' == sn2
  | otherwise              = False

-- Dummy entry for environments to be used for unused variables.
--
noStableTermName :: StableTermNameHeight t
noStableTermName = unsafePerformIO $ StableNameHeight <$> makeStableName undefined <*> pure 0
