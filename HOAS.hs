{-# LANGUAGE GADTs, StandaloneDeriving, DeriveDataTypeable #-}

module HOAS where

import Data.Typeable
import Text.Show.Functions


-- The level of lambda-bound variables. The root has level 0; then it increases with each bound
-- variable — i.e., it is the same as the size of the environment at the defining occurence.
--
type Level = Int

-- Open lambda terms (without a recursive knot) using Haskell functions to represent functionals.
--
-- * We don't care about exotic terms here, and hence don't use a parametrised representation.
-- * The `Typeable' contexts and the `Tag' variant are in preparation for being able to convert to a
--   de Bruijn representation.
--
data PreTerm term t where
    -- for conversion to de Bruijn
  Tag :: Typeable t                               => Level                   -> PreTerm term t    

  Con :: (Typeable t, Show t)                     => t                       -> PreTerm term t
  Lam :: (Typeable s, Typeable t, Show s, Show t) => (Term s -> term t)      -> PreTerm term (s -> t)
  App :: (Typeable s, Typeable t, Show s, Show t) => term (s -> t) -> term s -> PreTerm term t

-- Closed vanilla terms to represent source terms
--
newtype Term t = Term (PreTerm Term t)

deriving instance Typeable1 Term

-- Term constructors
--
con c     = Term $ Con c
lam f     = Term $ Lam f
f `app` a = Term $  f `App` a

-- A term interpreter for closed terms
--
intp :: Show t => Term t -> t
intp (Term (Tag ix))      = error "HOAS.intp: Tag is only for conversion"
intp (Term (Con v))       = v
intp (Term (Lam fun))     = intp . fun . Term . Con
intp (Term (App fun arg)) = (intp fun) (intp arg)
