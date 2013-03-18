{-# LANGUAGE GADTs, ScopedTypeVariables #-}

module Convert (convert, convertSharing) where

  -- standard libraries
import Data.List
import Data.Typeable
import Data.Maybe

  -- friends
import qualified DeBruijn
import qualified HOAS
import Sharing


-- A layout of an environment an entry for each entry of the environment.
-- Each entry in the layout holds the deBruijn index that refers to the
-- corresponding entry in the environment.
--
data Layout env env' where
  EmptyLayout :: Layout env ()
  PushLayout  :: Typeable t 
              => Layout env env' -> DeBruijn.Idx env t -> Layout env (env', t)

-- Yield the number of entries in an environment layout
--
size :: Layout env env' -> Int
size EmptyLayout        = 0
size (PushLayout lyt _) = size lyt + 1

-- Add an entry to a layout, incrementing all indices
--
inc :: Layout env env' -> Layout (env, t) env'
inc EmptyLayout         = EmptyLayout
inc (PushLayout lyt ix) = PushLayout (inc lyt) (DeBruijn.SuccIdx ix)

-- Project the nth index out of an environment layout.
--
-- All errors are internal errors.
--
prjIdx :: forall t env env'. Typeable t => String -> Int -> Layout env env' -> DeBruijn.Idx env t
prjIdx ctxt 0 (PushLayout _ (idx :: DeBruijn.Idx env0 t0))
  = case gcast idx of
      Just idx' -> idx'
      Nothing   -> 
        error $ "Convert.prjIdx: type mismatch at " ++ ctxt ++ "\n  " ++
                "Couldn't match expected type `" ++ show (typeOf (undefined::t)) ++ 
                "' with actual type `" ++ show (typeOf (undefined::t0)) ++ "'"
prjIdx ctxt n (PushLayout l _)  = prjIdx ctxt (n - 1) l
prjIdx ctxt _ EmptyLayout       = error $ "Convert.prjIdx: environment doesn't contain index at " ++ ctxt

-- |Convert a closed HOAS term to a closed de Bruijn term.
--
convert :: HOAS.Term t -> DeBruijn.Term () t
convert = cvt EmptyLayout
  where
    cvt :: Layout env env -> HOAS.Term t -> DeBruijn.Term env t
    cvt lyt (HOAS.Tag sz)      = DeBruijn.Var (prjIdx "'Tag' in vanilla convert" (size lyt - sz - 1) lyt)
    cvt lyt (HOAS.Con v)       = DeBruijn.Con v
    cvt lyt (HOAS.Lam f)       = DeBruijn.Lam (cvt lyt' (f tag))
      where
        tag  = HOAS.Tag (size lyt)
        lyt' = inc lyt `PushLayout` DeBruijn.ZeroIdx
    cvt lyt (HOAS.App fun arg) = DeBruijn.App (cvt lyt fun) (cvt lyt arg)

-- |Convert a closed HOAS term to a closed de Bruijn term while recovering the sharing of the
-- source expression.
--
convertSharing :: Typeable t => HOAS.Term t -> DeBruijn.Term () t
convertSharing = cvt EmptyLayout [] . recoverSharing
  where
    cvt :: Layout env env -> [StableSharingTerm] -> SharingTermFloated t -> DeBruijn.Term env t
    cvt lyt env (VarSharing st)
      | Just i <- findIndex (matchStableTerm st) env
      = DeBruijn.Var (prjIdx (ctxt ++ "; i = " ++ show i) i lyt)
      | null env
      = error $ "Cyclic definition of a term (st = " ++ show (hashStableTermHeight st) ++ ")"
      | otherwise                                   
      = error $ "convertSharing: " ++ err
      where
        ctxt = "shared term with stable name " ++ show (hashStableTermHeight st)
        err  = "inconsistent valuation @ " ++ ctxt ++ ";\n  env = " ++ show env
    cvt lyt env (LetSharing st@(StableSharingTerm _ boundTerm) bodyTerm)
      = let lyt' = inc lyt `PushLayout` DeBruijn.ZeroIdx
        in
        DeBruijn.Let (cvt lyt env boundTerm) (cvt lyt' (st:env) bodyTerm)
    cvt lyt env (TermSharing _ (Tag lvl)) 
      = DeBruijn.Var (prjIdx ("de Bruijn conversion tag " ++ show lvl) lvl lyt)
    cvt lyt env (TermSharing _ (Con v))
      = DeBruijn.Con v
    cvt lyt env (TermSharing _ (Lam sts f))
      = DeBruijn.Lam (cvt lyt' env' f)
      where
        lyt' = inc lyt `PushLayout` DeBruijn.ZeroIdx
        env' = sts:env
    cvt lyt env (TermSharing _ (App fun arg)) 
      = DeBruijn.App (cvt lyt env fun) (cvt lyt env arg)
