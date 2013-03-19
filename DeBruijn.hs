{-# LANGUAGE GADTs #-}

module DeBruijn where

import Text.Show.Functions

import Data.Char


-- Type environments are nested pairs (..((), t1), t2, ..., tn)

-- Index projecting a specific type from a type environment; it's value
-- corresponds to a natural number, the de Brujin index
--
data Idx env t where
  ZeroIdx ::              Idx (env, t) t
  SuccIdx :: Idx env t -> Idx (env, s) t

instance Show (Idx env t) where
  show = show . idxToInt

idxToInt :: Idx env t -> Int
idxToInt ZeroIdx     = 0
idxToInt (SuccIdx n) = idxToInt n + 1

-- Lambda terms using de Bruijn indices to represent variables
--
data Term env t where
  Var :: Idx env t                       -> Term env t
  Con :: Show t
      => t                               -> Term env t
  Lam :: Term (env, s) t                 -> Term env (s -> t)
  App :: Term env (s -> t) -> Term env s -> Term env t
  Let :: Term env s -> Term (env, s) t   -> Term env t

instance Show (Term env t) where
  show = showTerm
    where
      showTerm (Var ix)       = "#" ++ show ix
      showTerm (Con c)        = show c
      showTerm (Lam body)     = "\\" ++ show body
      showTerm (App fun arg)  = showParen fun ++ " " ++ showParen arg
      showTerm (Let bnd body) = "let " ++ show bnd ++ " in " ++ show body
      
      showParen t@(Var {}) = show t
      showParen t@(Con {}) = show t
      showParen t          = "(" ++ show t ++ ")"

pprTerm :: Term env t -> String
pprTerm = ppr (-1)
  where
    ppr :: Int -> Term env t -> String
    ppr lvl (Var ix)      = pprIdx lvl ix
    ppr lvl (Con c)       = show c
    ppr lvl (Lam body)    = "\\" ++ pprIdx (lvl + 1) ZeroIdx ++ ". " ++ ppr (lvl + 1) body
    ppr lvl (App fun arg) = pprParen lvl fun ++ " " ++ pprParen lvl arg
    ppr lvl (Let bnd body) = "let " ++ pprIdx (lvl + 1) ZeroIdx ++ " = " ++ ppr lvl bnd ++
                             " in " ++ ppr (lvl + 1) body
    
    pprParen :: Int -> Term  env t -> String
    pprParen lvl t@(Var {}) = ppr lvl t
    pprParen lvl t@(Con {}) = ppr lvl t
    pprParen lvl t          = "(" ++ ppr lvl t ++ ")"
    
    pprIdx :: Int -> Idx env t -> String
    pprIdx lvl idx
      | n < 26    = [chr (ord 'a' + n)]
      | otherwise = 'v':show n
      where
        n = lvl - idxToInt idx

-- Valuation for a type environment
--
data Val env where
  Empty :: Val ()
  Push  :: Val env -> t -> Val (env, t)

-- Projection of a value from a valuation using a de Bruijn index
--
prj :: Idx env t -> Val env -> t
prj ZeroIdx       (Push val v) = v
prj (SuccIdx idx) (Push val _) = prj idx val

-- A term interpreter, evaluating a term under a valuation
--
intp :: Term env t -> Val env -> t
intp (Var ix)       val = prj ix val
intp (Con v)        val = v
intp (Lam body)     val = intp body . (val `Push`)
intp (App fun arg)  val = (intp fun val) (intp arg val)
intp (Let bnd body) val = intp body (val `Push` intp bnd val)
