{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Data.Typeable

import qualified DeBruijn
import qualified HOAS
import Convert

i = HOAS.lam $ \x -> x

zero  = HOAS.lam $ \f -> HOAS.lam $ \x -> x
one   = HOAS.lam $ \f -> HOAS.lam $ \x -> f `HOAS.app` x
two   = HOAS.lam $ \f -> HOAS.lam $ \x -> f `HOAS.app` (f `HOAS.app` x)
three = HOAS.lam $ \f -> HOAS.lam $ \x -> 
          f `HOAS.app` (f `HOAS.app` (f `HOAS.app` x))

plus = HOAS.lam $ \m -> 
       HOAS.lam $ \n -> 
       HOAS.lam $ \f -> 
       HOAS.lam $ \x -> m `HOAS.app` f `HOAS.app` (n `HOAS.app` f `HOAS.app` x)

plusTwoThree = plus `HOAS.app` two `HOAS.app` three

data Nat = Z | S Nat deriving (Show, Typeable)

pair = HOAS.lam $ \x -> 
       HOAS.lam $ \y -> 
       HOAS.lam $ \z -> z `HOAS.app` x `HOAS.app` y
pairfst = HOAS.lam $ \p -> p `HOAS.app` (HOAS.lam $ \x -> HOAS.lam $ \y -> x)
pairsnd = HOAS.lam $ \p -> p `HOAS.app` (HOAS.lam $ \x -> HOAS.lam $ \y -> y)

pairfstPair = pairfst `HOAS.app` 
                (pair `HOAS.app` (HOAS.con 'a') `HOAS.app` (HOAS.con 'b'))

main 
  = do
      printLine "Identity           :"     (convert i)
      printLine "zero               :"     (convert zero)
      printLine "one                :"     (convert one)
      printLine "two                :"     (convert two)
      printLine "three              :"     (convert three)
      printLine "plus               :"     (convert plus)
      printLine "plus two three     :\n  " (convert plusTwoThree)
      printLine' "EVAL plus two three:" 
        ((DeBruijn.intp (convert plusTwoThree)) DeBruijn.Empty S Z)
      printLine "pairfst (pair 'a' 'b'):\n  " (convert pairfstPair)
      printLine' "EVAL pairfst (pair 'a' 'b'):"   
        (DeBruijn.intp (convert pairfstPair) DeBruijn.Empty)
  where
    printLine  desc e = putStrLn $ desc ++ " " ++ show e ++ "    — " ++ DeBruijn.pprTerm e
    printLine' desc e = putStrLn $ desc ++ " " ++ show e
