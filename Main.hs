{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Data.Typeable

import qualified DeBruijn
import qualified HOAS
import HOAS (con, lam, app)
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

plusTwoTwoLet = let a = two in plus `app` a `app` a

data Nat = Z | S Nat deriving (Show, Typeable)

pair = HOAS.lam $ \x -> 
       HOAS.lam $ \y -> 
       HOAS.lam $ \z -> z `HOAS.app` x `HOAS.app` y
pairfst = HOAS.lam $ \p -> p `HOAS.app` (HOAS.lam $ \x -> HOAS.lam $ \y -> x)
pairsnd = HOAS.lam $ \p -> p `HOAS.app` (HOAS.lam $ \x -> HOAS.lam $ \y -> y)

pairfstPair = pairfst `HOAS.app` 
                (pair `HOAS.app` (HOAS.con 'a') `HOAS.app` (HOAS.con 'b'))

testSharing :: (Eq t, Show t, Typeable t) => HOAS.Term t -> IO ()
testSharing t
  | sharingResult == nonSharingResult 
  = putStrLn $ DeBruijn.pprTerm (convertSharing t) ++ ": OK"
  | otherwise
  = do
    { putStrLn $ DeBruijn.pprTerm (convert t) ++ ": OK"
    ; putStrLn $ "  Without sharing: " ++ show nonSharingResult
    ; putStrLn $ "  With sharing   : " ++ show sharingResult
    }
  where
    nonSharingResult = DeBruijn.intp (convert t)        DeBruijn.Empty
    sharingResult    = DeBruijn.intp (convertSharing t) DeBruijn.Empty

main 
  = do
      printLine "Identity               :"     (convert i)
      printLine "zero                   :"     (convert zero)
      printLine "one                    :"     (convert one)
      printLine "two                    :"     (convert two)
      printLine "three                  :"     (convert three)
      printLine "plus                   :"     (convert plus)
      printLine "plus two three         :\n  " (convert plusTwoThree)
      printLine "let x = two in plus x x:\n  " (convert plusTwoTwoLet)
      printLine "let x = two in plus x x:\n  " (convertSharing plusTwoTwoLet)
      putStrLn  "  (with sharing)"
      printLine' "EVAL plus two three:" 
        ((DeBruijn.intp (convert plusTwoThree)) DeBruijn.Empty S Z)
      printLine "pairfst (pair 'a' 'b'):\n  " (convert pairfstPair)
      printLine' "EVAL pairfst (pair 'a' 'b'):"   
        (DeBruijn.intp (convert pairfstPair) DeBruijn.Empty)

        -- [too polymorphic: let y = \a -> (1::Int) in let x = let z = (\c d -> c) y in z z in x ((\b -> x) y)]
        -- let y = \a -> 1 in let x = let z = (\c -> c) y in (\d -> z) z in (\e -> x 2) ((\b -> x) y)
      testSharing (let    y = lam $ \a -> con (1::Int) 
                   in let x = let z = (lam $ \c -> c) `app` y in (lam $ \d -> z) `app` z
                   in
                   (lam $ \e -> x `app` con (2::Int)) `app` ((lam $ \b -> x) `app` y))
      testSharing (let inc = con (+) `app` con 1
                   in let nine = let three = inc `app` con 2 in (con (*)) `app` three `app` three
                   in
                   con (-) `app` (inc `app` nine) `app` nine)
      testSharing (let plus = lam $ \x -> lam $ \y -> let z = con (*) `app` x `app` y 
                                                      in  con (+) `app` z `app` z 
                   in let inc = plus `app` con 1 
                   in 
                   plus `app` (inc `app` (inc `app` con 1)) `app` con 42)
  where
    printLine  desc e = putStrLn $ desc ++ " " ++ show e ++ "    — " ++ DeBruijn.pprTerm e
    printLine' desc e = putStrLn $ desc ++ " " ++ show e
