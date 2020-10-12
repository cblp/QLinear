{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Internal.Quasi.Matrix.Pattern.Quote (pat) where

import Data.Char (isSpace)
import Data.Proxy (Proxy (..))
import GHC.TypeNats (Nat)
import Language.Haskell.TH.Lib (listP, litT, numTyLit, varP)
import Language.Haskell.TH.Syntax (Pat, Q, mkName)

import Internal.Matrix (Matrix (..))

-- DRAFT. VERY DIRTY AND VERY RAW DRAFT

data MatrixPat (m :: Nat) (n :: Nat) a = MatrixPat (Proxy m) (Proxy n) [[a]]

matrixPatHelper :: Matrix m n a -> MatrixPat m n a
matrixPatHelper (Matrix _ elems) = MatrixPat Proxy Proxy elems

pat :: String -> Q Pat
pat raw =
  [p|
    (matrixPatHelper ->
      MatrixPat (Proxy :: Proxy $height) (Proxy :: Proxy $width) $ps)
  |]
  where
    ls = map trim $ split ';' raw
    height = litT $ numTyLit $ fromIntegral $ length ls
    width = litT $ numTyLit $ fromIntegral $ length $ head ls
    ps = listP $ map (listP . map (varP . mkName) . words) ls

split :: Char -> String -> [String]
split sep = reverse . go [] [] where
  go res [] [] = res
  go res str [] = reverse str : res
  go res str (c:cs)
    | c == sep = go (reverse str: res) [] cs
    | otherwise = go res (c:str) cs

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
