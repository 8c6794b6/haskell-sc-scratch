{-# LANGUAGE TypeFamilies, TypeSynonymInstances #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : non-portable (TypeFamilies, TypeSynonymInstances)

Module to count size of expression node in haskell source code.
-}
module Language.Haskell.CodeSize
  ( ExprNum(..)
  , Src
  , src
  , countExpressions
  ) where

import Language.Haskell.Exts
import Data.Generics

-- | Wrapper to handle FilePath as path for source code.
newtype Src = Src FilePath

-- | Lifter for source code.
src :: FilePath -> Src
src = Src

-- | Counts number of expressions from given source.
class ExprNum s where
  type Count s :: *
  count :: s -> Count s

-- | Count expressions in raw string.
instance ExprNum String where
  type Count String = Int
  count code = case parseModule code of
    ParseFailed loc err -> error $ unwords [show loc, err]
    ParseOk mdl         -> count mdl

-- | Count expression in source code.
instance ExprNum Src where
  type Count Src = IO Int
  count (Src path) = do
    res <- parseFile path
    case res of
      ParseFailed loc err -> error $ unwords [show loc, err]
      ParseOk mdl         -> return $ count mdl

-- | Count expressions in Module.
-- Does NOT include module hearder, imports and exports.
instance ExprNum Module where
  type Count Module = Int
  count = countExpressions


-- | Count number of expression in given Module.
countExpressions :: Module -> Int
countExpressions = everything (+) (const 0 `extQ` lits `extQ` names `extQ` spcon)

-- | Haskell literal. Increment by 1.
lits :: Literal -> Int
lits _ = 1

-- | Haskell name. Increment by 1.
names :: Name -> Int
names _ = 1

-- | Builtin special constructors, e.g. [], (). Increment by 1.
spcon :: SpecialCon -> Int
spcon _ = 1
