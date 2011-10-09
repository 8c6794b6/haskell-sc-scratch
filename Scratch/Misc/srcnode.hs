{-|
Module      : $Header$
License     : BSD3
Stability   : unstable
Portability : portable

Module to count number of expressions in haskell sorce code.
Counts 'Name' element in given haskell src path from command line.
Multiple files could be passed as command line argument.

-}
module Main where

import Data.Generics (everything, mkQ)
import System.Environment (getArgs)
import Language.Haskell.Exts

main :: IO ()
main = mapM_ go =<< getArgs

-- | Guts of expression node counter. 
-- Read contents of given file path, parse it, and count expressions.
go :: FilePath -> IO ()
go filename = do
  contents <- readFile filename
  case parseWithMode myParseMode contents of
    ParseOk a ->
      putStrLn (show (exprSize a) ++ "\t" ++ filename)
    ParseFailed loc err -> 
      putStrLn $ unwords [prettyPrint (loc {srcFilename=filename}),err]
    
-- | Counts number of expressions in Module.
exprSize :: Module -> Int
exprSize = everything (+) (0 `mkQ` exprUnit)

-- | Constantly returns 1. 
exprUnit :: Name -> Int
exprUnit _ = 1
    
-- | Custom ParseMode used for parsing.
-- Enabling almost all language extensions.
myParseMode :: ParseMode
myParseMode = defaultParseMode {extensions = es, fixities = Nothing} where
 es =  
  [ OverlappingInstances
  , UndecidableInstances
  -- , IncoherentInstances
  , RecursiveDo
  , ParallelListComp
  , MultiParamTypeClasses
  , NoMonomorphismRestriction
  , FunctionalDependencies
  -- , ExplicitForall
  , Rank2Types
  , RankNTypes
  , PolymorphicComponents
  , ExistentialQuantification
  , ScopedTypeVariables
  -- , ImplicitParams
  , FlexibleContexts
  , FlexibleInstances
  , EmptyDataDecls
  , CPP
  , KindSignatures
  , BangPatterns
  , TypeSynonymInstances
  , TemplateHaskell
  , ForeignFunctionInterface
  , Arrows
  , Generics
  , NoImplicitPrelude
  , NamedFieldPuns
  , PatternGuards
  , GeneralizedNewtypeDeriving
  , ExtensibleRecords
  , RestrictedTypeSynonyms
  , HereDocuments
  , MagicHash
  , TypeFamilies
  , StandaloneDeriving
  , UnicodeSyntax
  , PatternSignatures
  , UnliftedFFITypes
  , LiberalTypeSynonyms
  , TypeOperators
  , RecordWildCards
  , RecordPuns
  , DisambiguateRecordFields
  , OverloadedStrings
  , GADTs
  , MonoPatBinds
  , NoMonoPatBinds
  , RelaxedPolyRec
  , ExtendedDefaultRules
  , UnboxedTuples
  , DeriveDataTypeable
  , ConstrainedClassMethods
  , PackageImports
  , ImpredicativeTypes
  , NewQualifiedOperators
  , PostfixOperators
  , QuasiQuotes
  , TransformListComp
  , ViewPatterns
  -- , XmlSyntax
  , RegularPatterns
  , TupleSections
  ]
