{-|

Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Module for defining quasiquoter for heredoc. Inspired from:

* <http://www.haskell.org/haskellwiki/Poor_man%27s_here_document>

-}
module SynthDB.QHereDoc (hereDoc) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

hereDoc :: QuasiQuoter
hereDoc = QuasiQuoter stringE (litP . stringL) undefined (const $ return [])