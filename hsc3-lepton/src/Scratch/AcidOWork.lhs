Scratch to run code written in AcidO from emacs.

\begin{code}

module Scratch.AcidOWork where
 
import Sound.SC3
import Sound.SC3.Lepton
import Scratch.AcidO

\end{code}

Setting up

withSC3 reset
withSC3 $ flip send $ dumpOSC HexPrinter
withSC3 $ flip send $ dumpOSC NoPrinter
withSC3 $ flip send $ clearSched

leptseq l_freeAll
leptseq $ l_free "dseq"

withSC3 printRootNode
leptseq l_dump

setup'acido

  setB
  setD dseq0
  setD dseq1
  setD dseq2
  setD dseq3

setB >> setD dseq3
setB1 >> setD dseq3
setB2
