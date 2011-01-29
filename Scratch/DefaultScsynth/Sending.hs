{-# LANGUAGE PackageImports #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sending where

import Control.Exception (bracket)
import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.Lepton
import "mtl" Control.Monad.Reader

newtype SC a = SC {unSC :: ReaderT UDP IO a}
                deriving (Functor, Monad, MonadIO, MonadReader UDP)

instance Show (SC a) where
  show _ = "<SC>"

sc :: SC a -> IO a
sc x = bracket (openUDP "127.0.0.1" 57110) close (runReaderT (unSC x))

tran :: OSC -> SC ()
tran msg = do
  con <- ask
  liftIO $ send con msg

stay :: String -> SC OSC
stay str = do
  fd <- ask
  liftIO $ wait fd str

asyn :: OSC -> SC OSC
asyn msg = tran msg >> stay "/done"

fromUDP :: (UDP -> IO a) -> SC a
fromUDP act = do
  con <- ask
  liftIO $ act con

simpleSine :: UGen
simpleSine = out 0 $ pan2 sig pan 1
  where
    sig = sinOsc ar freq 0 * amp
    pan = ctrl "pan" 0
    freq = ctrl "freq" 440
    amp = ctrl "amp" 0.2

simpleLine :: UGen
simpleLine = out outBus sig
  where
    outBus = ctrl "out" 1000
    sig = line kr str end dur RemoveSynth
    str = ctrl "str" 0
    end = ctrl "end" 0
    dur = ctrl "dur" 1

setLin :: (Transport t) => Int -> String -> Double -> Double -> t -> IO ()
setLin i n t v fd = do
  send fd $ c_get [1000]
  Message "/c_set" [_,Float str] <- wait fd "/c_set"
  send fd $ s_new "simpleLine" (-1) AddToHead 1
    [("str",str),("end",v),("dur",t),("out",fromIntegral i)]
  send fd $ n_map i [("freq",fromIntegral i)]
