------------------------------------------------------------------------------
-- | Playing with network. Trying to implement something to respond
-- osc message in client side.
--
-- Client side (in ghci):
-- 
-- > > h <- openlog "localhost" "514" "testprog"
-- > > syslog h USER INFO "This is my message"
-- > > closelog h
--
-- Server side:
-- 
-- > serveLog "514" plainHandler
-- 

module Scheduling where

import Control.Monad
import Data.Bits
import Data.List
import Network.Socket
import Network.BSD
import qualified Sound.OpenSoundControl as O

--
-- From realworldhaskell's chapter 27, "Sockets and Syslog"
--

data SyslogHandle
    = SyslogHandle {slSocket :: Socket,
                    slProgram :: String,
                    slAddress :: SockAddr }

openlog :: HostName
        -> String
        -> String
        -> IO SyslogHandle
openlog hostname port progname =
    do addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
       let serveraddr =  head addrinfos

       sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
       return $ SyslogHandle sock progname (addrAddress serveraddr)


syslog :: SyslogHandle -> Facility -> Priority -> String -> IO ()
syslog syslogh fac pri msg =
    sendstr sendmsg
    where code = makeCode fac pri
          sendmsg = "<" ++ show code ++ ">" ++ (slProgram syslogh) ++
                    ":" ++ msg
          sendstr :: String -> IO ()
          sendstr [] = return ()
          sendstr omsg = do sent <- sendTo (slSocket syslogh) omsg
                                    (slAddress syslogh)
                            sendstr (genericDrop sent omsg)

closelog :: SyslogHandle -> IO ()
closelog syslogh = sClose (slSocket syslogh)

makeCode :: Facility -> Priority -> Int
makeCode fac pri =
    let faccode = codeOfFac fac
        pricode = fromEnum pri
    in  (faccode `shiftL` 3) .|. pricode


-- SyslogTypes.hs

data Priority = DEBUG
              | INFO
              | NOTICE
              | WARNING
              | ERROR
              | CRITICAL
              | ALERT
              | EMERGENCY
                deriving (Eq, Ord, Show, Read, Enum)

data Facility = KERN
              | USER
              | MAIL
              | DAEMON
              | AUTH
              | SYSLOG
              | LPR
              | NEWS
              | UUCP
              | CRON
              | AUTHPRIV
              | FTP
              | LOCAL0
              | LOCAL1
              | LOCAL2
              | LOCAL3
              | LOCAL4
              | LOCAL5
              | LOCAL6
              | LOCAL7
                deriving (Eq, Show, Read)

facToCode =
    [ (KERN,0),
      (USER,1),
      (MAIL,2),
      (DAEMON,3),
      (AUTH,4),
      (SYSLOG,5),
      (LPR,6),
      (NEWS,7),
      (UUCP,8),
      (CRON,9),
      (AUTHPRIV,10),
      (FTP,11),
      (LOCAL0,16),
      (LOCAL1,17),
      (LOCAL2,18),
      (LOCAL3,19),
      (LOCAL4,20),
      (LOCAL5,21),
      (LOCAL6,22),
      (LOCAL7,23)
    ]

codeToFac = map (\(x,y) -> (y,x)) facToCode

codeOfFac :: Facility -> Int
codeOfFac f = case lookup f facToCode of
                Just x -> x
                _ -> error $ "Internal error in codeOfFac"

facOfCode :: Int -> Facility
facOfCode f = case lookup f codeToFac of
                Just x -> x
                _ -> error $ "Invalid code in facOfCode"


-- syslogserver

type HandlerFunc = SockAddr -> String -> IO ()

serveLog :: String -> HandlerFunc -> IO ()
serveLog port handlerfunc = withSocketsDo $
  do addrinfos <- getAddrInfo
                  (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                  Nothing (Just port)
     let serveraddr = head addrinfos
     sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
     bindSocket sock (addrAddress serveraddr)
     procMessages sock
    where
      procMessages sock =
          do (msg,_,addr) <- recvFrom sock 1024
             handlerfunc addr msg
             procMessages sock

plainHandler :: HandlerFunc
plainHandler addr msg =
    putStrLn $ "From " ++ show addr ++ ": " ++ msg
