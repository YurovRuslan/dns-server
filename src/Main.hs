module Main where
import Control.Monad (unless)
import Network.Socket hiding (recvFrom, sendTo)
import Network.Socket.ByteString
import Network.DNS.Decode
import Control.Exception
import Control.Monad
port = "3000"
main = withSocketsDo $ bracket establishSocket serveDNS releaseSocket

establishSocket :: IO Socket
establishSocket = do
    (serveraddr:_) <- getAddrInfo
        (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
        Nothing (Just port)
    sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
    bind sock (addrAddress serveraddr) >> return sock

serveDNS :: Socket -> IO ()
serveDNS sock = forever $ do
    dnsMsg <- receive sock
    print $ show dnsMsg
    return ()

releaseSocket :: Socket -> IO ()
releaseSocket sock = close

