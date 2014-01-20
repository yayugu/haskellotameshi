import Network.Socket
import Network.BSD
import Data.List
import Data.Bits
import Data.String

import System.IO (Handle, IOMode(ReadWriteMode), hPutStrLn, hGetLine, hGetContents, hClose)
import Control.Monad (liftM)

mHostAddresses :: IO HostEntry -> IO [HostAddress]
mHostAddresses = liftM hostAddresses

addresses :: HostName -> IO [HostAddress]
addresses host = mHostAddresses $ getHostByName host

checkHost :: [HostAddress] -> HostName -> IO ()
checkHost addrs host =
    if null addrs
        then error $ "no such host : " ++ host
        else return ()

connectTo :: String -> Int -> IO Handle
connectTo host port_ = do
    let port = toEnum port_
    sock <- socket AF_INET Stream 0
    addrs <- addresses host
    checkHost addrs host
    connect sock $ SockAddrInet port (head addrs)
    handle <- socketToHandle sock ReadWriteMode
    return handle

getStats_ :: Handle -> String -> IO String
getStats_ h buffer = do
    text_ <- hGetLine h
    if text_ == "END\r"
        then return buffer
        else getStats_ h (buffer ++ "\n" ++ text_)

getStats :: Handle -> IO String
getStats h = do
    hPutStrLn h "stats"
    resultString <- getStats_ h ""
    return resultString

client :: IO ()
client = withSocketsDo $ do
    h <- connectTo "localhost" 11211
    str <- getStats h
    mapM_ putChar str
    hClose h

main :: IO ()
main = do
    client
