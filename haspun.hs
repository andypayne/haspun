module Main (main) where

import System.IO
import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as BSC
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Calendar as Cal
import qualified Data.Time.Format as TmFmt
import System.Directory (doesFileExist)

import HSOptions as HSO

-- Given a filename, open the file, zip it, and return the contents
zipFile inFile = do inFileContents <- LBS.readFile inFile
                    return $ GZip.compress inFileContents

-- Zip a string
zipString str = GZip.compress str

logRequest retCode req cli = do
  now <- Clock.getCurrentTime
  putStrLn $ "(" ++ cli ++ ") [" ++ show now ++ "][" ++ retCode ++ "]: " ++ req

contentTypeForFile f = case ext f of
                         "css"  -> "Content-Type: text/css; charset=ISO-8859-1"
                         "htm"  -> "Content-Type: text/html; charset=utf-8"
                         "html" -> "Content-Type: text/html; charset=utf-8"
                         "png"  -> "Content-Type: image/png"
                         "jpg"  -> "Content-Type: image/jpeg;"
                         "js"   -> "Content-Type: application/javascript;"
                         "mp3"  -> "Content-Type: audio/mpeg;"
                         "pdf"  -> "Content-Type: application/pdf;"
                         "gif"  -> "Content-Type: image/gif;"
                         "txt"  -> "Content-Type: text/plain; charset=ISO-8859-1"
                         "zip"  -> "Content-Type: application/zip;"
                         _      -> "Content-Type: text/html; charset=ISO-8859-1"
                       where ext s = reverse $ takeWhile (/='.') $ reverse s

type HttpHeader = [(String, String)]

-- TODO: Not used
-- headersToStr [("Date", "now"), ("Server", "Haskell"), ("Connection", "Close")]
headersToStr :: HttpHeader -> String
headersToStr hh = unlines $ map joinDict hh
                   where joinDict (k, v) = k ++ ": " ++ v

putHeader :: Socket -> String -> String -> Int -> IO ()
putHeader s retCode contentType fileSize = do
  now <- Clock.getCurrentTime
  sendPacked s $ "HTTP/1.1 " ++ retCode ++ "\n"
  sendPacked s $ "Cache-Control: private, max-age=0\n"
  sendPacked s $ "Content-Encoding: gzip\n"
  sendPacked s $ "Date: " ++ show now ++ "\n"
  -- TODO: Expires header - hardcoded at 1 year from now
  sendPacked s $ "Expires: " ++ show (TmFmt.parseTimeOrError True TmFmt.defaultTimeLocale "%Y-%M-%d" (show (Cal.addDays 365 (Clock.utctDay now))) ::Clock.UTCTime) ++ "\n"
  sendPacked s $ contentType ++ "\n"
  sendPacked s $ "Accept-Ranges: bytes" ++ "\n"
  sendPacked s $ "Server: Haspun" ++ "\n"
  sendPacked s $ "Content-Length: " ++ (show fileSize) ++ "\n"
  sendPacked s $ "Connection: Close\n\n"
  where
    sendPacked sock str = sendAll sock (BSC.pack str)

runServer :: HSO.Options -> Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runServer opts mhost port server = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close loop
  where
    resolve = do
      let hints = defaultHints {
          addrFlags = [AI_PASSIVE]
        , addrSocketType = Stream
        }
      head <$> getAddrInfo (Just hints) mhost (Just port)
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      setSocketOption sock ReuseAddr 1
      withFdSocket sock $ setCloseOnExecIfNeeded
      bind sock $ addrAddress addr
      listen sock 1024
      return sock
    loop sock = forever $ do
      (conn, _peer) <- accept sock
      void $ forkFinally (server conn) (const $ gracefulClose conn 5000)


main :: IO ()
main = do
  opts <- setConfig
  putStrLn $ "Listening on port:  " ++ show (listenPort opts)
  runServer opts Nothing (show $ listenPort opts) serve
  where
    serve s = do
      reqStr <- recv s 1024
      unless (S.null reqStr) $ do
        let filePath = (BSC.words reqStr) !! 1
        -- TODO: (docRoot opts)
        let localFilePath = "./test/docroot/" ++ (BSC.unpack (BSC.dropWhile (=='/') filePath))
        fileExists <- doesFileExist localFilePath
        if fileExists then do
          -- TODO: Check for Accept-Encoding: gzip, deflate
          fileContents <- zipFile localFilePath
          let fileSize = LBS.length fileContents
          let contentType = contentTypeForFile (BSC.unpack filePath)
          putHeader s "200 OK" contentType (fromIntegral fileSize)
          sendAll s $ S.pack (LBS.unpack fileContents)
          logRequest "200" (takeWhile (/='\r') (BSC.unpack reqStr)) "client_id"
        else do
          let respHtml = "<!DOCTYPE html><html lang=\"en\"> \
            \<head><title>404</title>\
            \<style type=\"text/css\">* { font-family:Helvetica,sans-serif; }</style>\
            \</head><body>\
            \<h1>404 - File not found.</h1>\
            \<hr>\
            \<p>The file you requested was not found.</p>\
            \</body></html>"
          let zrespHtml = zipString (LBS.fromChunks [(BSC.pack respHtml)])
          putHeader s "404 Not Found" "Content-Type: text/html; charset=ISO-8859-1" (fromIntegral $ LBS.length zrespHtml)
          sendAll s $ S.pack (LBS.unpack zrespHtml)
          logRequest "404" (BSC.unpack reqStr) "client_id"

