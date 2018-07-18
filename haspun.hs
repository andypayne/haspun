import Network.Socket
import System.IO
import Control.Exception
import Control.Concurrent
-- import System.Time
-- import Data.Time.Clock
-- import Data.Time.Clock.POSIX
import Data.Time
import System.Environment
import System.Directory (doesFileExist)
import qualified Data.ByteString.Lazy.Char8 as BS (readFile, pack, unpack, length)
import Data.Int
import Codec.Compression.GZip

import HSOptions


-- Given a filename, open the file, zip it, and return the contents
zipFile inFile = do inFileContents <- BS.readFile inFile
                    return $ compress inFileContents


-- Zip a string
zipString inString = compress inString


logRequest retCode req cli = do
  now <- getCurrentTime
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


thServer :: Options -> Socket -> IO ()
thServer opts sock = do
                -- setSocketOption sock ReuseAddr 1
                bind sock (SockAddrInet (fromIntegral (listenPort opts)) iNADDR_ANY)
                listen sock 2
                -- (listen $ PortNumber (fromIntegral (listenPort opts)))
                -- (sClose)
                (loop sock)
    where loop sock = accept sock >>= handle >> loop sock
          handle (h, n, p) = forkIO (serveThread (docRoot opts) h n >> hClose h)


serveThread documentRoot h n = do
  reqStr <- hGetLine h
  let filePath = (words reqStr) !! 1
  let localFilePath = documentRoot ++ (dropWhile (=='/') filePath)
  fileExists <- doesFileExist localFilePath
  if fileExists then do
    hf <- openFile localFilePath ReadMode
    hClose hf
    -- TODO: Check for Accept-Encoding: gzip, deflate
    fileContents <- zipFile localFilePath
    let fileSize = BS.length fileContents
    let contentType = contentTypeForFile filePath
    putHeader h "200 OK" contentType fileSize
    hPutStr h $ BS.unpack fileContents
    logRequest "200" reqStr n
    hClose h
  else do
    let respHtml = "<!DOCTYPE html><html lang=\"en\"> \
      \<head><title>404</title>\
      \<style type=\"text/css\">font-family:Helvetica,Arial,sans-serif;</style>\
      \</head><body>\
      \<h1>404 - File not found.</h1>\
      \<hr>\
      \<p>The file you requested was not found.</p>\
      \</body></html>"
    let zrespHtml = zipString (BS.pack respHtml)
    putHeader h "404 Not Found" "Content-Type: text/html; charset=ISO-8859-1" (fromIntegral $ BS.length zrespHtml)
    hPutStrLn h (BS.unpack zrespHtml)
    logRequest "404" reqStr n
    hClose h


putHeader :: Handle -> String -> String -> Int64 -> IO ()
putHeader h retCode contentType fileSize = do
  -- now <- getClockTime
  now <- getCurrentTime
  hPutStrLn h $ "HTTP/1.1 " ++ retCode
  hPutStrLn h "Cache-Control: private, max-age=0"
  hPutStrLn h "Content-Encoding: gzip"
  hPutStrLn h $ "Date: " ++ show now
  -- TODO: Expires header - hardcoded at 1 year from now
  -- hPutStrLn h $ "Expires: " ++ show (timeFrom now (60*60*24*365))
  --  parseTimeOrError True defaultTimeLocale "%Y-%M-%d" (show (addDays 365 (utctDay d))) ::UTCTime
  hPutStrLn h $ "Expires: " ++ show (parseTimeOrError True defaultTimeLocale "%Y-%M-%d" (show (addDays 365 (utctDay now))) ::UTCTime)
  hPutStrLn h $ contentType
  hPutStrLn h "Accept-Ranges: bytes"
  hPutStrLn h "Server: Haspun"
  hPutStrLn h $ "Content-Length: " ++ (show fileSize)
  hPutStrLn h "Connection: Close\n"



serveFile h documentRoot filePath = do
  fileExists <- doesFileExist fileToServe
  if fileExists then readFile fileToServe >>= hPutStrLn h else putStrLn "404"
    where fileToServe = documentRoot ++ (dropWhile (=='/') filePath)


main = do
  opts <- setConfig
  sock <- socket AF_INET Stream 0
  putStrLn $ "Document Root:  " ++ (docRoot opts)
  putStrLn $ "Port:           " ++ show (listenPort opts)
  thServer opts sock

