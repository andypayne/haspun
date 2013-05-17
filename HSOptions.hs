module HSOptions where

import System.IO
import System.Console.GetOpt
import System.Exit (exitWith, ExitCode(..))
import System.Environment (getProgName, getArgs)


bannerInfo :: String
bannerInfo = "Haspun -- version 0.0001"


data Options = Options {
  confFile :: String,
  docRoot :: String,
  listenPort  :: Integer
  }


defaultConfig :: Options
defaultConfig = Options {
  confFile = "",
  docRoot = ".",
  listenPort = 8080
  }


options :: [OptDescr (Options -> IO Options)]
options = [
  Option ['V'] ["version"] (NoArg showVersion)         "show version number",
  Option ['h'] ["help"]    (NoArg showHelp)            "show usage",
  Option ['c'] ["config"]  (ReqArg setConfFile "FILE") "set configuration file",
  Option ['d'] ["docroot"] (ReqArg setDocRoot "DIR")   "set doc root",
  Option ['p'] ["port"]    (ReqArg setPort "PORT")     "set port"
  ]


showVersion _ = do
  putStrLn bannerInfo
  exitWith ExitSuccess

setConfFile arg opt = return opt { confFile = arg }
setDocRoot arg opt =  return opt { docRoot = arg }
setPort arg opt =     return opt { listenPort = read arg }

showHelp _ = do
  putStrLn bannerInfo
  prg <- getProgName
  hPutStrLn stderr (usageInfo prg options)
  hFlush stderr
  exitWith ExitSuccess


setConfig = do
  args <- getArgs
  let (actions, nonOpts, msgs) = getOpt RequireOrder options args
  opts <- foldl (>>=) (return defaultConfig) actions
  return opts


