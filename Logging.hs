module Logging where

import Control.Monad.IO.Class
import Data.Time.Clock
import Data.Time.Format
import System.Directory
import System.IO
import System.Posix.Process
import System.Locale
import System.FilePath as FP

data Logger = Logger Handle
newtype LogDirectory = LogDirectory FilePath



logString :: MonadIO m => Logger -> String -> m ()
logString (Logger handle) msg =
 liftIO $ do
   time <- getCurrentTime
   timeString <- return $ formatTime defaultTimeLocale "[%T%Q] " time
   hPutStr handle timeString
   hPutStrLn handle msg
   hFlush handle


makeLogger :: MonadIO m => LogDirectory -> String -> m Logger
makeLogger (LogDirectory dir) name =
 liftIO $ do
   fullname <- return $ FP.combine dir name
   handle <- openFile fullname WriteMode 
   return $ Logger handle

makeLogDirectory :: MonadIO m => m LogDirectory
makeLogDirectory =
 liftIO $ do
  time <- getCurrentTime
  pid <- getProcessID
  formatString <- return $ "logs_%T." ++ show pid
  name <- return $ formatTime defaultTimeLocale formatString time
  createDirectory name
  return $ LogDirectory name
