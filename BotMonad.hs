{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module BotMonad (
 BotMonad,
 timeRemaining,
 runBotMonad,
 debugMessage,
) where 

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Random
import Control.Monad.Trans.Reader (runReaderT, ReaderT)
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Ants
import Data.Time
import Control.Applicative
import Control.Monad.ST

import Data.Array
import Data.Array.IArray
import Data.List (isPrefixOf, foldl')
import Data.Char (digitToInt, toUpper)
import Data.Maybe

import Data.Time.Clock
import System.IO
import System.Random

import Util
import Point


newtype BotMonad a = BotMonad { unBotMonad :: (ReaderT GameState (RandT StdGen IO) a) }
  deriving (Monad, Functor, MonadRandom)

instance MonadReader GameState BotMonad where
 ask = BotMonad ask
 local f (BotMonad b) = BotMonad $ local f b
  
timeRemaining :: BotMonad NominalDiffTime
timeRemaining = BotMonad $ do
  ask >>= lift.lift.timeTill.startTime
  where
    timeTill start = getCurrentTime >>= return . (flip diffUTCTime start)


runBotMonad :: BotMonad a -> GameState -> StdGen -> IO a
runBotMonad (BotMonad bot) state rand =
  evalRandT (runReaderT bot state) rand
  
debugMessage :: Show a => a -> BotMonad ()
debugMessage = 
    BotMonad . liftIO . hPutStrLn stderr . show  

