{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module BotMonad (
 BotMonad,
 timeRemaining,
 runBotMonad,
 debugMessage,
) where 

import Ants
import Control.Monad.IO.Class
import Control.Monad.Random
import Control.Monad.Reader.Class
import Control.Monad.Trans.Reader (runReaderT, ReaderT)
import Control.Monad.Trans.Class


import Data.Time.Clock

import System.IO

newtype BotMonad a = BotMonad (ReaderT GameState (RandT StdGen IO) a)
  deriving (Monad, Functor, MonadRandom, MonadIO)

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

