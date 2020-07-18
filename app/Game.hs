{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Game where

import Control.Lens
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import System.Environment

import Data

type PlayerKey = String
type ServerUrl = String

data GameStage = Unstarted | InProgress | Finished
  deriving (Enum, Show)
makePrisms ''GameStage

data GameState = GameState
  {
  }

data GameResponse = GameResponse
  { _unknown1 :: ()
  , _gameStage :: GameStage
  , _unknownl :: ()
  , _gameState :: GameState
  }
makeLenses ''GameResponse

data Request 
  = JoinRequest
  | StartRequest
  | CommandRequest

mkJoinRequest :: PlayerKey -> Request
mkJoinRequest = undefined
mkStartRequest :: PlayerKey -> GameResponse -> Request
mkStartRequest = undefined
mkCommandRequest :: PlayerKey -> GameResponse -> Request
mkCommandRequest = undefined

send :: MonadIO m => ServerUrl -> Request -> m GameResponse
send = undefined

parseResponse :: Value -> GameResponse
parseResponse (VCons (VInt 1) (VCons (VInt stage) (VCons unknownl state)))
  = GameResponse () (toEnum . fromIntegral $ stage) () (parseState state)

parseState :: Value -> GameState
parseState = undefined

main :: IO ()
main = do
  [serverUrl, playerKey] <- getArgs

  let joinRequest = mkJoinRequest playerKey

  gameResponse <- send serverUrl joinRequest

  let startRequest = mkStartRequest playerKey gameResponse

  gameResponse <- send serverUrl startRequest

  flip evalStateT gameResponse $ whileM do
    commandRequest <- mkCommandRequest playerKey <$> get
    put =<< send serverUrl commandRequest
    gameStage `uses` isn't _Finished
