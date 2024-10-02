{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
-- import qualified Data.Text as T
import Discord
import Discord.Types
import qualified Discord.Requests as R
import qualified Telegram.Bot.API as Telegram
import Telegram.Bot.Simple
import Telegram.Bot.API.Types
import Telegram.Bot.API
import qualified Data.Yaml as Y
import Options.Applicative
import Data.Aeson ((.:), FromJSON(..))
import qualified Data.Map as M

-- Config file is a YAML file with the following structure:
-- telegram
--   token
-- discord
--   token
--   defaultChannel
-- mapping
--   - telegramChannel: "channel1"
--     discordChannel: "channel2"
--   - telegramChannel: "channel3"
--     discordChannel: "channel4"

instance Y.FromJSON Config where
  parseJSON = Y.withObject "Config" $ \o -> do
    telegram <- o .: "telegram"
    discord <- o .: "discord"
    mapping <- o .: "mapping"
    configTelegramToken <- telegram .: "token"
    configDiscordToken <- discord .: "token"
    discordDefaultChannel <- discord .: "defaultChannel"
    telegramTodiscordMapping <- Y.parseJSON mapping
    return Config {..}

instance Y.FromJSON ChannelPair where
  parseJSON = Y.withObject "ChannelPair" $ \o -> do
    telegramChannel <- o .: "telegramChannel"
    discordChannel <- o .: "discordChannel"
    return ChannelPair {..}

data ChannelPair = ChannelPair
  { telegramChannel :: Telegram.ChatId
  , discordChannel :: Discord.Types.ChannelId
  } 

data Config = Config
  { configTelegramToken :: Telegram.Token
  , configDiscordToken :: Text
  , discordDefaultChannel :: Text
  , telegramTodiscordMapping :: [ChannelPair]
  }

-- Define the bot's behavior

data Action = NoOp | ForwardMessage Discord.Types.ChannelId Text

instance Ord Telegram.ChatId where
  compare (Telegram.ChatId a) (Telegram.ChatId b) = compare a b

-- Telegram bot application
telegramBot :: Config -> BotApp Config Action
telegramBot config = BotApp
  { botInitialModel = config
  , botAction = handleUpdate
  , botHandler = handleAction
  , botJobs = []
  }

-- Handle incoming Telegram updates
handleUpdate :: Telegram.Update -> Config -> Maybe Action
handleUpdate update config =
  updateMessage update >>= \msg ->  do
      let channelMap = M.fromList $ map (\ChannelPair{..} -> (telegramChannel, discordChannel)) (telegramTodiscordMapping config)
          chatid  = chatId (messageChat msg)
      txt <- messageText msg
      chn <- M.lookup chatid channelMap
      return $ (ForwardMessage chn txt)

-- Handle actions
handleAction :: Action -> Config -> Eff Action Config
handleAction NoOp model = pure model
handleAction (ForwardMessage channel txt) model = model <# do
  liftIO $ sendToDiscord model channel txt
  pure NoOp

-- Function to send message to Discord
sendToDiscord :: Config -> Discord.Types.ChannelId -> Text -> IO ()
sendToDiscord config channel txt = do
--  let msg = def { messageText = txt }
  void $ runDiscord (def { discordToken = configDiscordToken config, discordOnStart = handlers })
  where
    handlers = do
      void $ restCall (R.CreateMessage channel txt)
      stopDiscord

-- The command line options as below
data Options = Options
  { configFile :: FilePath
  }

options :: Parser Options
options = Options
  <$> strOption
    ( long "config"
    <> short 'c'
    <> metavar "CONFIG_FILE"
    <> help "Path to the configuration file"
    )

-- Main function to run the Telegram bot
main :: IO ()
main = do
  Options {..} <- execParser (info options fullDesc)
  config <- Y.decodeFileThrow configFile
  let telegramToken = configTelegramToken config
  
  env <- Telegram.defaultTelegramClientEnv telegramToken
  startBot_ (telegramBot config) env
