{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Functor
import Data.Text qualified as T
import DotEnv.Micro
import System.Environment
import Telegram.Bot.API
import Telegram.Bot.Simple

newtype Post = Post FileId

data Action
  = SendToAdmins Post Message
  | SendWelcome Message
  | PostToChannel Post Message Message
  | Delete Message Message

data Model
  = Model
  { adminChatId :: ChatId
  -- , channelChatId :: ChatId
  }

suggBot :: IO (BotApp Model Action)
suggBot = do
  adid <- ChatId . read <$> getEnv "ADMIN_CHAT_ID"
  -- chid <- ChatId . read <$> getEnv "CHANNEL_CHAT_ID"
  pure $
    BotApp
      { botInitialModel = Model adid
      , botAction = updateToAction
      , botHandler = handleAction
      , botJobs = []
      }

updateToAction :: Update -> Model -> Maybe Action
updateToAction upd Model{adminChatId} =
  case updateMessage upd of
    Nothing -> Nothing
    Just msg ->
      case (ps', t') of
        (Nothing, Nothing) -> Nothing
        (Just ps, _) ->
          if null ps
            then Nothing
            else Just $ SendToAdmins (Post $ photoSizeFileId $ last ps) msg
        (_, Just t) ->
          if cid == adminChatId
            then case t of
              "+" -> PostToChannel . Post <$> rpFID' <*> rpl' <*> pure msg
              "-" -> Delete <$> rpl' <*> pure msg
              _ -> Nothing
            else
              if t == "/start"
                then Just $ SendWelcome msg
                else Nothing
     where
      ps' = messagePhoto msg
      t' = messageText msg
      rpl' = messageReplyToMessage msg
      rpFID' =
        rpl'
          >>= messagePhoto
          <&> last
          <&> photoSizeFileId
      cid = chatId $ messageChat msg

delete :: Message -> Message -> BotM ()
delete msg1 msg2 = do
  let cid1 = chatId $ messageChat msg1
      cid2 = chatId $ messageChat msg2
      mid1 = messageMessageId msg1
      mid2 = messageMessageId msg2
  void $ runTG (deleteMessage cid1 mid1)
  void $ runTG (deleteMessage cid2 mid2)
handleAction :: Action -> Model -> Eff Action Model
handleAction a m@Model{..} =
  let sid = SomeChatId adminChatId
   in case a of
        SendToAdmins (Post fID) msg ->
          let mid = SomeChatId $ chatId $ messageChat $ msg
           in m <# do
                void $ runTG $ defSendMessage mid "Спасибо, фан-арт принят!"
                void $ runTG $ defSendPhoto sid (PhotoFileId fID)
        PostToChannel (Post fID) msg1 msg2 ->
          m <# do
            -- let sid = SomeChatId channelChatId
            --     req = defSendPhoto sid (PhotoFileId fID)
            -- void $ runTG req
            -- delete msg1 msg2
            pure ()
        Delete msg1 msg2 -> m <# pure ()
        SendWelcome msg ->
          let mid = SomeChatId $ chatId $ messageChat $ msg
           in m <# do
                void $
                  runTG
                    ( defSendMessage
                        mid
                        "Привет, это предложка, присылай сюда свой фан-арт"
                    )

run :: Token -> IO ()
run t = do
  env <- defaultTelegramClientEnv t
  bot <- suggBot
  startBot_ bot env

main :: IO ()
main = do
  loadDotEnv Nothing
  token <- Token . T.pack <$> getEnv "TELEGRAM_BOT_TOKEN"
  run token
