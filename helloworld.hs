{-# LANGUAGE TypeFamilies, QuasiQuotes, GeneralizedNewtypeDeriving, FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell, OverloadedStrings, GADTs, MultiParamTypeClasses #-}

import Yesod
import Data.Text (Text)
import Data.DateTime
import Database.Persist.Sqlite

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Person
  firstName String
  lastName String
  age Int Gt Desc
  deriving Show
Action
  name Text
  event Text
  time DateTime
  uid Int
  server Text
  grade Int
  deriving Show
|]

data HelloWorld = HelloWorld ConnectionPool

mkYesod "HelloWorld" [parseRoutes|
                      / HomeR GET
                      /person/#Text PersonR GET
                      /person_persist/#PersonId PersonPersistR GET
                      /action/#Text/#Text/#Integer/#Int/#Text/#Int ActionR GET
                      /all_action AllActionR GET
                      |]

instance Yesod HelloWorld

instance YesodPersist HelloWorld where
  type YesodPersistBackend HelloWorld = SqlPersist

  runDB action = do
    HelloWorld pool <- getYesod
    runSqlPool action pool

getHomeR :: Handler RepHtml
getHomeR = defaultLayout [whamlet|Hello World!|]

getPersonR :: Text -> Handler RepHtml
getPersonR name = defaultLayout [whamlet|<h1>Hello #{name}!|]

getPersonPersistR :: PersonId -> Handler RepPlain
getPersonPersistR personId = do
  person <- runDB $ get404 personId
  return $ RepPlain $ toContent $ show person

getActionR :: Text -> Text -> Integer -> Int -> Text -> Int -> Handler RepPlain
getActionR uri action time uid server grade = do
  actionId <- runDB $ insert (Action uri action (fromSeconds time) uid server grade)
  return $ RepPlain $ toContent $ show actionId

getAllActionR :: Handler RepPlain
getAllActionR = do
  actions <- runDB $ selectList [] [Desc ActionTime]
  return $ RepPlain $ toContent $ show actions

openConnectionCount :: Int
openConnectionCount = 10

main :: IO()
main = withSqlitePool "test.db3" openConnectionCount $ \pool -> do
  runSqlPool (runMigration migrateAll) pool
  runSqlPool (insert $ Person "Michael" "Snoyman" 26) pool
  warpDebug 3000 (HelloWorld pool)

