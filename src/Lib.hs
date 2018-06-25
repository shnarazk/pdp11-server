{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module Lib
    ( startApp
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Blaze
import qualified Text.Blaze.Html5 as H
import System.ReadEnvVar (readEnvDef)
import PDP11
import Assembler
import Simulator

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type API = "users" :> Get '[JSON] [User]
        :<|> Get '[HTML] H.Html -- the root path: see http://haskell-servant.readthedocs.io/en/stable/tutorial/ApiType.html

startApp :: IO ()
startApp = do
  port <- readEnvDef "PORT" 8080
  putStrLn $ ";;; start server at " ++ show port
  run port app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return users :<|> return homePage

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        , User 3 "Stephen" "Hawking"
        ]

homePage :: H.Html
homePage = H.docTypeHtml $ do
  H.head $ do
    H.title "Live te serve"
    H.body $ do
      H.h1 "Hello!"
      H.p "This is a type-safe web server"
      H.p $ H.toMarkup (repl "MOV R1, R2")

repl :: String -> String
repl str = concatMap toBit (lines str)
  where
    l1 :: String
    l1 = case runPDP11 str of
             Just result -> result
             Nothing -> "wrong code"
    form :: String -> String
    form l = l' ++ replicate (32 - length l') ' '
      where
        l' :: String
        l' = reverse . dropWhile (`elem` (" \t" :: String)) . reverse . dropWhile (`elem` (" \t" :: String)) $ l
    -- printer :: String -> (Int, BitBlock) -> String
    printer l (i, b) = form (if i == 0 then l else "") ++ show b
    toBit :: String -> String
    toBit l = case assemble (l ++ "\n") of
        Right [as] -> concatMap (printer l) (zip [0 ..] (toBitBlocks as))
        Left mes -> show mes

