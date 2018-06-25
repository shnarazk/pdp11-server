{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module Lib
    ( startApp
    ) where

import Data.Aeson
import Data.Aeson.TH
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html5 ((!))
import System.ReadEnvVar (readEnvDef)
import PDP11
import Assembler
import Simulator
import Web.FormUrlEncoded(FromForm(..), ToForm(..))

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

data Code = Code
 {
   program :: String
 } deriving (Eq, Show, Generic)

instance FromForm Code

type API = "users" :> Get '[JSON] [User]
           :<|> "run" :> ReqBody '[FormUrlEncoded] Code :> Post '[HTML] H.Html
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
server = return users :<|> (\d ->  return (resultPage d)) :<|> return homePage

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        , User 3 "Stephen" "Hawking"
        ]

homePage :: H.Html
homePage = H.docTypeHtml $ do
  H.head $ do
    H.title "PDP11 simulator"
    H.body $ do
      H.h1 "Hello!"
      H.p "Powered by Servant, a type-safe web server written in Haskell."
      H.p "TYPE YOUR ASSEMBLY CODE"
      H.p $
        H.form ! A.method "POST" ! A.action "run" $ do
        H.p $ H.textarea ! A.name "program" ! A.cols "50" ! A.rows "10" $ "MOV R1, R2"
        H.p $ H.button ! A.type_ "submit" ! A.name "action" ! A.value "send" $ "RUN"
      H.pre ! A.style "background: #ccf;" $ H.toMarkup (repl "MOV R1, R2\n")

resultPage :: Code -> H.Html
resultPage (Code str) = H.docTypeHtml $ do
  H.head $ do
    H.title "PDP11 simulator"
    H.body $ do
      H.h1 "Hello!"
      H.p "Powered by Servant, a type-safe web server written in Haskell."
      H.p "YOUR ASSEMBLY CODE"
      H.p $
        H.form ! A.method "POST" ! A.action "run" $ do
        H.p $ H.textarea ! A.name "program" ! A.cols "50" ! A.rows "10" $ H.toMarkup str
        H.p $ H.button ! A.type_ "submit" ! A.name "action" ! A.value "send" $ "RERUN"
      H.h1 $ "Rusult"
      H.pre ! A.style "background: #ccf;" $ H.toMarkup (repl (str ++ "\n"))

repl :: String -> String
repl str = l1 ++ "\n" ++ concatMap toBit (lines str)
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
