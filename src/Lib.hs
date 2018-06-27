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
import qualified PDP11 as PDP
import qualified Assembler as PA
import qualified Simulator as PS
import Web.FormUrlEncoded(FromForm(..), ToForm(..))

version :: String
version = "0.1.2.0"

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
    H.title . H.toHtml $ "PDP11 simulator (version " ++ version ++ ")"
    H.body $ do
      H.h1 "Hello!"
      H.p "Powered by Servant, a type-safe web server written in Haskell."
      H.p "TYPE YOUR ASSEMBLY CODE"
      H.p $
        H.form ! A.method "POST" ! A.action "run" $ do
        H.p $ H.textarea ! A.name "program" ! A.cols "40" ! A.rows "10" $ "MOV R1, R2"
        H.p $ H.button ! A.type_ "submit" ! A.name "action" ! A.value "send" $ "RUN"
      H.h1 $ "Rusult and Disassembled Code"
      H.pre ! A.style "background: #eef;" $ H.code ! A.style "font-family: Courier, monospace;" $ H.toMarkup (repl_ (shaping "MOV R1, R2\n"))

resultPage :: Code -> H.Html
resultPage (Code str) = H.docTypeHtml $ do
  H.head $ do
    H.style ! A.type_ "text/css" $ "<!-- \n\
\table,tr,td,th {border:1px black solid;border-collapse:collapse;font-family:monospace;}\n\
\th {text-align:center;}\n\
\td {text-align:right;font-family:monospace;width:32px;padding:2px;}\n\
\.opcode {text-align:left;width:100px;padding-left:4px;}\n\
\-->"
    H.title . H.toHtml $ "PDP11 simulator (version " ++ version ++ ")"
    H.body $ do
      H.h1 "Your Assembly Code"
      H.p $
        H.form ! A.method "POST" ! A.action "run" $ do
        H.p $ H.textarea ! A.name "program" ! A.cols "40" ! A.rows "10" $ H.toMarkup str
        H.p $ H.button ! A.type_ "submit" ! A.name "action" ! A.value "send" $ "RERUN"
      H.h1 $ "Rusult"
--      H.pre ! A.style "background: #eef;" $ H.code ! A.style "font-family: Courier, monospace;" $ H.toMarkup (repl (str ++ "\n"))
      case repl (shaping str) of
        Left str -> H.pre ! A.style "background: #fee;" $ H.toMarkup str
        Right lst -> do
          H.table $ do
            H.tr $ do
              H.th ! A.rowspan "2" $ "Opcode"
              H.th ! A.colspan "12" ! A.style "background:#efe;" $ "Memory 11 - 0"
              H.th ! A.colspan "8"  ! A.style "background:#eef;" $ "Register 7 - 0"
            H.tr $ do
              mapM_ (\m -> H.th ! A.style "background:#efe;" $ (H.toMarkup (show m))) (reverse [0 .. 11])
              mapM_ (\r -> H.th ! A.style "background:#eef;" $ (H.toMarkup ("R" ++ show r))) (reverse [0 .. 7])
            mapM_ (\(ins, (ms, rs)) ->
                     H.tr ! A.style "border: 1pt;" $ do
                      H.td ! A.class_ "opcode" $ H.toMarkup ins
                      mapM_ (\m -> H.td ! A.style "background:#efe;" $ (H.toMarkup (show m))) (reverse ms)
                      mapM_ (\r -> H.td ! A.style "background:#eef;" $ (H.toMarkup (show r))) (reverse rs)
                  ) lst
      case asBits (shaping str) of
        Left str -> return ()
        Right l -> do
          H.h1 "Binary Codes"
          H.pre . H.code $ H.toMarkup l

repl_ :: String -> String
repl_ str = l1 ++ "\n" ++ concatMap toBit (lines str)
  where
    l1 :: String
    l1 = case PS.runPDP11 str of
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
    toBit l = case PA.assemble (l ++ "\n") of
        Right [as] -> unlines $ map (printer l) (zip [0 ..] (PDP.toBitBlocks as))
        Left mes -> show mes

shaping :: String -> String
shaping str = unlines . map (filter ('\r' /=)) . filter (not . null) . lines $ str

repl :: String -> Either String [(String, ([Int], [Int]))]
repl str = zipWith (\ins m -> (ins, PDP.dump m)) ("-----" : lines str) <$> PS.runSimulator' <$> PA.assemble str

asBits :: String -> Either String String
asBits str = (unlines . map (show . PDP.toBitBlocks)) <$> PA.assemble str
