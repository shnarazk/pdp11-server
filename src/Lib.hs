{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module Lib
    ( startApp
    ) where

import Control.Monad
import Control.Monad.IO.Class
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
import System.Random
import System.IO.Unsafe
import System.ReadEnvVar (readEnvDef)
import qualified PDP11 as PDP
import qualified Assembler as PA
import qualified Simulator as PS
import Web.FormUrlEncoded(FromForm(..), ToForm(..))

version :: String
version = "0.5.0.0"

data Code = Code
 {
   program   :: String
 , randomize :: String
 } deriving (Eq, Show, Generic)

instance FromForm Code

type API = "run" :> ReqBody '[FormUrlEncoded] Code :> Post '[HTML] H.Html
           :<|> Get '[HTML] H.Html

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
server = (\d ->  return (resultPage d)) :<|> return homePage

homePage :: H.Html
homePage = H.docTypeHtml $ do
  H.head $ do
    H.title . H.toHtml $ "PDP11 simulator (version " ++ version ++ ")"
    H.body $ do
      H.h1 "Welcome to a PDP11 simulator"
      H.p "Fill you code in"
      H.p $
        H.form ! A.method "POST" ! A.action "run" $ do
        H.p $ H.textarea ! A.name "program" ! A.cols "40" ! A.rows "10" $ "MOV R1, R2"
        H.p $ do
          H.button ! A.type_ "submit" ! A.name "action" ! A.value "send" $ "RUN"
          H.input ! A.type_ "hidden" ! A.name "randomize" ! A.value "RandomizeOff"
      H.hr
      H.p ! A.style "text-align: right" $ "powered by Servant, a type-safe web server written in Haskell."
      H.p ! A.style "text-align: right;" $ H.toMarkup ("version " ++ version ++ " by nrzk, nagasaki-u.")

resultPage :: Code -> H.Html
resultPage (Code str rnd) = H.docTypeHtml $ do
  H.head $ do
    H.style ! A.type_ "text/css" $ "<!-- \n\
\table,tr,td,th {border:1px black solid;border-collapse:collapse;font-family:monospace;}\n\
\th {text-align:center;}\n\
\td {text-align:right;font-family:monospace;width:26px;padding:2px;}\n\
\.opcode {text-align:left;width:90px;padding-left:4px;}\n\
\pre code {font-family:monospace;}\n\
\-->"
    H.title "A PDP11 simulator"
    H.body $ do
      H.h1 "Assembly Code"
      H.p $ do
        H.form ! A.method "POST" ! A.action "run" $ do
          H.p $ H.textarea ! A.name "program" ! A.cols "40" ! A.rows "10" $ H.toMarkup str
          H.p $ do
            H.select ! A.id "randomize" ! A.name "randomize" $ do
              H.optgroup ! A.label "Memory Randomization" $ do
                H.option "randomize on"
                H.option ! A.selected "randomize off" $ "randomize off"
            H.button ! A.type_ "submit" ! A.name "action" ! A.value "send" ! A.style "margin-left: 120px;" $ "UPDATE"
      let prg = shaping str
      if null prg
        then do
          H.p "You sent an empty program."
          H.a ! A.href "/" $ "RESET"
        else do
          case asBits prg of
            Left str -> return ()
            Right l -> do
              H.h1 $ H.toMarkup $ "Binary Representation (ver. " ++ PDP.version ++ ")"
              H.pre ! A.style "width:300px;background:#f8f8f8;border:1px solid #777;margin:8px;padding:8px;" $ H.code $ H.toMarkup l
          H.h1 $ H.toMarkup $ "Execution Trace (ver. " ++ PS.version ++ ")"
          let mac = unsafePerformIO $ makeMachine (rnd == "randomize on")
          case repl mac prg of
            Left str -> H.pre ! A.style "background: #fee;" $ H.toMarkup str
            Right lst -> do
              H.table $ do
                H.tr $ do
                  H.th ! A.rowspan "2" $ "PC"
                  H.th ! A.rowspan "2" $ "Opcode"
                  H.th ! A.colspan "12" ! A.style "background:#efe;" $ "Memory 11 - 0"
                  H.th ! A.colspan "8"  ! A.style "background:#eef;" $ "Register 7 - 0"
                  H.th ! A.colspan "4"  ! A.style "background:#fee;" $ "PSW"
                H.tr $ do
                  mapM_ (\m -> H.th ! A.style "background:#efe;" $ (H.toMarkup (show m))) (reverse [0 .. 11])
                  mapM_ (\r -> H.th ! A.style "background:#eef;" $ (H.toMarkup ("R" ++ show r))) (reverse [0 .. 7])
                  mapM_ (\r -> H.th ! A.style "background:#fee;" $ r) ["N", "Z", "V", "C"]
                mapM_ (\(ms, rs, psw, addr, asm) ->
                         H.tr ! A.style "border: 1pt;" $ do
                          H.td ! A.class_ "PC" $ H.toMarkup (show addr) 
                          H.td ! A.class_ "opcode" $ H.toMarkup (show asm) 
                          mapM_ (\m -> H.td ! A.style "background:#efe;" $ (H.toMarkup (show m))) (reverse (take 12 ms))
                          mapM_ (\r -> H.td ! A.style "background:#eef;" $ (H.toMarkup (show r))) (reverse (take 8 rs))
                          mapM_ (\f -> H.td ! A.style "background:#fee;" $ (H.toMarkup (show f))) psw
                      ) lst
      H.p ! A.style "text-align: right;" $ H.toMarkup ("version " ++ version ++ " by nrzk, nagasaki-u.")

shaping :: String -> String
shaping str = unlines . filter (not . null) . map trim . lines $ str
  where trim = reverse . dropWhile (`elem` [' ', '\t', '\r']) . reverse . dropWhile (`elem` [' ', '\t'])

repl :: PDP.Machine -> String -> Either String [([Int], [Int], [Int], Int, PDP.ASM)]
repl machine str = map PDP.dump <$> PS.runSimulator 64 machine <$> PA.assemble str

asBits :: String -> Either String String
asBits str = (unlines . map show . concatMap PDP.toBitBlocks) <$> PA.assemble str

makeMachine :: Bool -> IO PDP.Machine
makeMachine False = return PDP.initialMachine
makeMachine True = do
  i <- getStdRandom (randomR (0, 111))
  j <- getStdRandom (randomR (0, 111))
  k <- getStdRandom (randomR (0, 111))
  return $ PDP.makePDP11 [0,0,mod i 256, div i 256, mod j 256, div j 256, mod k 256, div k 256, 1, 1] [0,0,0,0,0,0,0,100]
